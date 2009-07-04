%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-06-03

%% @doc Module supervisor.  Starts/restarts module processes.
%% @todo Take module dependencies into account when starting/restarting modules.

-module(zp_module_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, start_link/1, upgrade/1]).

%% supervisor callbacks
-export([init/1, deactivate/2, activate/2, active/1, active_dir/1, all/1, scan/1, prio/1, prio_sort/1, module_exists/1]).

-include_lib("zophrenic.hrl").

%% The default module priority
-define(MOD_PRIO, 500).


%% @spec start_link(Args) -> ServerRet
%% @doc API for starting the module supervisor.
start_link() ->
    start_link([]).
start_link(Args) ->
    Context = zp_context:new(),
    Args1 = [{context, Context} | Args],
    Result = supervisor:start_link({local, ?MODULE}, ?MODULE, Args1),
    zp_notifier:notify({module_ready}, Context),
    Result.


%% @spec upgrade(context()) -> ok
%% @doc Reload the list of all modules, add processes if necessary.
upgrade(Context) ->
    {ok, {_, Specs}} = init([{context, Context}]),

    Old  = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New  = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [ start_child(Spec) || Spec <- Specs ],

    sets:fold(fun(Id, ok) -> 
                zp_notifier:notify({module_activate, Id}, Context), 
                ok
            end, ok, New),
    sets:fold(fun(Id, ok) -> 
                zp_notifier:notify({module_deactivate, Id}, Context), 
                ok 
            end, ok, Kill),
    zp_notifier:notify({module_ready}, Context),
    ok.


    %% @doc Try to start the child, do not crash on missing modules.
    start_child({Name, _, _, _, _, _} = Spec) ->
        Info =  try
                    erlang:get_module_info(Name, attributes)
                catch
                    M:E -> 
                        ?ERROR("Can not fetch module info for module ~p, error: ~p:~p", [Name, M, E]),
                        error
                end,
        case Info of
            L when is_list(L) ->
                supervisor:start_child(?MODULE, Spec);
            error ->
                error
        end.
        

%% @spec init(proplist()) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    init([{context, zp_context:new()}]);
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    Ms = lists:filter(fun module_exists/1, active(Context)),
    Processes = [
        {M, 
            {M, start_link, [[{context, Context}]]},
            permanent, 5000, worker, [M]} || M <- Ms
    ],
    {ok, {{one_for_one, 1000, 10}, Processes}}.


%% @doc Deactivate a module. The module is marked as deactivated and stopped when it was running.
%% @spec deactivate(Module, context()) -> ok
deactivate(Module, Context) ->
    case zp_db:q("update module set is_active = false, modified = now() where name = $1", [Module], Context) of
        1 -> upgrade(Context);
        0 -> ok
    end.


%% @doc Activate a module. The module is marked as active and started as a child of the module supervisor.
%% @spec deactivate(Module, context()) -> ok
activate(Module, Context) ->
    Scanned = scan(Context),
    {Module, _Dirname} = proplists:lookup(Module, Scanned),
    F = fun(Ctx) ->
        case zp_db:q("update module set is_active = true, modified = now() where name = $1", [Module], Ctx) of
            0 -> zp_db:q("insert into module (name, is_active) values ($1, true)", [Module], Ctx);
            1 -> 1
        end
    end,
    zp_db:transaction(F, Context),
    upgrade(Context).
    
    

%% @doc Return the list of active modules.
%% @spec active(context()) -> [ atom() ]
active(Context) ->
    Modules = zp_db:q("select name from module where is_active = true order by name", Context),
    [ zp_convert:to_atom(M) || {M} <- Modules ].


%% @doc Return the list of all active modules and their directories
%% @spec active_dir(context()) -> [ {atom, Dir}, ... ]
active_dir(Context) ->
    Active = active(Context),
    All    = scan(Context),
    [ {M, proplists:get_value(M, All)} || M <- Active ].


%% @doc Return the list of all modules in the database.
%% @spec active(context()) -> [ atom() ]
all(Context) ->
   Modules = zp_db:q("select name from module order by name", Context),
   [ zp_convert:to_atom(M) || {M} <- Modules ].


%% @doc Scan for a list of modules present in the site's module directories. A module is always a directory,
%% the name of the directory is the same as the name of the module.
%% @spec scan(context()) -> [ {atom(), dirname()} ]
scan(#context{host=Host}) ->
    Priv  = filename:join([code:lib_dir(zophrenic, priv), "sites", Host, "modules", "mod_*"]),
    Src   = filename:join([code:lib_dir(zophrenic, modules), "mod_*"]),
    Files = filelib:wildcard(Priv) ++ filelib:wildcard(Src),
    [ {zp_convert:to_atom(filename:basename(F)), F} ||  F <- Files ].


%% @doc Return the priority of a module. Default priority is 500, lower is higher priority.  Never crash on a missing module.
%% @spec prio(Module) -> integer()
prio(Module) ->
    try
        Info = erlang:get_module_info(Module, attributes),
        case proplists:get_value(mod_prio, Info) of
            [Prio] -> Prio;
            _ -> ?MOD_PRIO
        end
    catch
        _M:_E -> ?MOD_PRIO
    end.


%% @doc Sort the results of a scan on module priority first, module name next. The list is made up of {module, Values} tuples
%% @spec prio_sort(proplist()) -> proplist()
prio_sort(ModuleProps) ->
    WithPrio = [ {zp_module_sup:prio(M), {M, X}} || {M, X} <- ModuleProps ],
    Sorted = lists:sort(WithPrio),
    [ X || {_Prio, X} <- Sorted ].


module_exists(M) ->
    case code:ensure_loaded(M) of
        {module,M} -> true;
        {error, _} -> false
    end.
    