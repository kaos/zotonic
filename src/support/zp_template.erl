%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Template handling, compiles and renders django compatible templates using an extended version of erlydtl
%% @todo Make the template handling dependent on the host of the context (hosts have different modules enabled).

-module(zp_template).
-behaviour(gen_server).

-author("Marc Worrell <marc@worrell.nl>").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

-include_lib("zophrenic.hrl").

%% External exports
-export([compile/2, render/3, find_template/2, find_template/3, reset/1]).

-record(state, {reset_counter}).

start_link() -> 
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc Force a reset of all templates, used after a module has been activated or deactivated.
reset(_Context) ->
    gen_server:cast(?MODULE, reset).


%% @spec render(File, Variables, Context) -> iolist()
%% @doc Render a template.  First requests the template module from the template server, then renders the template.
render(File, Variables, Context) ->
    case find_template(File, Context) of
        {ok, FoundFile} ->
            Result = case gen_server:call(?MODULE, {check_modified, FoundFile}) of
                modified -> compile(File, Context);
                Other -> Other
            end,

            case Result of
                {ok, Module} ->
                    case Module:render(Variables, Context) of
                        {ok, Output}   -> 
                            Output;
                        {error, Reason} ->
                            ?ERROR("Error rendering template: ~p (~p)", [File, Reason]),
                            <<>>
                     end;
                {error, Reason} ->
                    ?ERROR("Error compiling template: ~s (~p)", [File, Reason]),
                    <<>>
            end;
        {error, Reason} ->
            ?LOG("Could not find template: ~s (~p)", [File, Reason]),
            <<>>
    end.
            


%% @spec compile(File, Context) -> {ok, atom()} | {error, Reason}
%% @doc Compile a template, retun the module name.
compile(File, Context) ->
    case find_template(File, Context) of
        {ok, FoundFile} ->
            gen_server:call(?MODULE, {compile, FoundFile, Context});
        {error, Reason} ->
            {error, Reason}
    end.


%% @spec find_template(File, Context) -> {ok, filename()} | {error, code} 
%% @doc Finds the template designated by the file, check modules.
find_template(File, Context) ->
    zp_module_indexer:find(template, File, Context).
    

%% @spec find_template(File, All, Context) -> FilenameList
%% @doc Finds the first or all templates designated by the file, check modules.
find_template(File, false, Context) ->
    case zp_module_indexer:find(template, File, Context) of
        {ok, TemplateFile} -> [TemplateFile];
        {error, _Reason} -> []
    end;
find_template(File, true, Context) ->
    zp_module_indexer:find_all(template, File, Context).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initialize the template server, handles template compiles and rendering.
init(_Options) ->
    {ok, #state{reset_counter=0}}.


%% @spec check_modified(File) -> {ok, Module} | {error, Reason}
%% @doc Compile the template if its has been modified, return the template module for rendering.
handle_call({check_modified, File}, _From, State) ->
    ModuleName = filename_to_modulename(File),
    Module = list_to_atom(ModuleName),
    Result = case template_is_modified(Module, State#state.reset_counter) of
                true  -> modified;
                false -> {ok, Module}
             end,
    {reply, Result, State};


%% @doc Compile the template, loads the compiled template in memory.  Make sure that we only compile 
%% one template at a time to prevent overloading the server on restarts.
handle_call({compile, File, Context}, _From, State) ->
    FinderFun  = fun(FinderFile, All) ->
        ?MODULE:find_template(FinderFile, All, Context)
    end,
    ModuleName = filename_to_modulename(File),
    Module     = list_to_atom(ModuleName),
    ErlyResult = case erlydtl:compile(File, Module, [{finder, FinderFun}, {template_reset_counter, State#state.reset_counter}]) of
                    {ok, Module1} -> {ok, Module1};
                    Error -> Error
                 end,
    {reply, ErlyResult, State}.

%% @doc Reset all compiled templates, done by the module_indexer after the module list changed.
handle_cast(reset, State) -> 
    {noreply, State#state{reset_counter=State#state.reset_counter+1}};
handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.


%% @doc Translate a filename to a module name
filename_to_modulename(File) ->
    filename_to_modulename(File, []).

filename_to_modulename([], Acc) ->
    "template_" ++ lists:reverse(Acc);
filename_to_modulename([C|T], Acc) ->
    filename_to_modulename(T, [savechar(C)|Acc]).

savechar(C) when C >= $0 andalso C =< $9 ->
    C;
savechar(C) when C >= $a andalso C =< $z ->
    C;
savechar(C) when C >= $A andalso C =< $Z ->
    C+32;
savechar(_C) ->
    $_.


%% Check if the template or one of the by the template included files is modified since compilation
%% or if the template has been compiled before a reset of all compiled templates.
template_is_modified(Module, ResetCounter) ->
    case catch Module:template_reset_counter() < ResetCounter of
        true ->
            true;
        false ->
            Deps = Module:dependencies(),
            is_modified(Deps);
        _Error -> 
            true
    end.

%% Check if one of the template files is newer than the given datetime
is_modified([]) ->
    false;
is_modified([{File, DateTime}|Rest]) ->
    case filelib:last_modified(File) of
        0 -> 
            true;
        FileMod when FileMod =/= DateTime ->
            true;
        _ ->
            is_modified(Rest)
    end.
