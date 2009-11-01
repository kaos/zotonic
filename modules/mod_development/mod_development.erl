%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-01
%%
%% @doc Development server.  Periodically performs a "make" and loads new files.
%% When new files are loaded the caches are emptied.

-module(mod_development).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Development").
-mod_description("Development support, periodically builds and loads changed files.").
-mod_prio(1000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
	reload/1,
	make/1
]).

-include_lib("zotonic.hrl").

-record(state, {context}).

% Interval for checking for new and/or changed files.
-define(DEV_POLL_INTERVAL, 10000).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link(Args) -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).


reload(Context) ->
	z_event:notify(development_reload, Context).

make(Context) ->
	z_event:notify(development_make, Context).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    timer:send_interval(?DEV_POLL_INTERVAL, poll),
	z_notifier:observe(development_reload, self(), Context),
	z_notifier:observe(development_make, self(), Context),
    {ok, #state{
        context  = z_context:new(Context)
    }}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
handle_cast(reload, State) ->
	reload_all(State#state.context),
    {noreply, State};

handle_cast(make, State) ->
	make_all(State#state.context),
	reload_all(State#state.context),
    {noreply, State};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Periodic check for changed beam files.
handle_info(poll, State) ->
	reload_all(State#state.context),
    {noreply, State};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
	z_notifier:detach(development_reload, self(), State#state.context),
	z_notifier:detach(development_make, self(), State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Check if any of the loaded modules has been changed. If so reload the module's beam file.
reload_all(_Context) ->
	case reload_all() of
		[] -> ok;
		_ -> z:flush()
	end.

%% @doc Remake beam files from source. Do not load the new files (yet).
make_all(_Context) ->
	make:all([]),
	ok.

%% @doc Reload a module, purge the old code.
reload_module(M) ->
	code:purge(M),
	code:soft_purge(M),
	{module, M} = code:load_file(M),
	{ok, M}.

%% @doc Reload all modules from the zotonic directory or subdirectories.  Return a list of modules reloaded. Empty list when nothing changed.
%% @spec reload_all() -> [Result]
reload_all() ->
	Dir = code:lib_dir(zotonic),
	Modules = [{M,P} || {M, P} <- code:all_loaded(), is_list(P) andalso string:str(P, Dir) > 0],
	[reload_module(M) || {M,Path} <- Modules, module_changed(M,Path)].
	
%% @doc Check if the version number of the module has been changed.  Skip template modules.
%% @spec module_changed(atom(), filename()) -> bool()
module_changed(Module, BeamFile) ->
	case z_template:is_template_module(Module) of
		true -> 
			false;
		false ->
			Props = Module:module_info(attributes),
			case proplists:get_value(vsn, Props) of
				undefined ->
					false;
				Version ->
					case beam_lib:version(BeamFile) of
						{ok, {_Module, Version}} -> false;
						{ok, {_Module, _OtherVersion}} -> true;
						{error, _} -> false
					end
			end
	end.

	