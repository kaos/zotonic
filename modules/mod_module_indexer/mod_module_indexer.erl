%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-06-06
%%
%% @doc Implements the module extension mechanisms for scomps, templates, actions etc.  Scans all active modules
%% for scomps (etc) and maintains lookup lists for when the system tries to find a scomp (etc).

-module(mod_module_indexer).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_server).

-mod_title("Module Indexer").
-mod_description("Implements the module extension mechanisms for scomps, templates, actions etc.").
-mod_prio(1000).


%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    reindex/1,
    find/3,
    find_all/3
]).

-record(state, {scomps=[], actions=[], validators=[], models=[], templates=[]}).

-include("zophrenic.hrl").

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

    
%% @doc Reindex the list of all scomps, etc for the site in the context.
reindex(Context) ->
    gen_server:cast(?MODULE, {{module_ready}, Context}).


%% @doc Find a scomp, validator etc.
%% @spec find(What, Name, Context) -> {ok, term()} | {error, Reason}
find(What, Name, _Context) ->
    gen_server:call(?MODULE, {find, What, Name}).

%% @doc Find a scomp, validator etc.
%% @spec find_all(What, Name, Context) -> list()
find_all(What, Name, _Context) ->
    gen_server:call(?MODULE, {find_all, What, Name}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    zp_notifier:observe(module_ready, self(), Context),
    {ok, #state{}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Find a template definition
handle_call({find, scomp, Name}, _From, State) ->
    {reply, lookup(Name, State#state.scomps), State};
handle_call({find, action, Name}, _From, State) ->
    {reply, lookup(Name, State#state.actions), State};
handle_call({find, validator, Name}, _From, State) ->
    {reply, lookup(Name, State#state.validators), State};
handle_call({find, model, Name}, _From, State) ->
    {reply, lookup(Name, State#state.models), State};
handle_call({find, template, Name}, _From, State) ->
    {reply, lookup(Name, State#state.templates), State};

handle_call({find_all, scomp, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.scomps), State};
handle_call({find_all, action, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.actions), State};
handle_call({find_all, validator, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.validators), State};
handle_call({find_all, model, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.models), State};
handle_call({find_all, template, Name}, _From, State) ->
    {reply, lookup_all(Name, State#state.templates), State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Scan for all scomps etc. for the context given.
handle_cast({{module_ready}, Context}, State) ->
    Scanned = scan(Context),
    State1 = State#state{
        scomps     = proplists:get_value(scomp, Scanned),
        actions    = proplists:get_value(action, Scanned),
        validators = proplists:get_value(validator, Scanned),
        models     = proplists:get_value(model, Scanned),
        templates  = proplists:get_value(template, Scanned)
    },
    {noreply, State1};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    Context = zp_context:new(),
    zp_notifier:detach(module_ready, self(), Context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Find a scomp etc in a lookup list
lookup(Name, List) ->
    case proplists:get_value(Name, List) of
        undefined ->
            {error, enoent};
        Result ->
            {ok, Result}
    end.

%% @doc Find all scomps etc in a lookup list
lookup_all(Name, List) ->
    proplists:get_all_values(Name, List).


%% @doc Scan the module directories for scomps, actions etc.
scan(Context) ->
    [ {What, scan1(What, Context)} || What <- [ scomp, action, validator, model, template ] ].


%% @doc Scan module directories for specific kinds of parts. Returns a lookup list [ {itemname, rootname, File} ]
scan1(What, Context) ->
    {Subdir, Prefix, Extension} = subdir(What),
    Scan = scan_subdir(Subdir, Prefix, Extension, Context), 
    Sorted = zp_module_sup:prio_sort(Scan),
    FlattenFun = fun({_Module, {_ModuleDir, Files}}, Acc) ->
        Files1 = [ file2index(What, F) || F <- Files ],
        Files1 ++ Acc
    end,
    lists:foldr(FlattenFun, [], Sorted).


    subdir(scomp)     -> { "scomps",     "scomp_",     ".erl" };
    subdir(action)    -> { "actions",    "action_",    ".erl" };
    subdir(validator) -> { "validators", "validator_", ".erl" };
    subdir(model)     -> { "models",     "m_",         ".erl" };
    subdir(template)  -> { "templates",  "",           ""     }.

    file2index(template, {Basename, File}) ->
        {Basename, File};
    file2index(_, {NoPrefixExt, File}) ->
        ModuleName = list_to_atom(filename:basename(File, ".erl")),
        {list_to_atom(NoPrefixExt), ModuleName}.


%% @doc Scan all module directories for templates/scomps/etc.  Example: scan("scomps", "scomp_", ".erl", Context)
%% @spec scan_subdir(Subdir, Prefix, Extension, context()) -> [ {ModuleAtom, {ModuleDir, [{Name, File}]}} ]
scan_subdir(Subdir, Prefix, Extension, Context) ->
    Modules = zp_module_sup:active_dir(Context),
    Scan1 = fun({Module, Dir}, Acc) ->
        Prefix1 = Prefix ++ module2prefix(Module) ++ "_",
        Pattern = filename:join([Dir, Subdir, Prefix1 ++ "*" ++ Extension]),
        Files   = filelib:wildcard(Pattern),
        case Files of
            [] -> Acc;
            _  -> 
                PrefixLen = length(Prefix1),
                Files1    = [ {scan_remove_prefix_ext(F, PrefixLen, Extension), F} || F <- Files ],
                [{Module, {Dir, Files1}} | Acc]
        end
    end,
    lists:foldl(Scan1, [], Modules).
    
    module2prefix(Module) ->
        case atom_to_list(Module) of
            "mod_" ++ Rest -> Rest;
            Name -> Name
        end.

    scan_remove_prefix_ext(Filename, PrefixLen, Ext) ->
        Basename = filename:basename(Filename, Ext),
        lists:nthtail(PrefixLen, Basename).
