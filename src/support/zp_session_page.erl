%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Page session for interaction with the page displayed on the user agent. Support for comet polls.
%%      The page session is the switchboard for getting data pushed to the user agent.  The page session
%%      also caches the page dictionary.  Whenever the new page session is loaded it will check the page
%%      dictionary serial nr with the one stored.  When they differ then the page session will queue a request
%%      for fetching the data from the page in the user agent.  All queued requests can be sent via 
%%      the current request being handled or via a comet poll.


-module(zp_session_page).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

-include_lib("zophrenic.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% session exports
-export([
            start_link/0, 
            start_link/1, 
            stop/1, 
            
            set/3, 
            get/2, 
            incr/3, 
            append/3, 
            append_list/3, 
            
            add_script/2,
            get_scripts/1,
            comet_attach/2,
            comet_detach/1,
            get_attach_state/1,
            
            spawn_link/4
        ]).

-record(page_state, {
            vars,
            last_detach,
            comet_pid=undefined,
            comet_queue=[]
        }).


start_link() ->
    start_link([]).
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    try
        gen_server:cast(Pid, stop)
    catch _Class:_Term -> 
        error 
    end.

get_attach_state(Pid) ->
    try
        gen_server:call(Pid, get_attach_state)
    catch _Class:_Term -> 
        error 
    end.
        
set(Key, Value, Pid) ->
    gen_server:cast(Pid, {set, Key, Value}).

get(Key, Pid) ->
    gen_server:call(Pid, {get, Key}).

incr(Key, Value, Pid) ->
    gen_server:call(Pid, {incr, Key, Value}).

append(Key, Value, Pid) ->
    gen_server:cast(Pid, {append, Key, Value}).

append_list(Key, List, Pid) ->
    gen_server:cast(Pid, {append_list, Key, List}).

%% @doc Attach the comet request process to the page session, enabling sending scripts to the user agent
comet_attach(CometPid, Pid) ->
    gen_server:cast(Pid, {comet_attach, CometPid}).

%% @doc Called when the comet request process closes, we will need to wait for the next connection
comet_detach(Pid) ->
    gen_server:cast(Pid, comet_detach).

%% @doc Called by the comet process or the page request to fetch any outstanding scripts
get_scripts(Pid) ->
    gen_server:call(Pid, get_scripts).

%% @doc Send a script to the user agent, will be queued and send when the comet process attaches
add_script(Script, Pid) ->
    gen_server:cast(Pid, {add_script, Script}).

%% @doc Spawn a new process, linked to the page pid
spawn_link(Module, Func, Args, Context) ->
    gen_server:call(Context#context.page_pid, {spawn_link, Module, Func, Args, Context}).


%% ------------------------------------------------------------------------------------
%% Server implementation
%% ------------------------------------------------------------------------------------


init(_Args) ->
    State = #page_state{vars=dict:new(), last_detach=0},
    {ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast({set, Key, Value}, State) ->
    State1 = State#page_state{vars = dict:store(Key, Value, State#page_state.vars)},
    {noreply, State1};
handle_cast({append, Key, Value}, State) ->
    State1 = State#page_state{vars = dict:append(Key, Value, State#page_state.vars)},
    {noreply, State1};
handle_cast({append_list, Key, List}, State) ->
    State1 = State#page_state{vars = dict:append_list(Key, List, State#page_state.vars)},
    {noreply, State1};

handle_cast({comet_attach, CometPid}, State) ->
    case zp_utils:is_process_alive(CometPid) of
        true ->
            erlang:monitor(process, CometPid),
            StateComet = State#page_state{comet_pid=CometPid},
            StatePing  = ping_comet(StateComet),
            {noreply, StatePing};
        false ->
            {noreply, State}
    end;
handle_cast(comet_detach, State) ->
    StateNoComet = State#page_state{comet_pid=undefined, last_detach=zp_utils:now()},
    {noreply, StateNoComet};

handle_cast({add_script, Script}, State) ->
    StateQueued = State#page_state{comet_queue=[Script|State#page_state.comet_queue]},
    StatePing   = ping_comet(StateQueued),
    {noreply, StatePing}.

handle_call({spawn_link, Module, Func, Args, Context}, _From, State) ->
    Pid = spawn_link(Module, Func, [Args, Context]),
    {reply, Pid, State};

handle_call(get_scripts, _From, State) ->
    Queue   = State#page_state.comet_queue,
    State1  = State#page_state{comet_queue=[]},
    Scripts = lists:reverse(Queue),
    {reply, Scripts, State1};

handle_call({get, Key}, _From, State) ->
    Value = case dict:find(Key, State#page_state.vars) of
                {ok, V} -> V;
                error -> false
            end,
    {reply, Value, State};

handle_call({incr, Key, Delta}, _From, State) ->
    State1 = State#page_state{
                vars = dict:update_counter(Key, Delta, State#page_state.vars)
            },
    {ok, Value}  = dict:find(Key, State1#page_state.vars),
    {reply, Value, State1};

handle_call(get_attach_state, _From, State) ->
    case State#page_state.comet_pid of
        undefined ->
            {reply, {detached, State#page_state.last_detach}, State};
        _Pid ->
            {reply, attached, State}
    end.

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) when Pid == State#page_state.comet_pid ->
    {noreply, State#page_state{comet_pid=undefined, last_detach=zp_utils:now()}};
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------------------------------------------------------------------
%% Local functions
%% ------------------------------------------------------------------------------------

%% @doc Ping the comet process that we have a script queued
ping_comet(#page_state{comet_queue=[]} = State) ->
    State;
ping_comet(#page_state{comet_pid=undefined} = State) ->
    State;
ping_comet(State) ->
    try
        State#page_state.comet_pid ! script_queued,
        State
    catch _M : _E ->
        State#page_state{comet_pid=undefined}
    end.

