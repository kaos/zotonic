%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc User agent session management for zophrenic.  A ua session is a process started for every
%%      user agent visiting the site.  The session is alive for a fixed period after the 
%%      last request has been done.  The session manager manages all the ua session processes.

%% TODO: make sure that all sessions and page sessions are linked to some process so that they will be killed 
%%       when the application is unloaded.


-module(zp_session_manager).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% The name of the session cookie
-define(SESSION_COOKIE, "zpsid").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

%% External exports
-export([
    ensure_session/1, 
    stop_session/1, 
    rename_session/1, 
    count/0, 
    dump/0, 
    tick/0
]).

-include_lib("webmachine.hrl").
-include_lib("zophrenic.hrl").

%% The session server state
-record(session_srv, {key2pid, pid2key}).


start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @spec start_session(Context) -> Context
%% @doc Start a new session or continue an existing session
ensure_session(Context) ->
    gen_server:call(?MODULE, {ensure_session, Context}).

%% @spec stop_session(Context) -> Context
%% @doc Explicitly stop an existing session
stop_session(Context) ->
    gen_server:call(?MODULE, {stop_session, Context}).

%% @spec rename_session(Context) -> Context
%% @doc Ensure a session and rename the cookie
rename_session(Context) ->
    gen_server:call(?MODULE, {rename_session, Context}).

%% @spec count() -> Int
%% @doc Return the number of open sessions
count() ->
    gen_server:call(?MODULE, count).


%% @spec dump() -> void()
%% @doc Dump all session to stdout
dump() ->
    gen_server:call(?MODULE, dump).


%% @spec tick() -> none()
%% @doc Periodic tick used for cleaning up sessions
tick() ->
    gen_server:cast(?MODULE, tick).


%% gen_server callbacks

%% @spec init([]) -> {ok, session_srv())}
%% @doc Initialize the session server with an empty session table.  We make the session manager a system process
%%      so that crashes in sessions are isolated from each other.
init(_Args) ->
    State = #session_srv{key2pid=dict:new(), pid2key=dict:new()},
    timer:apply_interval(?SESSION_CHECK_EXPIRE * 1000, ?MODULE, tick, []),
    process_flag(trap_exit, true),
    {ok, State}.

%% Ensure that the request has a session attached, pings the session
handle_call({ensure_session, Context}, _From, State) ->
    SessionId = get_session_id(Context),
    Pid       = session_find_pid(SessionId, State),
    {_Pid1, Context1, State1} = ensure_session1(SessionId, Pid, Context, State),
    {reply, Context1, State1};

%% Stop the session from the context or the request props
handle_call({stop_session, Context}, _From, State) ->
    ReqProps = zp_context:get_reqprops(Context),
    case Context#context.session_pid of
        undefined -> 
            SessionId = get_session_id(ReqProps),
            case SessionId of
                undefined -> true;
                S -> forget_session_id(S, State)
            end;
        Pid -> 
            zp_session:stop(Pid)
    end,
    Context1 = clear_session_id(Context),
    {reply, Context1, State};


%% Rename the current session, retain the same session pid, only call this after ensure_session
handle_call({rename_session, Context}, _From, State) ->
    {reply, Context, State};

%% Return the number of sessions
handle_call(count, _From, State) ->
    Count = dict:size(State#session_srv.pid2key),
    {reply, Count, State};

%% Dump all sessions to stdout
handle_call(dump, _From, State) ->
    SesPids = dict:fetch_keys(State#session_srv.pid2key),
    lists:foreach(fun(Pid) -> zp_session:dump(Pid) end, SesPids),
    {reply, ok, State};
    
handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.


%% Handle the down message from a stopped session, remove it from the session admin
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    State1 = erase_session_pid(Pid, State),
    {noreply, State1};
handle_info(_Msg, State) -> 
    {noreply, State}.


handle_cast(tick, State) ->
    Tick    = zp_utils:now(),
    SesPids = dict:fetch_keys(State#session_srv.pid2key),
    lists:foreach(fun(Pid) -> zp_session:check_expire(Tick, Pid) end, SesPids),
    {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.


%% Make sure that the session cookie is set and that the session process has been started.
ensure_session1(S, P, Context, State) when S == undefined orelse P == error ->
    Pid       = spawn_session(State),
    SessionId = zp_ids:id(),
    Context1  = set_session_id(SessionId, Context),
    State1    = store_session_pid(SessionId, Pid, State),
    Context2  = Context1#context{session_pid = Pid},
    {Pid, Context2, State1};
ensure_session1(_SessionId, Pid, Context, State) ->
    zp_session:keepalive(Context#context.page_pid, Pid),
    Context1  = Context#context{session_pid = Pid},
    {Pid, Context1, State}.


%% @spec forget_session_pid(pid(), State) -> State
%% @doc Remove the pid from the session state
erase_session_pid(Pid, State) ->
    case dict:find(Pid, State#session_srv.pid2key) of
        {ok, Key} ->
            State#session_srv{
                    pid2key = dict:erase(Pid, State#session_srv.pid2key),
                    key2pid = dict:erase(Key, State#session_srv.key2pid)
                };
        error ->
            State
    end.


%% @spec store_session_pid(pid(), State) -> State
%% @doc Add the pid to the session state
store_session_pid(SessionId, Pid, State) ->
    State#session_srv{
            pid2key = dict:store(Pid, SessionId, State#session_srv.pid2key),
            key2pid = dict:store(SessionId, Pid, State#session_srv.key2pid)
        }.

%% @spec forget_session_id(SessionId::string(), State) -> true | error
%% @doc Stop the session process linked to the session id
forget_session_id(SessionId, State) ->
    case dict:find(SessionId, State#session_srv.key2pid) of
        {ok, Pid} ->
            zp_session:stop(Pid);
        error ->
            error
    end.


%% @spec session_find_pid(string(), State) ->  error | pid()
%% @doc find the pid associated with the session id
session_find_pid(undefined, _State) ->
    error;
session_find_pid(SessionId, State) ->
    case dict:find(SessionId, State#session_srv.key2pid) of
        {ok, Pid} ->
            Pid;
        error ->
            error
    end.


%% @spec new_session(State::state()) -> pid()
%% @doc Spawn a new session, monitor the pid as we want to know about normal exits
spawn_session(_State) ->
    case zp_session:start_link() of
        {ok, Pid} ->
                erlang:monitor(process, Pid),
                Pid
    end.


%% @spec get_session_id(Context) -> undefined | string()
%% @doc fetch the session id from the request, return error when not found
get_session_id(Context) ->
    ReqProps = zp_context:get_reqprops(Context),
    Req      = ?REQ(ReqProps),
    Req:get_cookie_value(?SESSION_COOKIE).

%% @spec set_session_id(SessionId::string(), Context::#context) -> #context
%% @doc Save the session id in a cookie on the user agent
set_session_id(SessionId, Context) ->
    ReqProps = zp_context:get_reqprops(Context),
    Req      = ?REQ(ReqProps),
    %% TODO: set the {domain,"example.com"} of the session cookie
    {K,V}    = mochiweb_cookies:cookie(?SESSION_COOKIE, SessionId, []),
    Req:add_response_header(K,V),
    Context.

%% @spec clear_session_id(Context::#context) -> #context
%% @doc Remove the session id from the user agent and clear the session pid in the context
clear_session_id(Context) ->
    ReqProps = zp_context:get_reqprops(Context),
    Req      = ?REQ(ReqProps),
    %% TODO: set the {domain,"example.com"} of the session cookie
    Hdr      = mochiweb_cookies:cookie(?SESSION_COOKIE, "", [{max_age, 0}]),
    Req:merge_response_headers([Hdr]),
    Context#context{session_pid=undefined}.

