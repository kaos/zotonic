%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Person process.  The person process binds all sessions of a person together.

-module(zp_person).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([new_anonymous/1, new_cookie_person/2, associate_session/2]).

-include_lib("zophrenic.hrl").

-record(state, {sessions}).

%%====================================================================
%% API
%%====================================================================

%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the person process server
start_link() ->
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).


%% @spec new_anonymous(SessionPid) -> {ok, pid()}
%% @doc Start a new anonymous user process, link to the new process.
new_anonymous(SessionPid) ->
    {ok, Pid} = start_link([{session, SessionPid}]),
    {ok, Pid}.


%% @spec new_cookie_person(CookieId, SessionPid) -> {ok, pid()} | error
%% @doc Start a new process for the person associated with the cookie, return error when no person associated.
new_cookie_person(_CookieId, _SessionPid) ->
    error.


%% @spec associate_session(Pid, SessionPid) -> void()
%% @doc Associate a new session with this person, monitor the session so that we known when the session is gone.
associate_session(Pid, SessionPid) when is_pid(Pid) andalso is_pid(SessionPid) ->
    gen_server:cast(Pid, {associate_session, SessionPid});
associate_session(_Pid, _SessionPid) -> 
    ok.
    

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    State = #state{
                sessions=[]
            },
    State1 = add_session(proplists:get_value(session, Args), State),
    {ok, State1, ?PERSON_TIMEOUT}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State, ?PERSON_TIMEOUT}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}

%% @doc Trap unknown casts
handle_cast({associate_session, SessionPid}, State) ->
    State1 = add_session(SessionPid, State),
    {noreply, State1, ?PERSON_TIMEOUT};

%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State, ?PERSON_TIMEOUT}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}

%% @doc Handle the disappearance of a session process
handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    RemainingSessions = lists:delete(Pid, State#state.sessions),
    {noreply, State#state{sessions=RemainingSessions}, ?PERSON_TIMEOUT};

%% @doc Handle a timeout after a period of inactivity, stop the process when no sessions left
handle_info(timeout, State) ->
    case length(State#state.sessions) of
        0 -> {stop, normal, State};
        _ -> {noreply, State, ?PERSON_TIMEOUT}
    end;

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State, ?PERSON_TIMEOUT}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @spec add_session(pid(), State) -> NewState
%% @doc Add a session to the person session, ignore when undefined or pid is already known
add_session(undefined, State) ->
    State;
add_session(SessionPid, State) ->
    case lists:member(SessionPid, State#state.sessions) of
        false ->
            erlang:monitor(process, SessionPid),
            State#state{sessions=[SessionPid|State#state.sessions]};
        true ->
            State
    end.
