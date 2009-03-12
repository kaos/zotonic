%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Simple implementation of an observer/notifier. Relays events to observers of that event.
%% Also implements map and fold operations.

-module(zp_notifier).

-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    observe/2,
    observe/3,
    detach/2,
    detach_all/1,
    get_observers/1,
    notify/1, 
    notify1/1, 
    first/1, 
    map/1, 
    foldl/2, 
    foldr/2
]).

%% internal
-export([notify_observer/3, test/0, test_observer/1]).

-include_lib("zophrenic.hrl").

-define(DEFAULT_PRIORITY, 500).
-define(TIMEOUT, 60000).

-record(state, {observers}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the notification server
start_link() ->
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% @doc Subscribe to an event. Observer is a {M,F} or pid()
observe(Event, Observer) ->
    gen_server:cast(?MODULE, {'observe', Event, Observer, ?DEFAULT_PRIORITY}).

%% @doc Subscribe to an event. Observer is a {M,F} or pid()
observe(Event, Observer, Priority) ->
    gen_server:cast(?MODULE, {'observe', Event, Observer, Priority}).

%% @doc Detach all observers and delete the event
detach_all(Event) ->
    gen_server:cast(?MODULE, {'detach_all', Event}).

%% @doc Unsubscribe from an event. Observer is a {M,F} or pid()
detach(Event, Observer) ->
    gen_server:cast(?MODULE, {'detach', Event, Observer}).

%% @doc Return all observers for a particular event
get_observers(Event) ->
    gen_server:call(?MODULE, {'get_observers', Event}).

%% @doc Cast the event to all observers
notify(Event) ->
    gen_server:cast(?MODULE, {'notify', Event}).

%% @doc Cast the event to the first observer
notify1(Event) ->
    gen_server:cast(?MODULE, {'notify1', Event}).

%% @doc Call all observers till one returns something else than false
first(Event) ->
    gen_server:call(?MODULE, {'first', Event}).

%% @doc Call all observers, return the list of answers
map(Event) ->
    gen_server:call(?MODULE, {'map', Event}).

%% @doc Do a fold over all observers, prio 1 observers first
foldl(Event, Acc0) ->
    gen_server:call(?MODULE, {'foldl', Event, Acc0}).

%% @doc Do a fold over all observers, prio 1 observers last
foldr(Event, Acc0) ->
    gen_server:call(?MODULE, {'foldr', Event, Acc0}).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server, creates a new observer list
init(_Args) ->
    State = #state{observers=dict:new()},
    {ok, State}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages

%% @doc Return the list of observers for an event
handle_call({'get_observers', Event}, _From, State) ->
    case dict:find(Event, State#state.observers) of
        {ok, Observers} ->
            {reply, Observers, State};
        error ->
            {reply, [], State}
    end;

%% @doc Call all observers, return the list of answers
%% @todo Make the map a pmap operation
handle_call({'map', Msg}, From, State) ->
    Event = element(1, Msg),
    case dict:find(Event, State#state.observers) of
        {ok, Observers} ->
            spawn(fun() -> 
                        Result = lists:map(fun(Obs) -> notify_observer(Msg, Obs, true) end, Observers),
                        gen_server:reply(From, Result)
                  end),
            {noreply, State};
        error -> 
            {reply, undefined, State}
    end;

%% @doc Call the observers, highest prio first, till one return not false
handle_call({'first', Msg}, From, State) ->
    Event = element(1, Msg),
    case dict:find(Event, State#state.observers) of
        {ok, Observers} ->
            spawn(fun() -> 
                        Result = lists:foldl(
                                    fun
                                        (Obs, false) -> notify_observer(Msg, Obs, true);
                                        (_Obs, Acc) -> Acc
                                    end, 
                                    false,
                                    Observers),
                        gen_server:reply(From, Result)
                  end),
            {noreply, State};
        error -> 
            {reply, undefined, State}
    end;

%% @doc Do a fold over all observers, highest prio first. The accumulator is appended to the message.
handle_call({'foldl', Msg, Acc0}, From, State) ->
    Event = element(1, Msg),
    case dict:find(Event, State#state.observers) of
        {ok, Observers} ->
            spawn(fun() -> 
                        Result = lists:foldl(
                                    fun(Obs, Acc) -> 
                                        MsgAcc = list_to_tuple(tuple_to_list(Msg) ++ [Acc]),
                                        notify_observer(MsgAcc, Obs, true) 
                                    end, 
                                    Acc0,
                                    Observers),
                        gen_server:reply(From, Result)
                  end),
            {noreply, State};
        error -> 
            {reply, undefined, State}
    end;

%% @doc Do a fold over all observers, lowest prio first. The accumulator is appended to the message.
handle_call({'foldr', Msg, Acc0}, From, State) ->
    Event = element(1, Msg),
    case dict:find(Event, State#state.observers) of
        {ok, Observers} ->
            spawn(fun() -> 
                        Result = lists:foldr(
                                    fun(Obs, Acc) -> 
                                        MsgAcc = list_to_tuple(tuple_to_list(Msg) ++ [Acc]),
                                        notify_observer(MsgAcc, Obs, true) 
                                    end, 
                                    Acc0,
                                    Observers),
                        gen_server:reply(From, Result)
                  end),
            {noreply, State};
        error -> 
            {reply, undefined, State}
    end;

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}

%% @doc Add an observer to an event
handle_cast({'observe', Event, Observer, Priority}, State) ->
    Observers1 = case dict:find(Event, State#state.observers) of
                  {ok, EventObservers} -> 
                        Os1 = lists:sort([{Priority, Observer}|EventObservers]),
                        dict:store(Event, Os1, State#state.observers);
                  error -> 
                        dict:store(Event, [{Priority, Observer}], State#state.observers)
                  end,
    {noreply, State#state{observers=Observers1}};

%% @doc Detach an observer from an event
handle_cast({'detach', Event, Observer}, State) ->
    Observers1 = case dict:find(Event, State#state.observers) of
                  {ok, Olist} ->
                      Olist1 = lists:filter(fun({_Prio,Obs}) -> Obs /= Observer end, Olist),
                      dict:store(Event, Olist1, State#state.observers);
                  error ->
                      State#state.observers
                  end,
    {noreply, State#state{observers=Observers1}};


%% @doc Detach all observer from an event
handle_cast({'detach_all', Event}, State) ->
    {noreply, State#state{observers = dict:erase(Event, State#state.observers)}};


%% @doc Trigger an event, notify all observers asynchronously
handle_cast({'notify', Msg}, State) when is_tuple(Msg) ->
    Event = element(1, Msg),
    case dict:find(Event, State#state.observers) of
        {ok, Observers} ->
            spawn(fun() -> 
                        lists:foreach(fun(Obs) -> notify_observer(Msg, Obs, false) end, Observers)
                  end);
        error -> ok
    end,
    {noreply, State};


%% @doc Trigger an event, notify the first observer asynchronously
handle_cast({'notify1', Msg}, State) when is_tuple(Msg) ->
    Event = element(1, Msg),
    case dict:find(Event, State#state.observers) of
        {ok, [Observer|_Observers]} ->
            spawn(fun() -> notify_observer(Msg, Observer, false) end);
        error -> ok
    end,
    {noreply, State};


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
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Notify an observer of an event
notify_observer(Msg, {_Prio, Fun}, _IsCall) when is_function(Fun) ->
    Fun(Msg);
notify_observer(Msg, {_Prio, Pid}, IsCall) when is_pid(Pid) ->
    try
        Pid ! {'notify', Msg, self()},
        case IsCall of
            true ->
                receive
                {reply, Pid, Reply} -> 
                    Reply
                after ?TIMEOUT ->
                    false
                end;
            false -> 
                ok
        end
    catch _M:_E ->
        ?LOG("Error notifying %p with event %p. Detaching pid.", [Pid, Msg]),
        Event = element(1, Msg),
        detach(Event, Pid)
    end;
notify_observer(Msg, {_Prio, {M,F}}, _IsCall) ->
    M:F(Msg).


%% Simple test

test() ->
    detach_all(test_blaat),
    observe(test_blaat, {?MODULE, test_observer}),
    notify({test_blaat, arg1, arg2}),
    [{?DEFAULT_PRIORITY, {?MODULE, test_observer}}] = get_observers(test_blaat),
    detach(test_blaat, {?MODULE, test_observer}),
    [] = get_observers(test_blaat).
    

test_observer(Event) ->
    io:format("Received Event \"~p\"~n", [Event]).

