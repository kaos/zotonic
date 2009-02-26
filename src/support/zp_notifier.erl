%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Simple implementation of an observer/notifier. Relays events to observers of that event.

-module(zp_notifier).

-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([observe/2, detach/2, notify/1, notify_sync/1]).

%% internal
-export([notify_observers/2, test/0, test_observer/1]).


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
    gen_server:cast(?MODULE, {'observe', Event, Observer}).

%% @doc Detach all observers and delete the event
detach_all(Event) ->
    gen_server:cast(?MODULE, {'detach_all', Event}).

%% @doc Unsubscribe from an event. Observer is a {M,F} or pid()
detach(Event, Observer) ->
    gen_server:cast(?MODULE, {'detach', Event, Observer}).


%% @doc Async notify observers of an event
notify(Event) ->
    gen_server:cast(?MODULE, {'notify', Event}).


%% @doc Synchronously notify observers, wait till all have done their work
notify_sync(Event) ->
    gen_server:call(?MODULE, {'notify_sync', Event}).


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

%% @doc Trigger an event, notify all observers synchronously
handle_call({'notify_sync', Event}, _From, State) ->
    case dict:find(Event, State#state.observers) of
        {ok, Observers} -> notify_observers(Event, Observers);
        error -> ok
    end,
    {reply, ok, State};

%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}

%% @doc Add an observer to an event
handle_cast({'observe', Event, Observer}, State) ->
    Observers1 = case dict:find(Event, State#state.observers) of
                  {ok, _} -> dict:append(Event, Observer, State#state.observers);
                  error -> dict:store(Event, [Observer], State#state.observers)
                  end,
    {noreply, State#state{observers=Observers1}};

%% @doc Detach an observer from an event
handle_cast({'detach', Event, Observer}, State) ->
    Observers1 = case dict:find(Event, State#state.observers) of
                  {ok, Olist} ->
                      io:format("Before: ~p (delete ~p)~n", [Olist,Observer]),
                      Olist1 = lists:delete(Observer, Olist),
                      io:format("After: ~p~n", [Olist1]),
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
            spawn(fun() -> notify_observers(Msg, Observers) end);
        error -> ok
    end,
    {noreply, State};

handle_cast({'notify', Event}, State) ->
    case dict:find(Event, State#state.observers) of
        {ok, Observers} ->
            spawn(fun() -> notify_observers(Event, Observers) end);
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

%% @doc Notify all observers of an event
notify_observers(Event, Observers) ->
    lists:foreach(
                fun
                    (Fun) when is_function(Fun) ->
                        catch Fun(Event);
                    (Pid) when is_pid(Pid) ->
                        catch Pid ! {Event};
                    ({M,F}) ->
                        catch M:F(Event)
                end,
                Observers).


%% Simple test

test() ->
    detach_all(test),
    observe(test, {?MODULE, test_observer}),
    notify(test),
    notify({test, arg1, arg2}),
    detach(test, {?MODULE, test_observer}).

test_observer(Event) ->
    io:format("Received Event \"~p\"~n", [Event]).

