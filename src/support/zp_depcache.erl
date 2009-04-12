%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009  Marc Worrell
%%
%% @doc Simple caching server with dependency checks

-module(zp_depcache).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).

%% depcache exports
-export([set/2, set/3, set/4, get/1, get/2, flush/1, flush/0, tick/0, size/0]).
-export([memo/1, memo/2, memo/3, memo/4]).

%% internal export
-export([cleanup/1, cleanup/5]).

-include_lib("zophrenic.hrl").

-record(state, {now, serial, size=0}).
-record(meta,  {key, expire, serial, depend}).
-record(depend,{key, serial}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-define(META_TABLE, zp_depcache_meta).
-define(DEPS_TABLE, zp_depcache_deps).
-define(MEMORY_MAX, 20*1024*1024).

% Number of slots visited for each iteration
-define(CLEANUP_BATCH, 100).


memo({M,F,A}) ->
    memo({M,F,A}, ?HOUR, []).

memo({M,F,A}, MaxAge) ->
    memo({M,F,A}, MaxAge, []);
memo(F, Key) ->
    memo(F, Key, ?HOUR, []).

memo({M,F,A}, MaxAge, Dep) ->
    Key = memo_key({M,F,A}),
    case ?MODULE:get(Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Value = erlang:apply(M, F, A),
            set(Key, Value, MaxAge, Dep),
            Value
    end;
memo(F, Key, MaxAge) ->
    memo(F, Key, MaxAge, []).

memo(F, Key, MaxAge, Dep) ->
    case ?MODULE:get(Key) of
        {ok, Value} ->
            Value;
        undefined ->
            Value = F(),
            set(Key, Value, MaxAge, Dep),
            Value
    end.


memo_key({M,F,A}) -> 
    WithoutContext = lists:filter(fun(#context{}) -> false; (_) -> true end, A),
    {M,F,WithoutContext}.


%% @spec set(Key, Data, MaxAge) -> void()
%% @doc Add the key to the depcache, hold it for 3600 seconds and no dependencies
set(Key, Data) ->
    Size = erts_debug:flat_size(Data),
    gen_server:call(?MODULE, {set, Key, Data, Size, 3600, []}).


%% @spec set(Key, Data, MaxAge) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and no dependencies
set(Key, Data, MaxAge) ->
    Size = erts_debug:flat_size(Data),
    gen_server:call(?MODULE, {set, Key, Data, Size, MaxAge, []}).


%% @spec set(Key, Data, MaxAge, Depend) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and check the dependencies
set(Key, Data, MaxAge, Depend) ->
    Size = erts_debug:flat_size(Data),
    gen_server:call(?MODULE, {set, Key, Data, Size, MaxAge, Depend}).


%% @spec get(Key) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).


%% @spec get(Key, SubKey) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key, SubKey) ->
    gen_server:call(?MODULE, {get, Key, SubKey}).


%% @spec flush(Key) -> void()
%% @doc Flush the key and all keys depending on the key
flush(Key) ->
    gen_server:cast(?MODULE, {flush, Key}).

%% @spec flush() -> void()
%% @doc Flush all keys from the caches
flush() ->
    gen_server:cast(?MODULE, flush).

%% @spec tick() -> none()
%% @doc Periodic tick used for incrementing the clock
tick() ->
    gen_server:cast(?MODULE, tick).


%% @spec size() -> int()
%% @doc Return the total size of all stored terms
size() ->
    gen_server:call(?MODULE, size).
    

%% gen_server callbacks

%% @spec init([]) -> {ok, session_srv())}
%% @doc Initialize the session server with an empty session table.  We make the session manager a system process
%%      so that crashes in sessions are isolated from each other.
init(_Args) ->
    ets:new(?META_TABLE, [set, named_table, {keypos, 2}, protected]),
    ets:new(?DEPS_TABLE, [set, named_table, {keypos, 2}, protected]),
    State = #state{now=zp_utils:now(), serial=0},
    timer:apply_interval(1000, ?MODULE, tick, []),
    spawn_link(?MODULE, cleanup, [self()]),
    {ok, State}.

%% @doc Fetch a key from the cache, returns undefined when not found.
handle_call({get, Key}, _From, State) ->
    %% Find the key in the data table
    case ets:lookup(?META_TABLE, Key) of
        [] -> 
            {reply, undefined, State};
        [#meta{serial=Serial, expire=Expire, depend=Depend}] ->
            %% Check expiration
            case Expire >= State#state.now of
                false -> 
                    ets:delete(?META_TABLE, Key),
                    State1 = erase_key(Key, State),
                    {reply, undefined, State1};
                true ->
                    %% Check dependencies
                    case check_depend(Serial, Depend) of
                        true ->
                            case erlang:get(Key) of
                                {ok, _, Data} -> {reply, {ok, Data}, State};
                                _ -> {reply, undefined, State}
                            end;
                        false ->
                            ets:delete(?META_TABLE, Key),
                            State1 = erase_key(Key, State),
                            {reply, undefined, State1}
                    end
            end
    end;

%% @doc Fetch a subkey from a key from the cache, returns undefined when not found.
%% This is useful when the cached data is very large and the fetched data is small in comparison.
handle_call({get, Key, SubKey}, From, State) ->
    {reply, Data, State1} = handle_call({get, Key}, From, State),
    case Data of
        {ok, Value} ->
            {reply, {ok, find_value(SubKey, Value)}, State1};
        _ ->
            {reply, Data, State1}
    end;


%% @doc Return the size of all stored terms
handle_call(size, _From, State) ->
    {reply, State#state.size, State};


%% Add an entry to the cache table
handle_call({set, Key, Data, Size, MaxAge, Depend}, _From, State) ->
    %% On every update we inc the serial, flushing depending items
    State1 = State#state{serial=State#state.serial+1},
    
    %% Make sure all dependency keys are available in the deps table
    AddDepend = fun(D) -> 
                    ets:insert_new(?DEPS_TABLE, #depend{key=D, serial=State1#state.serial})
                end,
    lists:foreach(AddDepend, Depend),
    
    %% Flush the key itself in the dependency table - this will invalidate depending items
    case is_simple_key(Key) of
        true  -> ok;
        false -> ets:insert(?DEPS_TABLE, #depend{key=Key, serial=State#state.serial})
    end,

    %% Insert the record into the cache table
    erlang:put(Key, {ok, Size, Data}),
    ets:insert(?META_TABLE, #meta{key=Key, expire=State1#state.now+MaxAge, serial=State1#state.serial, depend=Depend}),
    {reply, ok, State1#state{size=State1#state.size + Size}}.

handle_cast({flush, Key}, State) ->
    ets:delete(?DEPS_TABLE, Key),
    ets:delete(?META_TABLE, Key),
    State1 = erase_key(Key, State),
    {noreply, State1};

handle_cast(flush, State) ->
    ets:delete_all_objects(?META_TABLE),
    ets:delete_all_objects(?DEPS_TABLE),
    erlang:erase(),
    {noreply, State#state{size=0}};

handle_cast(tick, State) ->
    {noreply, State#state{now=zp_utils:now()}};

handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_info(_Msg, State) -> 
    {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.



%% @doc Check if a key is usable as dependency key.  That is a string, atom, integer etc, but not a list of lists.
is_simple_key([]) ->
    true;
is_simple_key([H|_]) -> 
    not is_list(H);
is_simple_key(_Key) ->
    true.


%% @doc Erase the key from the process dictionary, substract the size from the accumulator.
erase_key(Key, State) ->
    Data = erlang:erase(Key),
    case Data of
        {ok, Size, _} ->
            State#state{size = State#state.size - Size};
        undefined ->
            State
    end.


%% @doc Check if all dependencies are still valid, that is they have a serial before or equal to the serial of the entry
check_depend(_Serial, []) ->
    true;
check_depend(Serial, Depend) ->
    CheckDepend = fun
                        (_Dep,false) -> false;
                        (Dep,true) ->
                            case ets:lookup(?DEPS_TABLE, Dep) of
                                [#depend{serial=DepSerial}] -> DepSerial =< Serial;
                                _ -> false
                            end
                    end,
    lists:foldl(CheckDepend, true, Depend).



% Index of list with an integer like "a[2]"
find_value(Key, L) when is_integer(Key) andalso is_list(L) ->
    try
        lists:nth(Key, L)
    catch
        _:_ -> undefined
    end;
find_value(Key, {GBSize, GBData}) when is_integer(GBSize) ->
    case gb_trees:lookup(Key, {GBSize, GBData}) of
        {value, Val} ->
            Val;
        _ ->
            undefined
    end;

%% Regular proplist lookup
find_value(Key, L) when is_list(L) ->
    proplists:get_value(Key, L);

%% Resource list handling, special lookups when skipping the index
find_value(Key, #rsc_list{list=L}) when is_integer(Key) ->
    try
        lists:nth(Key, L)
    catch
        _:_ -> undefined
    end;
find_value(Key, #rsc_list{list=[H|_T]}) ->
	find_value(Key, H);
find_value(_Key, #rsc_list{list=[]}) ->
	undefined;

% Index of tuple with an integer like "a[2]"
find_value(Key, T) when is_integer(Key) andalso is_tuple(T) ->
    try
        element(Key, T)
    catch 
        _:_ -> undefined
    end;

%% Other cases: context, dict or parametrized module lookup.
find_value(Key, Tuple) when is_tuple(Tuple) ->
    Module = element(1, Tuple),
    case Module of
        dict -> 
            case dict:find(Key, Tuple) of
                {ok, Val} ->
                    Val;
                _ ->
                    undefined
            end;
        Module ->
            Exports = Module:module_info(exports),
            case proplists:get_value(Key, Exports) of
                1 ->
                    Tuple:Key();
                _ ->
                    case proplists:get_value(get, Exports) of
                        1 -> 
                            Tuple:get(Key);
                        _ ->
                            undefined
                    end
            end
    end;

%% Any subvalue of a non-existant value is empty
find_value(_Key, _Data) ->
	undefined.


%% @doc Cleanup process for the depcache.  Periodically checks a batch of depcache items for their validity.
%%      Asks the depcache server to delete invalidated items.  When the load of the data table is too high then
%%      This cleanup process starts to delete random entries.  By using a random delete we don't need to keep
%%      a LRU list, which is a bit expensive.

cleanup(Pid) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    ?MODULE:cleanup(Pid, 0, zp_utils:now(), normal, 0).

%% Wrap around the end of table
cleanup(Pid, '$end_of_table', Now, _Mode, Ct) ->
    case ets:info(?META_TABLE, size) of
        0 -> ?MODULE:cleanup(Pid, 0, Now, cleanup_mode(Pid), 0);
        _ -> ?MODULE:cleanup(Pid, 0, Now, cleanup_mode(Pid), Ct)
    end;

%% In normal cleanup, sleep a second between each batch before continuing our cleanup sweep
cleanup(Pid, SlotNr, Now, normal, 0) -> 
    timer:sleep(1000),
    case ets:info(?META_TABLE, size) of
        0 -> ?MODULE:cleanup(Pid, 0, Now, normal, 0);
        _ -> ?MODULE:cleanup(Pid, SlotNr, zp_utils:now(), cleanup_mode(Pid), ?CLEANUP_BATCH)
    end;

%% After finishing a batch in cache_full mode, check if the cache is still full, if so keep deleting entries
cleanup(Pid, SlotNr, Now, cache_full, 0) -> 
    case cleanup_mode(Pid) of
        normal     -> ?MODULE:cleanup(Pid, SlotNr, Now, normal, 0);
        cache_full -> ?MODULE:cleanup(Pid, SlotNr, zp_utils:now(), cache_full, ?CLEANUP_BATCH)
    end;

%% Normal cleanup behaviour - check expire stamp and dependencies
cleanup(Pid, SlotNr, Now, normal, Ct) ->
    Slot =  try 
                ets:slot(?META_TABLE, SlotNr)
            catch
                _M:_E -> '$end_of_table'
            end,
    case Slot of
        '$end_of_table' -> ?MODULE:cleanup(Pid, '$end_of_table', Now, normal, 0);
        [] -> ?MODULE:cleanup(Pid, SlotNr+1, Now, normal, Ct-1);
        Entries ->
            lists:foreach(fun(Meta) -> flush_expired(Meta, Now) end, Entries),
            ?MODULE:cleanup(Pid, SlotNr+1, Now, normal, Ct-1)
    end;

%% Full cache cleanup mode - randomly delete every 10th entry
cleanup(Pid, SlotNr, Now, cache_full, Ct) ->
    Slot =  try 
                ets:slot(?META_TABLE, SlotNr)
            catch
                _M:_E -> '$end_of_table'
            end,
    case Slot of
        '$end_of_table' -> ?MODULE:cleanup(Pid, '$end_of_table', Now, cache_full, 0);
        [] -> ?MODULE:cleanup(Pid, SlotNr+1, Now, cache_full, Ct-1);
        Entries ->
            FlushExpire = fun (Meta) ->
                                case flush_expired(Meta, Now) of
                                    ok -> {ok, Meta};
                                    flushed -> flushed
                                end
                           end,
            RandomDelete = fun
                                ({ok, #meta{key=Key}}) ->
                                    case random:uniform(10) of
                                        10 -> ?MODULE:flush(Key);
                                        _  -> ok
                                    end;
                                (flushed) -> flushed
                           end,

            Entries1 = lists:map(FlushExpire, Entries),
            lists:foreach(RandomDelete, Entries1),
            ?MODULE:cleanup(Pid, SlotNr+1, Now, cache_full, Ct-1)
    end.


%% @doc Check if an entry is expired, if so delete it
flush_expired(#meta{key=Key, serial=Serial, expire=Expire, depend=Depend}, Now) ->
    Expired = Expire < Now orelse not check_depend(Serial, Depend),
    case Expired of
        true  -> ?MODULE:flush(Key), flushed;
        false -> ok
    end.


%% @doc When the data table is too large then we start to randomly delete keys.  It also signals the cleanup process
%% that it needs to be more aggressive, upping its batch size.
%% We use erts_debug:size() on the stored terms to calculate the total size of all terms stored.  This
%% is better than counting the number of entries.  Using the process_info(Pid,memory) is not very useful as the
%% garbage collection still needs to be done and then we delete too many entries.
cleanup_mode(_Pid) ->
    Memory = ?MODULE:size(),
    if 
        Memory >= ?MEMORY_MAX -> cache_full;
        true -> normal
    end.
