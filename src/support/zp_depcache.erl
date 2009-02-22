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
-export([set/2, set/3, set/4, get/1, flush/1, flush/0, tick/0]).

%% internal export
-export([cleanup/4]).

-record(state, {now, serial}).
-record(meta,  {key, expire, serial, depend}).
-record(data,  {key, data}).
-record(depend,{key, serial}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-define(DATA_TABLE, zp_depcache_data).
-define(META_TABLE, zp_depcache_meta).
-define(DEPS_TABLE, zp_depcache_deps).
-define(MEMORY_MAX, 10*1024*1024).

% Number of slots visited for each iteration
-define(CLEANUP_BATCH, 100).


%% @spec set(Key, Data, MaxAge) -> void()
%% @doc Add the key to the depcache, hold it for 3600 seconds and no dependencies
set(Key, Data) ->
    gen_server:cast(?MODULE, {set, Key, Data, 3600, []}).


%% @spec set(Key, Data, MaxAge) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and no dependencies
set(Key, Data, MaxAge) ->
    gen_server:cast(?MODULE, {set, Key, Data, MaxAge, []}).


%% @spec set(Key, Data, MaxAge, Depend) -> void()
%% @doc Add the key to the depcache, hold it for MaxAge seconds and check the dependencies
set(Key, Data, MaxAge, Depend) ->
    gen_server:cast(?MODULE, {set, Key, Data, MaxAge, Depend}).


%% @spec get(Key) -> {ok, Data} | undefined
%% @doc Fetch the key from the cache, return the data or an undefined if not found (or not valid)
get(Key) ->
    gen_server:call(?MODULE, {get, Key}).


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


%% gen_server callbacks

%% @spec init([]) -> {ok, session_srv())}
%% @doc Initialize the session server with an empty session table.  We make the session manager a system process
%%      so that crashes in sessions are isolated from each other.
init(_Args) ->
    ets:new(?DATA_TABLE, [set, named_table, {keypos, 2}, protected]),
    ets:new(?META_TABLE, [set, named_table, {keypos, 2}, protected]),
    ets:new(?DEPS_TABLE, [set, named_table, {keypos, 2}, protected]),
    State = #state{now=zp_utils:now(), serial=0},
    timer:apply_interval(1000, ?MODULE, tick, []),
    spawn_link(fun cleanup/0),
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
                    ets:delete(?DATA_TABLE, Key),
                    {reply, undefined, State};
                true ->
                    %% Check dependencies
                    case check_depend(Serial, Depend) of
                        true ->
                            [Data] = ets:lookup(?DATA_TABLE, Key),
                            {reply, {ok, Data#data.data}, State};
                        false ->
                            ets:delete(?META_TABLE, Key),
                            ets:delete(?DATA_TABLE, Key),
                            {reply, undefined, State}
                    end
            end
    end.


%% Add an entry to the cache table
handle_cast({set, Key, Data, MaxAge, Depend}, State) ->
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
        false -> 
            ets:insert(?DEPS_TABLE, #depend{key=Key, serial=State#state.serial})
    end,

    %% Insert the record into the cache table
    ets:insert(?DATA_TABLE, #data{key=Key, data=Data}),
    ets:insert(?META_TABLE, #meta{key=Key, expire=State1#state.now+MaxAge, serial=State1#state.serial, depend=Depend}),
    {noreply, State1};

handle_cast({flush, Key}, State) ->
    ets:delete(?DEPS_TABLE, Key),
    ets:delete(?META_TABLE, Key),
    ets:delete(?DATA_TABLE, Key),
    {noreply, State};

handle_cast(flush, State) ->
    ets:delete_all_objects(?META_TABLE),
    ets:delete_all_objects(?DEPS_TABLE),
    ets:delete_all_objects(?DATA_TABLE),
    {noreply, State};

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


%% @doc Cleanup process for the depcache.  Periodically checks a batch of depcache items for their validity.
%%      Asks the depcache server to delete invalidated items.  When the load of the data table is too high then
%%      This cleanup process starts to delete random entries.  By using a random delete we don't need to keep
%%      a LRU list, which is a bit expensive.

cleanup() ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    ?MODULE:cleanup(0, zp_utils:now(), normal, 0).

%% Wrap around the end of table
cleanup('$end_of_table', Now, _Mode, Ct) ->
    case ets:info(?META_TABLE, size) of
        0 -> ?MODULE:cleanup(0, Now, cleanup_mode(), 0);
        _ -> ?MODULE:cleanup(0, Now, cleanup_mode(), Ct)
    end;

%% In normal cleanup, sleep a second between each batch before continuing our cleanup sweep
cleanup(SlotNr, Now, normal, 0) -> 
    timer:sleep(1000),
    case ets:info(?META_TABLE, size) of
        0 -> ?MODULE:cleanup(0, Now, normal, 0);
        _ -> ?MODULE:cleanup(SlotNr, zp_utils:now(), cleanup_mode(), ?CLEANUP_BATCH)
    end;

%% After finishing a batch in cache_full mode, check if the cache is still full, if so keep deleting entries
cleanup(SlotNr, Now, cache_full, 0) -> 
    case cleanup_mode() of
        normal     -> ?MODULE:cleanup(SlotNr, Now, normal, 0);
        cache_full -> ?MODULE:cleanup(SlotNr, zp_utils:now(), cache_full, ?CLEANUP_BATCH)
    end;

%% Normal cleanup behaviour - check expire stamp and dependencies
cleanup(SlotNr, Now, normal, Ct) ->
    Slot =  try 
                ets:slot(?META_TABLE, SlotNr)
            catch
                _M:_E -> '$end_of_table'
            end,
    case Slot of
        '$end_of_table' -> ?MODULE:cleanup('$end_of_table', Now, normal, 0);
        [] -> ?MODULE:cleanup(SlotNr+1, Now, normal, Ct-1);
        Entries ->
            lists:foreach(fun(Meta) -> flush_expired(Meta, Now) end, Entries),
            ?MODULE:cleanup(SlotNr+1, Now, normal, Ct-1)
    end;

%% Full cache cleanup mode - randomly delete every 10th entry
cleanup(SlotNr, Now, cache_full, Ct) ->
    Slot =  try 
                ets:slot(?META_TABLE, SlotNr)
            catch
                _M:_E -> '$end_of_table'
            end,
    case Slot of
        '$end_of_table' -> ?MODULE:cleanup('$end_of_table', Now, cache_full, 0);
        [] -> ?MODULE:cleanup(SlotNr+1, Now, cache_full, Ct-1);
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
            ?MODULE:cleanup(SlotNr+1, Now, cache_full, Ct-1)
    end.


%% @doc Check if an entry is expired, if so delete it
flush_expired(#meta{key=Key, serial=Serial, expire=Expire, depend=Depend}, Now) ->
    Expired = Expire < Now orelse not check_depend(Serial, Depend),
    case Expired of
        true  -> ?MODULE:flush(Key), flushed;
        false -> ok
    end.


%% @doc When the data table is too large then this function randomly deletes keys.  It also signals the cleanup process
%%      That it needs to be more aggressive, upping its batch size.
cleanup_mode() ->
    Memory = ets:info(?DATA_TABLE, memory),
    if 
        Memory >= ?MEMORY_MAX -> cache_full;
        true -> normal
    end.
