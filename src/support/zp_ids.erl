%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Server supplying random strings and unique ids

-module(zp_ids).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

%% Length of session keys, used for the cookies, must be unique
-define(ID_LENGTH,20).
-define(OPTID_LENGTH,6).

%% Range of random numbers returned
-define(RANDOM_RANGE, 2000000000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% id server exports
-export([
    unique/0, 
    id/0,
    id/1,
    identifier/0,
    identifier/1,
    optid/1,
    sign_key/0,
    sign_key_simple/0,
    number/0,
    number/1,
    start_link/0
]).

-record(state, {sign_key, sign_key_simple}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Return an unique id to be used in javascript or html.  No randomness, just unique in the cluster.
unique() -> 
    gen_server:call(?MODULE, unique).

%% @doc Return a long random id, can be used for session ids.
id() -> 
    gen_server:call(?MODULE, {id, ?ID_LENGTH}).

id(Len) -> 
    gen_server:call(?MODULE, {id, Len}).

%% @spec identifier() -> binary()
%% @doc Get a random indentifier of a certain length, case insensitive
identifier() -> 
    gen_server:call(?MODULE, {identifier, ?OPTID_LENGTH}).

identifier(Len) -> 
    gen_server:call(?MODULE, {identifier, Len}).

optid(undefined) -> identifier(?OPTID_LENGTH);
optid(false) -> identifier(?OPTID_LENGTH);
optid(Id) -> Id.

%% @spec sign_key() -> binary()
%% @doc Get the key for signing requests stored in the user agent.
sign_key() -> 
    gen_server:call(?MODULE, sign_key).


%% @spec sign_key_simple() -> binary()
%% @doc Get the key for less secure signing of data (without nonce).
sign_key_simple() -> 
    gen_server:call(?MODULE, sign_key_simple).
    

%% @doc Return a big random integer, but smaller than maxint32
number() ->
    gen_server:call(?MODULE, number).

number(Max) ->
    gen_server:call(?MODULE, {number, Max}).


init([]) ->
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),
    {ok, #state{}}.


handle_call(unique, _From, State) ->
    Id = make_unique(),
    {reply, Id, State};

handle_call(number, _From, State) ->
    Number = random:uniform(1000000000), 
    {reply, Number, State};

handle_call({number, Max}, _From, State) ->
    Number = random:uniform(Max), 
    {reply, Number, State};

handle_call({identifier, Len}, _From, State) ->
    Id = generate_identifier(Len),
    {reply, Id, State};

handle_call({id, Len}, _From, State) ->
    Id = generate_id(Len),
    {reply, Id, State};

handle_call(sign_key, _From, State) ->
    case State#state.sign_key of
        undefined ->
            SKey = case os:getenv("ZOPHRENIC_SIGN_KEY") of false -> generate_id(50); K -> K end,
            Key  = list_to_binary(SKey),
            {reply, Key, State#state{sign_key=Key}};
        Key -> 
            {reply, Key, State}
    end;

handle_call(sign_key_simple, _From, State) ->
    case State#state.sign_key_simple of
        undefined ->
            SKey = case os:getenv("ZOPHRENIC_SIGN_KEY_SIMPLE") of false -> generate_id(10); K -> K end,
            Key  = list_to_binary(SKey),
            {reply, Key, State#state{sign_key_simple=Key}};
        Key -> 
            {reply, Key, State}
    end;

handle_call(Msg, _From, State) ->
    {stop, {unknown_call, Msg}, State}.


handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVersion, State, _Extra) -> {ok, State}.



%% @doc Create an unique temporary id, safe to use in html and javascript
make_unique() ->
    Ref = lists:flatten(io_lib:format("~p",[make_ref()])),
    "t" ++ unique1(Ref, []).

unique1([], Acc) -> Acc;
unique1([$.|T], Acc) -> 
    unique1(T, [$_|Acc]);
unique1([H|T], Acc) when H >= $0 andalso H =< $9 -> 
    unique1(T, [H|Acc]);
unique1([_|T], Acc) ->
    unique1(T, Acc).


%% @spec generate_id(int()) -> string()
%% @doc Generate a random key
generate_id(Len) ->
    generate_id(Len, []).

generate_id(0, Key) ->
    Key;
generate_id(Len, Key) ->
    Char = case random:uniform(62) of
                C when C =< 26 -> C - 1  + $a;
                C when C =< 52 -> C - 27 + $A;
                C -> C - 53 + $0
            end,
    generate_id(Len-1, [Char|Key]).


%% @spec generate_identifier(int()) -> string()
%% @doc Generate a random identifier, case insensitive, only letters
generate_identifier(Len) ->
    generate_identifier(Len, []).

generate_identifier(0, Key) ->
    Key;
generate_identifier(Len, Key) ->
    generate_identifier(Len-1, [random:uniform(26)-1+$a|Key]).

