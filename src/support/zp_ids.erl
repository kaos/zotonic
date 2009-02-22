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
-export([unique/0, id/0, id/1, optid/1, number/0, number/1, start_link/0]).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Return an unique id to be used in javascript or html.  No randomness, just unique in the cluster.
unique() -> 
    gen_server:call(?MODULE, unique).

%% @doc Return a long random id, can be used for session ids.
id() -> 
    gen_server:call(?MODULE, {id, ?ID_LENGTH}).

id(Len) -> 
    gen_server:call(?MODULE, {id, Len}).

optid(undefined) -> id(?OPTID_LENGTH);
optid(false) -> id(?OPTID_LENGTH);
optid(Id) -> Id.

%% @doc Return a big random integer, but smaller than maxint32
number() ->
    gen_server:call(?MODULE, number).

number(Max) ->
    gen_server:call(?MODULE, {number, Max}).


init([]) ->
    {A1,A2,A3} = erlang:now(),
    random:seed(A1, A2, A3),
    {ok, true}.


handle_call(unique, _From, State) ->
    Id = make_unique(),
    {reply, Id, State};

handle_call(number, _From, State) ->
    Number = random:uniform(1000000000), 
    {reply, Number, State};

handle_call({number, Max}, _From, State) ->
    Number = random:uniform(Max), 
    {reply, Number, State};

handle_call({id, Len}, _From, State) ->
    Id = generate_id(Len),
    {reply, Id, State}.

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
%% @doc Generate a random session key, stored in a cookie in the user agent
generate_id(Len) ->
    generate_id(Len, []).

generate_id(0, Key) ->
    Key;
generate_id(Len, Key) ->
    generate_id(Len-1, [random:uniform(26)+96|Key]).

