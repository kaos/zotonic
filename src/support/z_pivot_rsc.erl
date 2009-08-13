%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-17
%%
%% @doc Pivoting server for the rsc table. Takes care of full text indices. Polls the pivot queue for any changed resources.
%% @todo Support for multiple databases (now only supports the default database).

-module(z_pivot_rsc).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    poll/0,
    poll/1,
    pivot/2,
    
    pivot_resource/2,
    pg_lang/1
]).

-include("zotonic.hrl").

% Interval (in seconds) to check if there are any items to be pivoted.
-define(PIVOT_POLL_INTERVAL, 10).

% Number of queued ids taken from the queue at one go
-define(POLL_BATCH, 100).

-record(state, {timer}).



%% @doc Poll all databases, periodically called by a timer
%% @spec poll() -> void()
poll() ->
    gen_server:cast(?MODULE, {poll}).

%% @doc Poll the pivot queue for the database in the context
%% @spec poll(Context) -> void()
poll(Context) ->
    Host = ?HOST(Context),
    gen_server:cast(?MODULE, {poll, Host}).


%% @doc An immediate pivot request for a resource
%% @spec pivot(Id, Context) -> void()
pivot(Id, Context) ->
    Host = ?HOST(Context),
    gen_server:cast(?MODULE, {pivot, Id, Host}).


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() -> 
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(_Args) ->
    Timer = timer:apply_interval(?PIVOT_POLL_INTERVAL * 1000, ?MODULE, poll, []),
    {ok, #state{timer=Timer}}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% Poll the queue for the default host
%% @todo Poll for all hosts in the sites dir (when we have multiple pools)
handle_cast({poll}, State) ->
    do_poll(z_context:site()),
    {noreply, State};

%% Poll the queue for a particular database
handle_cast({poll, Host}, State) ->
    do_poll(Host),
    {noreply, State};


%% Poll the queue for a particular database
handle_cast({pivot, Id, Host}, State) ->
    do_pivot(Id, Host),
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
terminate(_Reason, State) ->
    timer:cancel(State#state.timer),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

%% @doc Poll a database for any queued updates.
do_poll(Host) ->
    Context = z_context:new_for_host(Host),
    Qs = fetch_queue(Context),
    F = fun(Ctx) ->
        [ pivot_resource(Id, Ctx) || {Id,_Serial} <- Qs]
    end,
    z_db:transaction(F, Context),
    delete_queue(Qs, Context).

%% @doc Pivot a specific id, delete its queue record if present
do_pivot(Id, Host) ->
    Context = z_context:new_for_host(Host),
    Serial = fetch_queue_id(Id, Context),
    pivot_resource(Id, Context),
    delete_queue(Id, Serial, Context).


%% @doc Fetch the next batch of ids from the queue. Remembers the serials, as a new
%% pivot request might come in while we are pivoting.
%% @spec fetch_queue(Context) -> [{Id,Serial}, ...]
fetch_queue(Context) ->
    z_db:q("select rsc_id, serial from rsc_pivot_queue order by is_update, due limit $1", [?POLL_BATCH], Context).

%% @doc Fetch the serial of id's queue record
fetch_queue_id(Id, Context) ->
    z_db:q1("select serial from rsc_pivot_queue where id = $1", [Id], Context).

%% @doc Delete the previously queued ids iff the queue entry has not been updated in the meanwhile
delete_queue(Qs, Context) ->
    F = fun(Ctx) ->
        [ z_db:q("delete from rsc_pivot_queue where rsc_id = $1 and serial = $2", [Id,Serial], Ctx) || {Id,Serial} <- Qs ]
    end,
    z_db:transaction(F, Context).

%% @doc Delete a specific id/serial combination
delete_queue(_Id, undefined, _Context) ->
    ok;
delete_queue(Id, Serial, Context) ->
    z_db:q("delete from rsc_pivot_queue where id = $1 and serial = $2", [Id,Serial], Context).



%% @doc Pivot a resource, collect all texts for indexing and some extra to be indexed fields.
%% @todo Also add the property tag/values
%% @spec pivot(Id, Context) -> void()
pivot_resource(Id, Context) ->
    R = m_rsc:get(Id, Context),
    {A,B} = lists:foldl(fun fetch_texts/2, {[],[]}, R),
    {ObjIds, ObjTexts} = related(Id, Context),
    {CatIds, CatTexts} = category(proplists:get_value(category_id, R), Context),
    Split = [ (split_lang(Ts, Context)) || Ts <- [A, B, CatTexts, ObjTexts] ],
    [TA,TB,TC,TD] = [ [ {Lng,list_to_binary(z_utils:combine(32, Ts))} || {Lng,Ts} <- Ps] || Ps <- Split ],

    {SqlA, ArgsA} = to_tsv(TA, $A, []),
    {SqlB, ArgsB} = to_tsv(TB, $B, ArgsA),
    {SqlC, ArgsC} = to_tsv(TC, $C, ArgsB),
    {SqlD, ArgsD} = to_tsv(TD, $D, ArgsC),

    TsvSql = [SqlA, " || ", SqlB, " || ", SqlC, " || ", SqlD],

    TsvObj = [ [" zpo",integer_to_list(OId)] || OId <- ObjIds ],
    TsvCat = [ [" zpc",integer_to_list(CId)] || CId <- CatIds ],
    TsvIds = list_to_binary([TsvObj,TsvCat]),

    {DateStart, DateEnd} = pivot_date(R),
    
    N = length(ArgsD),
    Sql = list_to_binary([
            "update rsc set pivot_tsv = ",TsvSql,
            ", pivot_rtsv     = to_tsvector($",integer_to_list(N+1),")",
            ", pivot_street   = $",integer_to_list(N+2),
            ", pivot_city     = $",integer_to_list(N+3),
            ", pivot_postcode = $",integer_to_list(N+4),
            ", pivot_state    = $",integer_to_list(N+5),
            ", pivot_country  = $",integer_to_list(N+6),
            ", pivot_first_name=$",integer_to_list(N+7),
            ", pivot_surname  = $",integer_to_list(N+8),
            ", pivot_gender   = $",integer_to_list(N+9),
            ", pivot_date_start= $",integer_to_list(N+10),
            ", pivot_date_end  = $",integer_to_list(N+11),
            " where id = $",integer_to_list(N+12)
        ]),
    SqlArgs = ArgsD ++ [
        TsvIds,
        proplists:get_value(street, R),
        proplists:get_value(city, R),
        proplists:get_value(postcode, R),
        proplists:get_value(state, R),
        proplists:get_value(country, R),
        proplists:get_value(first_name, R),
        proplists:get_value(surname, R),
        proplists:get_value(gender, R),
        DateStart,
        DateEnd,
        Id
    ],
    z_db:q(Sql, SqlArgs, Context).


    %% Make the setweight(to_tsvector()) parts of the update statement
    to_tsv([], _Level, Args) -> {"''", Args};
    to_tsv(List, Level, Args) -> 
        {Sql1, Args1} = lists:foldl(fun ({Lang,Text}, {Sql, As}) -> 
                N   = length(As) + 1,
                As1 = As ++ [Text],
                {[["setweight(to_tsvector('pg_catalog.",pg_lang(Lang),"', $",integer_to_list(N),"), '",Level,"')"] | Sql], As1}
            end, {[], Args}, List),
    
        {z_utils:combine(" || ", Sql1), Args1}.
            
    
    %      new.tsv := 
    %        setweight(to_tsvector('pg_catalog.dutch', coalesce(new.title_nl,'')), 'A') || 
    %        setweight(to_tsvector('pg_catalog.dutch', coalesce(new.desc_nl,'')),  'D') ||
    %        setweight(to_tsvector('pg_catalog.english', coalesce(new.title_en,'')), 'A') || 
    %        setweight(to_tsvector('pg_catalog.english', coalesce(new.desc_en,'')),  'D'); 

%% @doc Fetch the date range from the record
pivot_date(R) ->
    DateStart = proplists:get_value(date_start, R),
    DateEnd   = proplists:get_value(date_end, R),
    pivot_date1(DateStart, DateEnd).

    pivot_date1(S, E) when not is_tuple(S) andalso not is_tuple(E) ->
        {undefined, undefined};
    pivot_date1(S, E) when not is_tuple(S) andalso is_tuple(E) ->
        { {{-4000,0,0},{0,0,0}}, E};
    pivot_date1(S, E) when is_tuple(S) andalso not is_tuple(E) ->
        {S, ?ST_JUTTEMIS};
    pivot_date1(S, E) when is_tuple(S) andalso is_tuple(E) ->
        {S, E}.


%% @doc Split texts into different languages
split_lang(Texts, Context) ->
    Dict = split_lang(Texts, dict:new(), Context),
    dict:to_list(Dict).
    
split_lang([], Dict, _Context) -> Dict;
split_lang([{trans, Texts}|Rest], Dict, Context) ->
    Dict2 = lists:foldl(fun({Lang,Text}, D) -> add_lang(Lang, z_html:strip(Text), D) end, Dict, Texts),
    split_lang(Rest, Dict2, Context);
split_lang([Text|Rest], Dict, Context) ->
    Dict2 = add_lang(z_context:language(Context), Text, Dict),
    split_lang(Rest, Dict2, Context).

    add_lang(Lang, Text, Dict) ->
        case dict:find(Lang, Dict) of
            {ok, _} -> dict:append(Lang, z_html:strip(Text), Dict);
            error -> dict:store(Lang, [z_html:strip(Text)], Dict)
        end.
                

%% @doc Fetch the title of all things related to the resource
related(Id, Context) ->
    Ids = m_edge:objects(Id, Context),
    Texts = [ m_rsc:p(R, title, Context) || R <- Ids ],
    {Ids, Texts}.
    

%% @doc Fetch the names of all categories in the category path
%% @spec category(int(), Context) -> { IdList, TextsList }
category(CatId, Context) ->
    Names = [ z_convert:to_list(Name) || Name <- m_category:is_a(CatId, Context) ],
    Ids   = [ CatId |  m_category:get_path(CatId, Context) ],
    {Ids, Names}.


fetch_texts({title, Value}, {A,B}) ->
    {[Value|A], B};
fetch_texts({subtitle, Value}, {A,B}) ->
    {[Value|A], B};
fetch_texts({surname, Value}, {A,B}) ->
    {[Value|A], B};
fetch_texts({first_name, Value}, {A,B}) ->
    {[Value|A], B};
fetch_texts({given_name, Value}, {A,B}) ->
    {[Value|A], B};
fetch_texts({F, Value}, {A,B}) when is_binary(Value) ->
    case do_pivot_field(F) of
        false -> {A,B};
        true -> {A, [Value|B]}
    end;
fetch_texts({F, {{Y,M,D},{H,Min,S}} = Date}, {A,B} = Acc)
    when is_integer(Y) andalso is_integer(M) andalso is_integer(D) 
        andalso is_integer(H) andalso is_integer(Min) andalso is_integer(S) ->
    case do_pivot_field(F) of
        false -> Acc;
        true -> {A, [erlydtl_dateformat:format(Date, "Y m d H i s F l h")|B]}
    end;
fetch_texts({_, {trans, _} = V}, {A,B}) ->
    {A, [V|B]};
fetch_texts({F, V}, {A,B} = Acc) ->
    case do_pivot_field(F) of
        false -> Acc;
        true ->
            case z_string:is_string(V) of
                true -> {A, [V|B]};
                false -> Acc
            end
    end.

% Suppress some fields that are only for supporting the pivoting
do_pivot_field(pivot_category_nr) -> false; 
do_pivot_field(pivot_tsv) -> false; 
do_pivot_field(pivot_rtsv) -> false; 
do_pivot_field(pivot_first_name) -> false; 
do_pivot_field(pivot_surname) -> false; 
do_pivot_field(pivot_gender) -> false; 
do_pivot_field(pivot_date_start) -> false; 
do_pivot_field(pivot_date_end) -> false; 
do_pivot_field(pivot_date_start_month_day) -> false; 
do_pivot_field(pivot_date_end_month_day) -> false; 
do_pivot_field(pivot_street) -> false; 
do_pivot_field(pivot_city) -> false; 
do_pivot_field(pivot_state) -> false; 
do_pivot_field(pivot_postcode) -> false; 
do_pivot_field(pivot_country) -> false; 
do_pivot_field(pivot_geocode) -> false; 
do_pivot_field(uri) -> false; 
do_pivot_field(publication_start) -> false; 
do_pivot_field(publication_end) -> false; 
do_pivot_field(created) -> false; 
do_pivot_field(modified) -> false; 
do_pivot_field(_) -> true.


%% @doc Translate a language to a language string as used by postgresql
%% @todo Add more languages
pg_lang(en) -> "english";
pg_lang(nl) -> "dutch";
pg_lang(de) -> "german";
pg_lang(fr) -> "french";
pg_lang(_) -> "english".


