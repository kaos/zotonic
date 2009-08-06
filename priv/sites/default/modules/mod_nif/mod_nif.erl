%% @authorTim Benniks <tim@timbenniks.nl>
%% @copyright 2009 Tim Benniks
%% @date 2009-17-08
%%
%% @doc Module implementing the New Island Festival

-module(mod_nif).
-author("Tim Benniks <tim@timbenniks.nl>").
-behaviour(gen_server).

-mod_title("New Island Festival").
-mod_description("The website of the New Island Festival.").
-mod_prio(100).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    observe/2
]).

-include_lib("zotonic.hrl").

-record(state, {context}).

observe({search_query, Req, OffsetLimit}, Context) ->
    search(Req, OffsetLimit, Context).

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
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    z_notifier:observe(search_query, {?MODULE, observe}, Context),
    {ok, #state{context=z_context:new_for_host(Context)}}.

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
    z_notifier:detach(search_query, {?MODULE, observe}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================



search({nif_program, [{day, Day}, {genre, Genre}]}, _OffetLimit, Context) ->
    DateStart = {{2009,9,Day}, {4,0,0}},
    DateEnd   = {{2009,9,Day+1}, {4,0,0}},
    case Genre of
        Empty when Empty == []; Empty == undefined ->
            #search_sql{
                select="r.id, r.pivot_date_start",
                from="rsc r",
                where="pivot_date_start <= $1 and pivot_date_end >= $2",
                order="r.pivot_date_start asc",
                tables=[{rsc,"r"}],
                args=[DateEnd, DateStart],
                cats=[{"r", event}]
            };
        _ ->
            PredHasGenreId = m_predicate:name_to_id_check(hasgenre, Context),
            InIds = string:join([ integer_to_list(N) || N <- Genre], ","),
            #search_sql{
                select="distinct r.id, r.pivot_date_start",
                from="rsc r, edge e",
                where="r.id = e.subject_id and e.predicate_id = $1 and e.object_id in (" ++ InIds
                    ++ ") and pivot_date_start <= $2 and pivot_date_end >= $3"
                    ,
                order="r.pivot_date_start asc",
                tables=[{rsc,"r"}],
                args=[PredHasGenreId, DateEnd, DateStart],
                cats=[{"r", event}]
            }
    end;

search({nif_artist_upcoming, [{id, Id}]}, _OffetLimit, Context) ->
    %% It should be upcoming from today, which is today in NY.
    {Date,{H,_,_}} = calendar:universal_time(),
    Date1 = case H of 
        Hr when Hr < 6 ->
            %% Take 6 hours time diff to be sure
            calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) - 1);
        _ -> 
            Date
    end,
            
    PredPerfomerId = m_predicate:name_to_id_check(performer, Context),
    #search_sql{
        select="r.id",
        from="rsc r, edge e",
        where="r.id = e.subject_id and e.predicate_id = $1 and e.object_id = $2
            and r.pivot_date_end >= $3"
            ,
        order="r.pivot_date_start asc",
        tables=[{rsc,"r"}],
        args=[PredPerfomerId, Id, Date1],
        cats=[{"r", event}]
    };

search({nif_artist_events, [{id, Id}]}, _OffetLimit, Context) ->
    PredPerfomerId = m_predicate:name_to_id_check(performer, Context),
    #search_sql{
        select="r.id",
        from="rsc r, edge e",
        where="r.id = e.subject_id and e.predicate_id = $1 and e.object_id = $2",
        order="r.pivot_date_start asc",
        tables=[{rsc,"r"}],
        args=[PredPerfomerId, Id],
        cats=[{"r", event}]
    };

search(_, _, _) ->
    undefined.

