%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-06-09
%%
%% @doc Defines PostgreSQL queries for basic content searches in Zophrenic.
%% This module needs to be split in specific PostgreSQL queries and standard SQL queries when you want to 
%% support other databases (like MySQL).

-module(mod_search).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Search Queries").
-mod_description("Defines PostgreSQL queries for basic content searches in Zophrenic.").
-mod_prio(1000).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

%% interface functions
-export([
    observe/2, to_tsquery/2
]).

-include("zophrenic.hrl").

-record(state, {context}).


observe({search_query, Req, OffsetLimit}, Context) ->
    search(Req, OffsetLimit, Context).



%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
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
    zp_notifier:observe(search_query, {?MODULE, observe}, Context),
    {ok, #state{context=zp_context:prune_for_database(Context)}}.

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
    zp_notifier:detach(search_query, {?MODULE, observe}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================



%% @doc Return a list of resource ids, featured ones first
%% @spec search(SearchSpec, Range, Context) -> #search_sql{}
search({featured, []}, _OffsetLimit, _Context) ->
    #search_sql{
        select="r.id",
        from="rsc r",
        order="r.is_featured desc, r.id desc",
        tables=[{rsc,"r"}]
    };

%% @doc Return a list of resource ids inside a category, featured ones first
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({featured, [{cat, Cat}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    #search_sql{
        select="r.id",
        from="rsc r, category rc, category ic",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1",
        order="r.is_featured desc, r.id desc",
        args=[CatId],
        tables=[{rsc,"r"}]
    };

%% @doc Return a list of featured resource ids inside a category having a object_id as predicate
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({featured, [{cat,Cat},{object,ObjectId},{predicate,Predicate}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    PredId = m_predicate:name_to_id_check(Predicate, Context),
    #search_sql{
        select="r.id",
        from="rsc r, category rc, category ic, edge e",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1
                and r.id = e.subject_id and e.predicate_id = $2 and e.object_id = $3",
        order="r.is_featured desc, r.id desc",
        args=[CatId, PredId, ObjectId],
        tables=[{rsc,"r"}]
    };

search({latest, [{cat, Cat}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    #search_sql{
        select="r.id",
        from="rsc r, category rc, category ic",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1",
        order="r.modified desc, r.id desc",
        args=[CatId],
        tables=[{rsc,"r"}]
    };

search({upcoming, [{cat, Cat}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    #search_sql{
        select="r.id",
        from="rsc r, category rc, category ic",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1
            and pivot_date_end >= current_date",
        order="r.pivot_date_start asc",
        args=[CatId],
        tables=[{rsc,"r"}]
    };

search({autocomplete, [{text,QueryText}]}, _OffsetLimit, Context) ->
    TsQuery = to_tsquery(QueryText, Context),
    case TsQuery of
        A when A == undefined orelse A == [] ->
            #search_result{};
        _ ->
            #search_sql{
                select="r.id, ts_rank_cd(pivot_tsv, query, 32) AS rank",
                from="rsc r, to_tsquery($2, $1) query",
                where=" query @@ pivot_tsv",
                order="rank desc",
                args=[TsQuery, zp_pivot_rsc:pg_lang(Context#context.language)],
                tables=[{rsc,"r"}]
            }
    end;

search({fulltext, [{cat,Cat},{text,QueryText}]}, OffsetLimit, Context) when Cat == undefined orelse Cat == [] orelse Cat == <<>> ->
    search({fulltext, [{text,QueryText}]}, OffsetLimit, Context);

search({fulltext, [{text,QueryText}]}, _OffsetLimit, Context) ->
    case QueryText of
        A when A == undefined ->
            #search_sql{
                select="r.id, 1 AS rank",
                from="rsc r",
                order="r.modified desc",
                args=[],
                tables=[{rsc,"r"}]
            };
        _ ->
            #search_sql{
                select="r.id, ts_rank_cd(pivot_tsv, query, 32) AS rank",
                from="rsc r, plainto_tsquery($2, $1) query",
                where=" query @@ pivot_tsv",
                order="rank desc",
                args=[QueryText, zp_pivot_rsc:pg_lang(Context#context.language)],
                tables=[{rsc,"r"}]
            }
    end;

search({fulltext, [{cat,Cat},{text,QueryText}]}, _OffsetLimit, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} -> 
            case QueryText of
                A when A == undefined orelse A == "" orelse A == <<>> ->
                    #search_sql{
                        select="r.id, 1 AS rank",
                        from="rsc r, category rc, category ic",
                        where=" r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1",
                        order="r.modified desc",
                        args=[CatId],
                        tables=[{rsc,"r"}]
                    };
                _ ->
                    #search_sql{
                        select="r.id, ts_rank_cd(pivot_tsv, query, 32) AS rank",
                        from="rsc r, category rc, category ic, plainto_tsquery($3, $2) query",
                        where=" query @@ pivot_tsv  and r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1",
                        order="rank desc",
                        args=[CatId, QueryText, zp_pivot_rsc:pg_lang(Context#context.language)],
                        tables=[{rsc,"r"}]
                    }
            end;
        _ ->
            #search_result{result=[]}
    end;

search({referrers, [{id,Id}]}, _OffsetLimit, _Context) ->
    #search_sql{
        select="o.id, e.predicate_id",
        from="edge e join rsc o on o.id = e.subject_id",
        where="e.object_id = $1",
        order="e.id desc",
        args=[Id],
        tables=[{rsc,"o"}]
    };

search({media_category_image, [{cat,Cat}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    #search_sql{
        select="m.filename",
        from="rsc r, category rc, category ic, medium m",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1
                and m.rsc_id = r.id",
        tables=[{rsc,"r"}, {medium, "m"}],
        args=[CatId]
    };

search({media_category_depiction, [{cat,Cat}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    PredDepictionId = m_predicate:name_to_id_check(depiction, Context),
    #search_sql{
        select="m.filename",
        from="rsc r, rsc ro, category rc, category ic, medium m, edge e",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1
                and ro.id = e.object_id and e.subject_id = r.id and e.predicate_id = $2 and ro.id = m.rsc_id",
        tables=[{rsc,"r"}, {rsc, "ro"}, {medium, "m"}],
        args=[CatId, PredDepictionId]
    };


search({media, []}, _OffsetLimit, _Context) ->
    #search_sql{
        select="m.*",
        from="media m",
        tables=[{medium, "m"}],
        order="m.created desc",
        args=[],
        assoc=true
    };

search(_, _, _) ->
    undefined.




%% @doc Expand a search string like "hello wor" to "'hello' & 'wor:*'"
to_tsquery(undefined, _Context) ->
    [];
to_tsquery(Text, Context) when is_binary(Text) ->
    to_tsquery(binary_to_list(Text), Context);
to_tsquery(Text, Context) ->
    [{TsQuery, Version}] = zp_db:q("
        select plainto_tsquery($2, $1) , version()
    ", [Text, zp_pivot_rsc:pg_lang(zp_context:language(Context))], Context),
    % Version is something like "PostgreSQL 8.3.5 on i386-apple-darwin8.11.1, compiled by ..."
    case TsQuery of
        [] -> 
            [];
        _ ->
            case Version < <<"PostgreSQL 8.4">> of
                true ->
                    TsQuery;
                false ->
                    % Replace the last ' with :*' 
                    N = length(TsQuery),
                    {A, "'"} = lists:split(N-1, TsQuery),
                    A ++ ":*'"
            end
    end.

