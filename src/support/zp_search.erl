%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-15
%%
%% @doc Search the database, interfaces to specific search routines.

-module(zp_search).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    search/2,
    search/3,
    search_pager/3,
    search_pager/4,
    pager/3,
    pager/4
]).

-include_lib("zophrenic.hrl").

-define(OFFSET_LIMIT, {1,?SEARCH_PAGELEN}).
-define(OFFSET_PAGING, {1,1000}).

%% @doc Search items and handle the paging.  Uses the default page length.
%% @spec search({Name, SearchPropList}, Page, #context) -> #search_result
search_pager(Search, Page, Context) ->
    search_pager(Search, Page, ?SEARCH_PAGELEN, Context).

%% @doc Search items and handle the paging
%% @spec search_pager({Name, SearchPropList}, Page, PageLen, #context) -> #search_result
search_pager(Search, Page, PageLen, Context) ->
    SearchResult = search(Search, ?OFFSET_PAGING, Context),
    pager(SearchResult, Page, PageLen, Context).


pager(#search_result{pagelen=undefined} = SearchResult, Page, Context) ->
    pager(SearchResult, Page, ?SEARCH_PAGELEN, Context);
pager(SearchResult, Page, Context) ->
    pager(SearchResult, Page, SearchResult#search_result.pagelen, Context).

pager(#search_result{result=Result} = SearchResult, Page, PageLen, _Context) ->
    Total = length(Result),
    Pages = mochinum:int_ceil(Total / PageLen), 
    Offset = (Page-1) * PageLen + 1,
    OnPage = case Offset =< Total of
        true ->
            {P,_} = zp_utils:split(PageLen, lists:nthtail(Offset-1, Result)),
            P;
        false ->
            []
    end,
    Next = if Offset + PageLen < Total -> false; true -> Page+1 end,
    Prev = if Page > 1 -> Page-1; true -> 1 end,
    SearchResult#search_result{
        result=OnPage,
        all=Result,
        total=Total,
        page=Page,
        pagelen=PageLen,
        pages=Pages,
        next=Next,
        prev=Prev
    }.

%% @doc Search with the question and return the results
%% @spec search({Name, SearchPropList}, #context) -> #search_result
search(Search, Context) ->
    search(Search, ?OFFSET_LIMIT, Context).

%% @doc Perform the named search and its arguments
%% @spec search({Name, SearchPropList}, {Offset, Limit}, Context) -> #search_result
search({SearchName, Props}, Limit, Context) ->
    % todo: fetch paging information from props
    PropsSorted = lists:keysort(1, Props),
    Result = search:search({SearchName, PropsSorted}, Limit, Context),
    search_result(Result, Limit, Context);
search(Name, Limit, Context) ->
    search({zp_convert:to_atom(Name), []}, Limit, Context).


%% @doc Handle a return value from a search function.  This can be an intermediate SQL statement that still needs to be
%% augmented with extra ACL checks.
%% @spec search_result(Result, Limit, Context) -> #search_result
search_result(#search_result{} = S, _Limit, _Context) ->
    S;
search_result(#search_sql{} = Q, Limit, Context) ->
    Q1 = reformat_sql_query(Q, Context),
    {Sql, Args} = concat_sql_query(Q1, Limit),
    case Q#search_sql.run_func of
        F when is_function(F) ->
            F(Q, Sql, Args, Context);
        _ -> 
            Rows = zp_db:q(Sql, Args, Context),
            Rows1 = case Rows of
                [{_}|_] -> [ R || {R} <- Rows ];
                _ -> Rows
            end,
            #search_result{result=Rows1}
    end.


concat_sql_query(#search_sql{select=Select, from=From, where=Where, group_by=GroupBy, order=Order, limit=Limit, args=Args}, {OffsetN, LimitN}) ->
    Where1 = case Where of
        [] -> [];
        _ -> "where " ++ Where
    end,
    Order1 = case Order of
        [] -> [];
        _ -> "order by "++Order
    end,
    GroupBy1 = case GroupBy of
        [] -> [];
        _ -> "group by "++GroupBy
    end,
    {Parts, FinalArgs} = case Limit of
        undefined ->
            N = length(Args),
            Args1 = Args ++ [OffsetN-1, LimitN],
            {["select", Select, "from", From, Where1, GroupBy1, Order1, "offset", [$$|integer_to_list(N+1)], "limit", [$$|integer_to_list(N+2)]], Args1};
        _ ->
            {["select", Select, "from", From, Where1, GroupBy1, Order1, Limit], Args}
    end,
    {string:join(Parts, " "), FinalArgs}.
    

%% @doc Inject the ACL checks in the SQL query.
%% @spec reformat_sql_query(#search_sql, Context) -> #search_sql
reformat_sql_query(#search_sql{where=Where, tables=Tables, args=Args} = Q, Context) ->
    {ExtraWhere, Args1} = lists:foldl(fun(Table, {Acc,As}) ->
            {W,As1} = add_acl_check(Table, As, Context),
            {[W|Acc], As1}
        end, {[], Args}, Tables),
    Where1 = lists:flatten(concat_where(ExtraWhere, Where)),
    Q#search_sql{where=Where1, args=Args1}.


%% @doc Concatenate the where clause with the extra ACL checks using "and".  Skip empty clauses.
%% @spec concat_where(ClauseList, CurrentWhere) -> NewClauseList
concat_where([], Acc) -> 
    Acc;
concat_where([[]|Rest], Acc) -> 
    concat_where(Rest, Acc);
concat_where([W|Rest], []) -> 
    concat_where(Rest, [W]);
concat_where([W|Rest], Acc) ->
    concat_where(Rest, [W, " and "|Acc]).
    

%% @doc Create extra 'where' conditions for checking the access control
%% @spec add_acl_check({Table, Alias}, Args, Context) -> {Where, NewArgs}
add_acl_check({rsc, Alias}, Args, Context) ->
    add_acl_check1(rsc, Alias, Args, Context);
add_acl_check({media, Alias}, Args, Context) ->
    add_acl_check1(media, Alias, Args, Context);
add_acl_check(_, Args, _Context) ->
    {[], Args}.


%% @doc Create extra 'where' conditions for checking the access control
%% @spec add_acl_check1(Table, Alias, Args, Context) -> {Where, NewArgs}
add_acl_check1(Table, Alias, Args, Context) ->
    % N = length(Args),
    case zp_acl:can_see(Context) of
        ?ACL_VIS_GROUP ->
            % Admin or supervisor, can see everything
            {[], Args};
        ?ACL_VIS_PUBLIC -> 
            % Anonymous users can only see public published content
            Sql = Alias ++ ".visible_for = 0",
            Sql1 = case Table of
                rsc ->
                    Sql++" and "
                    ++Alias++".is_published and "
                    ++Alias++".publication_start <= now() and "
                    ++Alias++".publication_end >= now()";
                _ ->
                    Sql
            end,
            {Sql1, Args};
        ?ACL_VIS_COMMUNITY ->
            % Can see published community and public content or any content from one of the user's groups
            Sql = Alias ++ ".visible_for in (0,1) ",
            Sql1 = case Table of
                rsc ->
                    Sql++" and "
                    ++Alias++".is_published and "
                    ++Alias++".publication_start <= now() and "
                    ++Alias++".publication_end >= now()";
                _ ->
                    Sql
            end,
            N = length(Args),
            Sql2 = "((" ++ Sql1 ++ ") or rsc_id = $"++integer_to_list(N+1)
                    ++" or group_id in (select rg.group_id from rsc_group rg where rg.rsc_id = $"++integer_to_list(N+1)++"))",
            {Sql2, Args ++ [Context#context.user_id]}
    end.
    
