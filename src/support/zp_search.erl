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
    search/3
]).

-include_lib("zophrenic.hrl").

-define(OFFSET_LIMIT, {1,10}).

search(Search, Context) ->
    search(Search, ?OFFSET_LIMIT, Context).

%% @doc Perform the named search and its arguments
%% @spec search({Name, SearchPropList}, Context) -> #search_result
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


concat_sql_query(#search_sql{select=Select, from=From, where=Where, order=Order, limit=Limit, args=Args}, {OffsetN, LimitN}) ->
    Where1 = case Where of
        [] -> [];
        _ -> "where " ++ Where
    end,
    Order1 = case Order of
        [] -> [];
        _ -> "order by "++Order
    end,
    {Parts, FinalArgs} = case Limit of
        undefined ->
            N = length(Args),
            Args1 = Args ++ [OffsetN-1, LimitN],
            {["select", Select, "from", From, Where1, Order1, "offset", [$$|integer_to_list(N+1)], "limit", [$$|integer_to_list(N+2)]], Args1};
        _ ->
            {["select", Select, "from", From, Where1, Order1, Limit], Args}
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
    case zp_access_control:can_see(Context) of
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
            % can see published community and public content or any content from one of the user's groups
            % TODO: Need to check the groups of the user
            {[], Args}
    end.
    
