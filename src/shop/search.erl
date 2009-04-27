%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-12
%%
%% @doc 

-module(search).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    search/3
]).

-include_lib("zophrenic.hrl").


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
                        args=[CatId, zp_pivot_rsc:pg_lang(Context#context.language)],
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

search({fulltext_catbrand, [{cat,Cat},{text,QueryText}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    PredId = m_predicate:name_to_id_check(brand, Context),
    case QueryText of
        A when A == undefined orelse A == "" ->
            #search_result{result=[]};
        _ ->
            #search_sql{
                select="r.id, r.category_id, e.object_id, ts_rank_cd(pivot_tsv, query, 32) AS rank",
                from="rsc r left join edge e on r.id = e.subject_id and e.predicate_id = $4, category rc, category ic, plainto_tsquery($3, $2) query",
                where=" query @@ pivot_tsv  and r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1",
                order="rank desc",
                args=[CatId, QueryText, zp_pivot_rsc:pg_lang(Context#context.language), PredId],
                tables=[{rsc,"r"}]
            }
    end;

search({fulltext_catbrand_filter, [{brand,undefined},{cat,undefined},{text,QueryText}]}, _OffsetLimit, Context) ->
    search({fulltext_catbrand, [{cat,product},{text,QueryText}]}, _OffsetLimit, Context);

search({fulltext_catbrand_filter, [{brand,undefined},{cat,Cat},{text,QueryText}]}, _OffsetLimit, Context) ->
    search({fulltext_catbrand, [{cat,Cat},{text,QueryText}]}, _OffsetLimit, Context);

search({fulltext_catbrand_filter, [{brand,BrandId},{cat,undefined},{text,QueryText}]}, _OffsetLimit, Context) ->
    search({fulltext_catbrand_filter, [{brand,BrandId},{cat,product},{text,QueryText}]}, _OffsetLimit, Context);

search({fulltext_catbrand_filter, [{brand,BrandId},{cat,Cat},{text,QueryText}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    PredId = m_predicate:name_to_id_check(brand, Context),
    case QueryText of
        A when A == undefined orelse A == "" ->
            #search_result{result=[]};
        _ ->
            #search_sql{
                select="r.id, r.category_id, e.object_id, ts_rank_cd(pivot_tsv, query, 32) AS rank",
                from="rsc r left join edge e on r.id = e.subject_id and e.predicate_id = $4, category rc, category ic, plainto_tsquery($3, $2) query",
                where=" query @@ pivot_tsv and e.object_id = $5 and r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1",
                order="rank desc",
                args=[CatId, QueryText, zp_pivot_rsc:pg_lang(Context#context.language), PredId, BrandId],
                tables=[{rsc,"r"}]
            }
    end;

search({media_category_image, [{cat,Cat}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    #search_sql{
        select="m.filename",
        from="rsc r, category rc, category ic, rsc_media rm, media m",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1
                and rm.rsc_id = r.id and rm.media_id = m.id",
        tables=[{rsc,"r"}, {media, "m"}],
        args=[CatId]
    }.




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

