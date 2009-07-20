%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-06-09
%%
%% @doc SQL queries for the shop module.

-module(shop_queries).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    search/3
]).

-include("zotonic.hrl").


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
                args=[CatId, QueryText, z_pivot_rsc:pg_lang(Context#context.language), PredId],
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
                args=[CatId, QueryText, z_pivot_rsc:pg_lang(Context#context.language), PredId, BrandId],
                tables=[{rsc,"r"}]
            }
    end;


%% Return all orders
search({shop_order_list, []}, _OffsetLimit, _Context) ->
    #search_sql{
        select="o.*",
        from="shop_order o",
        order="o.id desc",
        tables=[],
        args=[],
        assoc=true
    };

%% Return all skus
search({shop_sku_list, [{text,QueryText}]}, _OffsetLimit, Context) ->
    case QueryText of
        A when A == undefined orelse A == "" ->
            #search_sql{
                select="sku.*",
                from="shop_sku sku",
                order="sku.article_nr",
                tables=[],
                args=[],
                assoc=true
            };
        _ ->
            #search_sql{
                select="sku.*, ts_rank_cd(sku.tsv, query, 32) AS rank",
                from="shop_sku sku, plainto_tsquery($2, $1) query",
                where="query @@ sku.tsv",
                order="rank desc",
                tables=[],
                args=[QueryText, z_pivot_rsc:pg_lang(Context#context.language)],
                assoc=true
            }
    end;

%% Return the top 10 best selling products in the last two weeks
search({shop_best_selling, []}, _OffsetLimit, _Context) ->
    #search_sql{
        select="sku.rsc_id, count(*) as ct",
        from="shop_order o, shop_order_line ol, shop_sku sku",
        where="     ol.shop_sku_id = sku.id 
                and ol.shop_order_id = o.id 
                and o.paid 
                and o.created >= now() - interval '14 days'",
        group_by="sku.rsc_id",
        order="ct desc",
        tables=[],
        args=[]
    };

%% Return the latest sold products
search({shop_latest_sold, []}, _OffsetLimit, _Context) ->
    #search_sql{
        select="sku.rsc_id, sku.variant, ol.price_incl, o.created, o.id",
        from="shop_order o, shop_order_line ol, shop_sku sku",
        where="     ol.shop_sku_id = sku.id 
                and ol.shop_order_id = o.id 
                and o.paid",
        order="o.created desc",
        tables=[],
        args=[]
    };
    
search(_, _, _) ->
    undefined.


