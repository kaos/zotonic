%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_search).
-author("Tim Benniks <tim@timbenniks.com>").

-export([]).

-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->
    CatId = case zp_context:get_q("qcat", Context) of
        undefined -> undefined;
        Cat ->
            case m_category:name_to_id(Cat, Context) of
                {ok, CId} -> CId;
                _ -> undefined
            end
    end,
    BrandId = case zp_context:get_q("qbrand", Context) of
        undefined -> undefined;
        Brand ->
            case m_rsc:name_to_id(Brand, Context) of
                {ok, BId} -> BId;
                _ -> undefined
            end
    end,
	Qs       = zp_context:get_q("qs", Context),
	Page     = try list_to_integer(zp_context:get_q("page", Context, "1")) catch _:_ -> 1 end,
    AllProds = zp_search:search({fulltext_catbrand, [{cat,product},{text,Qs}]}, {1,1000}, Context),
    Total    = length(AllProds#search_result.result),
    
    {Cats, Brands} = count_all(AllProds#search_result.result),
    BrandList = sort_brands(Brands, Context),
    CatCounts = count_categories(Cats, Context),

    Result = case {CatId, BrandId} of
        {undefined, undefined} ->
            zp_search:pager(AllProds, Page, Context);
        _ ->
            zp_search:search_pager({fulltext_catbrand_filter, [{brand,BrandId},{cat,CatId},{text,Qs}]}, Page, Context)
    end,
    
    Vars   = [
        {cat_id, CatId},
        {brand_id, BrandId},
        {page, Page},
        {text, Qs},
        {result, Result},
        {total, Total},
        {brand_count, BrandList},
        {cat_count, CatCounts}
    ],
    Html = zp_template:render("search.tpl", Vars, Context),
	zp_context:output(Html, Context).
	

count_all(List) ->
    count_all(List, dict:new(), dict:new()).
count_all([], Cats, Brands) ->
    {dict:to_list(Cats), dict:to_list(Brands)};
count_all([{_Id, CatId, BrandId, _Rank}|Rest], Cats, Brands) ->
    Cats1   = dict:update_counter(CatId, 1, Cats),
    Brands1 = dict:update_counter(BrandId, 1, Brands),
    count_all(Rest, Cats1, Brands1).

sort_brands(Brands, Context) ->
    Names = [ {Id, m_rsc:p(Id, name, Context), Count} || {Id, Count} <- Brands ],
    lists:keysort(2, Names).


%% @spec sort_categories(Cats, Context) -> [{Id, Nr, MainCat, Count}, ...]
count_categories(Cats, Context) ->
    count_categories(Cats, Context, dict:new()).

count_categories([], _Context, Dict) ->
    Dict;
count_categories([{Id,N}|Rest], Context, Dict) ->
    Dict1 = dict:update_counter(Id, N, Dict),
    Path = m_category:get_path(Id, Context),
    Dict2 = lists:foldl(fun(CatId,D) -> dict:update_counter(CatId, N, D) end, Dict1, Path),
    count_categories(Rest, Context, Dict2).

