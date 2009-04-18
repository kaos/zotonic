%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_search).
-author("Tim Benniks <tim@timbenniks.com>").

-export([resource_exists/2]).

-include_lib("resource_html.hrl").

resource_exists(_ReqProps, Context) ->
    ContextQs = zp_context:ensure_qs(Context),
    Cat = zp_context:get_q("cat", ContextQs, product),
    case m_category:name_to_id(Cat, ContextQs) of
         {ok, CatId} ->
             {true, zp_context:set([{cat_id, CatId}], ContextQs)};
         {error, _} -> 
             {false, ContextQs}
     end.
    
html(_ReqProps, Context) ->
    Context1 = zp_context:ensure_all(Context),
	CatId  = zp_context:get(cat_id, Context1),
	Qs     = zp_context:get_q("qs", Context1),
	Page   = try list_to_integer(zp_context:get_q("page", Context1, "1")) catch _:_ -> 1 end,
    Result = zp_search:search_pager({fulltext_catbrand, [{cat,CatId},{text,Qs}]}, Page, Context1),
    
    {Cats, Brands} = count_all(Result#search_result.all),
    BrandList = sort_brands(Brands, Context),
    CatCounts = count_categories(Cats, Context),
    
    Vars   = [
        {cat_id, CatId},
        {page, Page},
        {text, Qs},
        {result, Result},
        {brand_count, BrandList},
        {cat_count, CatCounts}
    ],
    Html = zp_template:render("search.tpl", Vars, Context1),
	zp_context:output(Html, Context1).
	

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

