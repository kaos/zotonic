%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-11
%%
%% @doc Basic routines for shop functionality

-module(shop).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    start_link/0,
    start_link/1,
    category_brands/2,
    category_subcat_bybrand/3,
    category_rsc_count/2
]).

-include_lib("zophrenic.hrl").


%% @doc Start the shop processes. Only a periodic Adyen notification checker is needed (should make that into a server...)
start_link() ->
    start_link([]).
start_link([]) ->
    Context = zp_context:new(),
    shop_adyen:init(Context),
    ignore.
    

%% @doc Return the list of brands in a certain category, and with each brand the number of resources attached to that brand
%% @spec category_brands(CatId, Context) -> [ {Id,Count} ]
category_brands(Cat, Context) ->
    Id = m_category:name_to_id_check(Cat, Context),
    BrandPred = m_predicate:name_to_id_check(brand, Context),
    {Left, Right} = m_category:get_range(Id, Context),
    zp_db:q("
        select b.id, b.name, count(r.id)
        from category c
         join rsc r on r.category_id = c.id
         join edge e on e.subject_id = r.id
         join rsc b on e.object_id = b.id
        where r.is_published = true
          and r.visible_for = 0
          and c.nr >= $1
          and c.nr <= $2
          and e.predicate_id = $3
        group by b.id, b.name
        order by b.name
    ", [Left, Right, BrandPred], Context).


%% @doc Return the list of subcategories that have a product in the main category, and where the product has a certain brand
%% @spec category_brands_subcat(CatId, BrandId, Context) -> [ {Id,Count} ]
category_subcat_bybrand(CatId, undefined, Context) ->
    m_category:get_by_parent(CatId, Context);
category_subcat_bybrand(CatId, BrandId, Context) ->
    BrandPred = m_predicate:name_to_id_check(brand, Context),
    {Left, Right} = m_category:get_range(CatId, Context),
    Nrs = zp_db:q("
        select distinct c.nr
        from category c
         join rsc r on r.category_id = c.id
         join edge e on e.subject_id = r.id
        where r.is_published = true
          and r.visible_for = 0
          and c.nr >= $1
          and c.nr <= $2
          and e.predicate_id = $3
          and e.object_id = $4
    ", [Left, Right, BrandPred, BrandId], Context),
    % Select all subcats that have a lft/rght around any of the Nrs
    Nrs1 = [ N || {N} <- Nrs ],
    Sub = m_category:get_by_parent(CatId, Context),
    InRange = fun(L, R) ->
        lists:any(fun(N) -> L =< N andalso R >= N end, Nrs1)
    end,
    lists:filter(fun(Cat) -> InRange(proplists:get_value(lft, Cat), proplists:get_value(rght, Cat)) end, Sub).


%% @doc Count the number of visible resources in the category
%% @spec category_brands(CatId, Context) -> [ {Id,Count} ]
category_rsc_count(Cat, Context) ->
    Id = m_category:name_to_id_check(Cat, Context),
    {Left, Right} = m_category:get_range(Id, Context),
    zp_db:q1("
        select count(r.id)
        from category c
         join rsc r on r.category_id = c.id
        where r.is_published = true
          and r.visible_for = 0
          and c.nr >= $1
          and c.nr <= $2
    ", [Left, Right], Context).
