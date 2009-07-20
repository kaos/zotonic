%% @author Tim Benniks <tim@timbenniks.com>
%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Tim Benniks.
%% @copyright 2009 Marc Worrell <marc@worrell.nl>
%% @doc Show

-module(resource_shop_category).

-author("Tim Benniks <tim@timbenniks.com>").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/2
]).

-include_lib("resource_html.hrl").

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    Cat    = z_context:get_q("cat", ContextQs),
    Subcat = z_context:get_q("subcat", ContextQs),
    Brand  = z_context:get_q("brand",  ContextQs),
    C1 = z_context:set([{cat_name, Cat}, {subcat_name, Subcat}], ContextQs),
    {Cat1, Subcat1} = case Cat of
        undefined -> {product, undefined};
        _ -> {Cat, Subcat}
    end,
    {OkBrand, C2} = case Brand of
        undefined ->
            {true, C1};
        _ ->
            case m_rsc:name_to_id(Brand, C1) of
                {ok, BrandId} ->
                    {true, z_context:set(brand_id, BrandId, C1)};
                {error, _} -> 
                    {false, C1}
            end
    end,
    
    {OkCat, C3} = case m_category:name_to_id(Cat1, C2) of
        {ok, CatId} ->
            case Subcat1 of
                undefined -> 
                    {true, z_context:set([{cat_id, CatId}, {is_subcat, false}], C2)};
                _ ->
                    case m_category:name_to_id(Subcat1, C2) of
                        {ok, SubcatId} ->
                            {true, z_context:set([{cat_id, SubcatId}, {is_subcat, true}], C2)};
                        {error, _} -> 
                            {false, C2}
                    end
            end;
        {error, _} ->
            {false, C2}
    end,
    ?WM_REPLY(OkCat andalso OkBrand, C3).


html(Context) ->
	IsSubCat = z_context:get(is_subcat, Context),
	CatId    = z_context:get(cat_id, Context),
	BrandId  = z_context:get(brand_id, Context),
	CatBrand = mod_shop:category_brands(CatId, Context),
    RscCount = mod_shop:category_rsc_count(CatId, Context),

	Vars = [
	    {cat_id, CatId},
	    {brand_id, BrandId},
	    {brand_name, m_rsc:p(BrandId, name, Context)},
	    {is_subcat, IsSubCat},
	    {cat, m_category:get(CatId, Context)},
	    {cat_brand, CatBrand},
	    {prod_count, RscCount}
	],
	Html = case IsSubCat of
		false ->
        	#search_result{result=Featured} = case BrandId of
        	    undefined -> z_depcache:memo(
        	                        {z_search, search, [{featured, [{cat,CatId}]}, Context]},
        	                        ?HOUR, [shop_import]);
        	    _ -> z_depcache:memo(
                                    {z_search, search, [{featured, [{cat,CatId},{object,BrandId},{predicate,brand}]}, Context]}, 
                                    ?HOUR, [shop_import])
        	end,
            {FeatShown,_} = z_utils:randomize(3, Featured),
            Vars1 = [
                {featured, FeatShown},
                {cat_name, z_context:get(cat_name, Context)},
                {subcats, mod_shop:category_subcat_bybrand(CatId, BrandId, Context)}
                | Vars ],
		    z_template:render("category.tpl", Vars1, Context);
		true ->
            #search_result{result=Products} = case BrandId of
                undefined -> z_depcache:memo(
                                    {z_search, search, [{featured, [{cat,CatId}]}, {1,1000}, Context]}, 
                                    ?HOUR, [shop_import]);
                _ -> z_depcache:memo(
                                    {z_search, search, [{featured, [{cat,CatId},{object,BrandId},{predicate,brand}]}, {1,1000}, Context]}, 
                                    ?HOUR, [shop_import])
            end,
            Vars1 = [
                {products, Products},
                {cat_name, z_context:get(cat_name, Context)},
                {subcat_name, z_context:get(subcat_name, Context)}
                | Vars ],
		    z_template:render("sub_category.tpl", Vars1, Context)
	end,
	z_context:output(Html, Context).
