%% @author Tim Benniks <tim@timbenniks.com>
%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Tim Benniks.
%% @copyright 2009 Marc Worrell <marc@worrell.nl>
%% @doc Show

-module(resource_category).

-author("Tim Benniks <tim@timbenniks.com>").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/2
]).

-include_lib("resource_html.hrl").

resource_exists(_ReqProps, Context) ->
    ContextQs = zp_context:ensure_qs(Context),
    Cat = zp_context:get_q("cat", ContextQs),
    Subcat = zp_context:get_q("subcat", ContextQs),
    case m_category:name_to_id(Cat, ContextQs) of
        undefined ->
            {false, ContextQs};
        CatId ->
            case Subcat of
                undefined -> 
                    {true, zp_context:set([{cat_id, CatId}, {is_subcat, false}], ContextQs)};
                _ ->
                    case m_category:name_to_id(Subcat, ContextQs) of
                        undefined -> 
                            {false, ContextQs};
                        SubcatId ->
                            {true, zp_context:set([{cat_id, SubcatId}, {is_subcat, true}], ContextQs)}
                    end
            end
    end.

html(_ReqProps, Context) ->
	MenuList = [
				[{title, "home"}, {uri, "/"}], 
				[{title, "Basic page"}, {uri, "/page/basic"}],
				[{title, "Product page"}, {uri, "/product/shimano/105-ST-5600"}],
				[{title, "Bike page"}, {uri, "/bike/trek/urban"}]
			],
	IsSubCat = zp_context:get(is_subcat, Context),
	CatId = zp_context:get(cat_id, Context),
    Featured = zp_depcache:memo({search, search, [{category_featured, CatId}, Context]}),
    {FeatShown,_} = zp_utils:randomize(3, Featured),
	Vars = [
	    {menu_list, MenuList},
	    {cat_id, CatId},
	    {is_subcat, IsSubCat},
	    {featured, FeatShown},
	    {cat, m_category:get(CatId, Context)}
	],
	Html = case IsSubCat of
		false -> zp_template:render("category.tpl", Vars, Context);
		true ->	zp_template:render("sub_category.tpl", Vars, Context)
	end,
	zp_context:output(Html, Context).
