%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_category).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").
 
html(_ReqProps, Context) ->
	MenuList = [
				[{title, "home"}, {uri, "/"}], 
				[{title, "Basic page"}, {uri, "/page/basic"}],
				[{title, "Product page"}, {uri, "/product/shimano/105-ST-5600"}],
				[{title, "Bike page"}, {uri, "/bike/trek/urban"}]
			],
    
	Html = case zp_context:get_q("subcat", Context) of
		undefined -> zp_template:render("category.tpl", [{menu_list, MenuList}], Context);
		_ ->
			zp_template:render("sub_category.tpl", [{menu_list, MenuList}], Context)
	end,
	
	zp_context:output(Html, Context).