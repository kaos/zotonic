%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_search).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->
	MenuList = [
				[{title, "home"}, {uri, "/"}], 
				[{title, "Basic page"}, {uri, "/page/basic"}],
				[{title, "Product page"}, {uri, "/product/shimano/105-ST-5600"}],
				[{title, "Bike page"}, {uri, "/bike/trek/urban"}]
			],
	Context1 = zp_context:set_context(menu_list, MenuList, Context),
    
    Html = zp_template:render("search.tpl", Context1),
	zp_context:output(Html, Context1).