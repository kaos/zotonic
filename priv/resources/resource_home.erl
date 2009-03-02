%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_home).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->
	
	MenuList = [
				[{title, "home"}, 		{uri, "/"}], 
				[{title, "fietsen"}, 	{uri, "/page/fietsen"}],
				[{title, "Trek bikes"}, {uri, "/product/trek/1"}]
			],

	Context1 = zp_context:set_context(menu_list, MenuList, Context),
    
    Html = zp_template:render("home.tpl", Context1),
	zp_context:output(Html, Context1).