%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_page).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->
	MenuList = [
				[{title, "home"}, {uri, "/"}], 
				[{title, "fietsen"}, {uri, "/page/fietsen"}]
			],
	Context1 = zp_context:set_context(menu_list, MenuList, Context),
    
    Html = zp_template:render("page.tpl", Context1),
	zp_context:output(Html, Context1).