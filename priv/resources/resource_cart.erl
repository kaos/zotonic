%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_cart).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->
	
	MenuList = [
				[{title, "Home"}, {uri, "/"}], 
				[{title, "Filialen"}, {uri, "/page/basic"}],
				[{title, "Klantenkaart"}, {uri, "/product/shimano/105-ST-5600"}],
				[{title, "Fiets van de zaak"}, {uri, "/bike/trek/urban"}],
				[{title, "Fietstochten"}, {uri, "/bike/trek/urban"}],
				[{title, "Agenda"}, {uri, "/bike/trek/urban"}]
			],

    Html = zp_template:render("cart.tpl", [{menu_list, MenuList}], Context),
	zp_context:output(Html, Context).