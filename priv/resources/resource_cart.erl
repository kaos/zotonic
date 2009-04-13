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
    {TotalPrice, Count, Cart} = shop_cart:get_cart_prices(Context),
    Vars = [
        {menu_list, MenuList},
        {shop_cart, Cart},
        {shop_cart_total, TotalPrice},
        {shop_cart_count, Count}
    ],

    Html = zp_template:render("cart.tpl", Vars, Context),
	zp_context:output(Html, Context).
