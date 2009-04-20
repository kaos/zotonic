%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_cart).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->

    {TotalPrice, Count, Cart} = shop_cart:get_cart_prices_tpl(Context),
    Vars = [
        {shop_cart, Cart},
        {shop_cart_total, TotalPrice},
        {shop_cart_count, Count}
    ],

    Html = zp_template:render("cart.tpl", Vars, Context),
	zp_context:output(Html, Context).
