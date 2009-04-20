%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_cart).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->

    {Count, Total, Backorder, Cart} = shop_cart:tpl_cart_allocated(Context),
    Vars = [
        {shop_cart, Cart},
        {shop_cart_total, Total},
        {shop_cart_count, Count},
        {shop_cart_backorder, Backorder}
    ],

    Html = zp_template:render("cart.tpl", Vars, Context),
	zp_context:output(Html, Context).
