%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc List all orders.

-module(resource_admin_shop_order).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(ReqData, Context).


html(Context) ->
	Html = zp_template:render("admin_shop_order.tpl", [{page_shop_order, true}], Context),
	zp_context:output(Html, Context).
