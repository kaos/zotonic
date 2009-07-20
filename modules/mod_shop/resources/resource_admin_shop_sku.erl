%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc List all skus.

-module(resource_admin_shop_sku).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    z_auth:wm_is_authorized(ReqData, Context).


html(Context) ->
	Html = z_template:render("admin_shop_sku.tpl", [{page_shop_sku, true}], Context),
	z_context:output(Html, Context).
