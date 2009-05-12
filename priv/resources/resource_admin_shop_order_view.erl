%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc View an order

-module(resource_admin_shop_order_view).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

%% @todo Change this into "visible" and add a view instead of edit template.
is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(true, visible, "id", ReqData, Context).


html(Context) ->
    Id       = zp_context:get_q("id", Context),
    IdN      = list_to_integer(Id),
    Order    = shop_order:get(IdN, Context),
    Vars     = [
        {id, IdN},
        {order, Order}
    ],
    Html = zp_template:render("admin_shop_order_view.tpl", Vars, Context),
	zp_context:output(Html, Context).


%% @doc Resend the order to VMSII
event({postback, email_admin, _FormId, _TargetId}, Context) ->
    zp_render:wire({growl, [{text,"Sent email (todo)."}]}, Context).

