%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell <marc@worrell.nl>
%% @doc Show an order by name

-module(resource_shop_order_view).

-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/2
]).

-include_lib("resource_html.hrl").

resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    Name = z_context:get_q("name", ContextQs),
    Order = shop_order:get_by_name(Name, ContextQs),
    case Order of
        undefined ->
            ?WM_REPLY(false, ContextQs);
        _ ->
            ContextOrder = z_context:set(order, Order, ContextQs),
            ?WM_REPLY(true, ContextOrder) 
    end.


html(Context) ->
	Order = z_context:get(order, Context),
	Vars = [
	    {order, Order}
	],
	Html = z_template:render("order_view.tpl", Vars, Context),
	z_context:output(Html, Context).
