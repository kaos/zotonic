%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_shop_confirm).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").
 
html(Context) ->
    case shop_adyen:payment_completion(Context) of
        {ok, OrderId} ->
            Order = shop_order:get(OrderId, Context),
            case proplists:get_value(paid, Order) of
                true -> shop_cart:clear(Context);
                false ->
                    case proplists:get_value(status, Order) of
                        payment_pending -> shop_cart:clear(Context);
                        _ -> nop
                    end
            end,
            
            Vars = [
                {order_id, OrderId},
                {order, Order}
            ],
            Html = z_template:render("checkout_confirm.tpl", Vars, Context),
            z_context:output(Html, Context);
            
        {error, sig_invalid} ->
            % Somebody is hacking our url - bail out
            Vars = [
                {sig_invalid, true}
            ],
            Html = z_template:render("checkout_confirm.tpl", Vars, Context),
            z_context:output(Html, Context)
    end.

