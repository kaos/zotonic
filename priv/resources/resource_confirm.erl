%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_confirm).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").
 
html(Context) ->
    case shop_adyen:payment_completion(Context) of
        {ok, OrderId} ->
            Vars = [
                {order_id, OrderId},
                {order, shop_order:get(OrderId, Context)}
            ],
            Html = zp_template:render("checkout_confirm.tpl", Vars, Context),
            zp_context:output(Html, Context);
            
        {error, sig_invalid} ->
            % Somebody is hacking our url - bail out
            Vars = [
                {sig_invalid, true}
            ],
            Html = zp_template:render("checkout_confirm.tpl", Vars, Context),
            zp_context:output(Html, Context)
    end.

