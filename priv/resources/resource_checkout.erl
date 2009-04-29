%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_checkout).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    event/2
]).

-include_lib("resource_html.hrl").
 
html(Context) ->
    {Count, Total, Backorder, Cart} = shop_cart:tpl_cart_allocated(Context),
    Vars = [
        {shop_cart, Cart},
        {shop_cart_total, Total},
        {shop_cart_count, Count},
        {shop_cart_backorder, Backorder}
    ],
    Html = zp_template:render("checkout.tpl", Vars, Context),
    zp_context:output(Html, Context).
    

%% @doc Handle the checkout event.  Start the payment processing.
event({submit,{payment, [ {total,TotalAmount} ]}, _TriggerId, _TargetId}, Context) ->
    % Fetch all parameters, they are validated so we know they are present

    DeliveryAttn     = zp_context:get_q("client-delivery-attn", Context),
    DeliveryStreet   = zp_context:get_q_validated("client-delivery-address", Context),
    DeliveryPostcode = zp_context:get_q_validated("client-delivery-postcode", Context),
    DeliveryCity     = zp_context:get_q_validated("client-delivery-city", Context),
    DeliveryCountry  = zp_context:get_q_validated("client-delivery-country", Context),

    BillingAddress = string:strip(zp_context:get_q("client-billing-address", Context)),
    Attn = case BillingAddress of
        [] -> DeliveryAttn;
        _ -> zp_context:get_q("client-billing-attn", Context)
    end,

    Street = case zp_context:get_q("client-billing-address", Context) of
        [] -> DeliveryStreet;
        Str -> Str
    end,

    {City, Country} = case zp_context:get_q("client-billing-city", Context) of
        [] -> {DeliveryCity, DeliveryCountry};
        Cty -> {Cty, zp_context:get_q("client-billing-country", Context)}
    end,

    Postcode = case zp_context:get_q("client-billing-postcode", Context) of
        [] -> DeliveryPostcode;
        Pcd -> Pcd
    end,

    Parms = [
        {lastname,  zp_context:get_q_validated("client-name", Context)},
        {phone,     zp_context:get_q_validated("client-phone", Context)},
        {email,     zp_context:get_q_validated("client-email", Context)},
        
        {attn,      Attn},
        {street,    Street},
        {postcode,  Postcode},
        {city,      City},
        {country,   Country},

        {delivery_attn,      zp_context:get_q("client-delivery-attn", Context)},
        {delivery_street,    zp_context:get_q_validated("client-delivery-address", Context)},
        {delivery_postcode,  zp_context:get_q_validated("client-delivery-postcode", Context)},
        {delivery_city,      zp_context:get_q_validated("client-delivery-city", Context)},
        {delivery_country,   zp_context:get_q_validated("client-delivery-country", Context)},
        
        {payment_method, "card"}
    ],

    Parms1 = [ {K, zp_html:escape(V)} || {K, V} <- Parms ],
    do_order(TotalAmount, Parms1, Context);

event({postback, {proceed, Props}, _TriggerId, _TargetId}, Context) ->
    {amount, Amount} = proplists:lookup(amount, Props),
    {details, Args}  = proplists:lookup(details, Props),
    do_order(Amount, Args, Context).



%% @doc Allocate the orderlines and redirect the user to the payment service provider.  Can show error dialog if
%% there is an error during the order processing.
%% @spec do_order(int(), Params, Context1) -> Context2
do_order(TotalAmount, Parms, Context) ->
    case shop_order:order_from_cart(TotalAmount, Parms, Context) of
        {ok, OrderId, Context1} ->
            %% OK, redirect to the payment service provider
            RedirectUri = shop_adyen:payment_start(OrderId, Context1),
            zp_render:wire({redirect, [{location, RedirectUri}]}, Context1);

        {new_total, NewTotal, Context1} ->
            %% Show dialog that the amount is changed, ask permission to go ahead
            DTitle = "Ander totaalbedrag",
            Vars = [
                {details, Parms},
                {total_amount, NewTotal}
            ],
            Html = zp_template:render("_checkout_dialog_amount_changed.tpl", Vars, Context1),
            {Html1, Context2} = zp_render:render_to_string(Html, Context1),
            zp_render:wire({dialog, [{title, DTitle}, {text, Html1}]}, Context2);

        {error, _Reason, Context1} ->
            %% Something went wrong, tell the user and redirect back to the cart page
            Html = zp_template:render("_checkout_dialog_error.tpl", [], Context1),
            {Html1, Context2} = zp_render:render_to_string(Html, Context1),
            zp_render:wire({dialog, [{title, "Kon order niet plaatsen"}, {text, Html1}]}, Context2)
    end.
