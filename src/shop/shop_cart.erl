%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-13
%%
%% @doc Shopping cart
%% @todo Make this into a gen_server to solve race conditions.
%% @todo Check stock when handling shopping cart

-module(shop_cart).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    tpl_sync_cart_prices/1,
    tpl_sync_cart_info/1,
    tpl_sync_cart_info/3,
    get_cart/1,
    in_cart/2,
    get_cart_prices_tpl/1,
    get_cart_prices/1,
    add_product/2,
    decr_product/2,
    del_product/2,
    format_price/1
]).

-include_lib("zophrenic.hrl").

-record(cart, {id, n, sku, variant, price, nprice}).


%% @doc Synchronize all prices shown on the cart page with the current cart, used inside event handlers
%% @spec tpl_sync_cart_prices(Context) -> Context
tpl_sync_cart_prices(Context) ->
    {Total, Count, Prices} = get_cart_prices(Context),
    C1 = zp_render:update("cart-price-total", format_price(Total), Context),
    C2 = tpl_sync_cart_info(Total, Count, C1),
    lists:foldl(
        fun(#cart{id=Id, nprice=NPrice}, C) ->
            zp_render:update("cart-price-"++integer_to_list(Id), format_price(NPrice), C)
        end,
        C2,
        Prices).

tpl_sync_cart_info(Context) ->
    {Total, Count, _Prices} = get_cart_prices(Context),
    tpl_sync_cart_info(Total, Count, Context).

tpl_sync_cart_info(_Total, 0, Context) ->
    zp_render:update("cart-info", "Uw winkelmand is leeg", Context);
tpl_sync_cart_info(Total, 1, Context) ->
    zp_render:update("cart-info", "Eén product van &euro;"++format_price(Total), Context);
tpl_sync_cart_info(Total, Count, Context) ->
    zp_render:update("cart-info", integer_to_list(Count)++" producten, &euro;"++format_price(Total), Context).
    

get_cart_prices_tpl(Context) ->
    {Total, Count, CartPrices} = get_cart_prices(Context),
    {Total, Count, [ cart_to_proplist(C) || C <- CartPrices ]}.

    cart_to_proplist(#cart{id=Id,n=N,variant=Variant,price=Price,nprice=NPrice}) ->
        [
            {id, Id}, {variant, Variant}, {n, N}, {price, Price}, {nprice, NPrice}
        ].

%% @doc Return a list of all unit prices, order line prices and cumulative price
%% @spec prices(Context) -> {TotalPrice, NumberOfProducts, [{id, N, price1, priceN}]}
get_cart_prices(Context) ->
    Cart = get_cart(Context),
    CartPrices = prices(Cart, Context, []),
    {Count,Total} = lists:foldl(fun(#cart{n=N, nprice=NPrice}, {C,T}) -> {C+N,T+NPrice} end, {0, 0}, CartPrices),
    {Total, Count, CartPrices}.

prices(undefined, _Context, Acc) ->
    Acc;
prices([], _Context, Acc) ->
    Acc;
prices([#cart{id=Id,n=N}|Rest], Context, Acc) ->
    P = case m_rsc:p(Id, price, Context) of
        undefined -> #cart{id=Id, n=N, price=0, nprice=0};
        Price -> #cart{id=Id, n=N, price=Price, nprice=N*Price}
    end,
    prices(Rest, Context, [P|Acc]).



%% @doc Format a price for displaying purposes, surpresses the ",00"
%% @spec format_price(float) -> String
format_price(Price) ->
    erlydtl_filters:format_price(Price).


%% @doc Get the shopping cart of the current user
%% @spec get_cart(Context) -> [{Id,N},..]
get_cart(Context) ->
    case zp_context:get_visitor(shop_cart, Context) of
        undefined -> [];
        Cart -> Cart
    end.

%% @doc Get the count of the product in the cart
%% @spec in_cart(Id, #context) -> int()
in_cart(Id, Context) ->
    Cart = get_cart(Context),
    case lists:filter(fun(#cart{id=CartId}) -> CartId == Id end, Cart) of
        [#cart{id=Id,n=N}] -> N;
        [] -> 0
    end.

%% @doc Add the Id to the cart, increment when the id is already in the cart
%% @spec add_product(Id, Context) -> NewCount
add_product(Id, Context) ->
    Cart = get_cart(Context),
    {N, Cart1} = add_cart(Cart, Id),
    zp_context:set_visitor(shop_cart, Cart1, Context), 
    N.

decr_product(Id, Context) ->
    Cart = get_cart(Context),
    {N1, Cart1} = case lists:keysearch(Id, #cart.id, Cart) of
        {value, #cart{n=N}} when N > 1 -> {N-1, set_cart(Cart, Id, N-1)};
        {value, #cart{n=1}} -> {1, Cart};
        _ -> {0, Cart}
    end,
    zp_context:set_visitor(shop_cart, Cart1, Context), 
    N1.

del_product(Id, Context) ->
    Cart = get_cart(Context),
    Cart1 = lists:keydelete(Id, #cart.id, Cart),
    zp_context:set_visitor(shop_cart, Cart1, Context).


add_cart(undefined, Id) ->
    {1, [{Id,1}]};
add_cart(Cart, Id) ->
    add_cart(Cart, Id, []).

add_cart([], Id, Acc) ->
    {1, lists:reverse([ #cart{id=Id,n=1} | Acc])};
add_cart([#cart{id=Id,n=N} = Cart |Rest], Id, Acc) ->
    {N+1, lists:reverse([ Cart#cart{n=N+1} | Acc]) ++ Rest};
add_cart([C|Rest], Id, Acc) ->
    add_cart(Rest, Id, [C|Acc]).

set_cart(Cart, Id, N) ->
    set_cart(Cart, Id, N, []).
set_cart([], Id, N, Acc) ->
    lists:reverse([ #cart{id=Id,n=N} | Acc]);
set_cart([#cart{id=Id} = Cart |Rest], Id, N, Acc) ->
    lists:reverse([ Cart#cart{n=N} | Acc]) ++ Rest;
set_cart([C|Rest], Id, N, Acc) ->
    set_cart(Rest, Id, N, [C|Acc]).
