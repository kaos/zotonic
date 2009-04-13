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
    get_cart_prices/1,
    add_product/2,
    decr_product/2,
    del_product/2,
    format_price/1
]).

-include_lib("zophrenic.hrl").



%% @doc Syncronise all prices shown on the cart page with the current cart, used inside event handlers
%% @spec tpl_sync_cart_prices(Context) -> Context
tpl_sync_cart_prices(Context) ->
    {Total, Count, Prices} = get_cart_prices(Context),
    C1 = zp_render:update("cart-price-total", format_price(Total), Context),
    C2 = tpl_sync_cart_info(Total, Count, C1),
    lists:foldl(
        fun({Id, _N, _Price, NPrice}, C) ->
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
    


%% @doc Return a list of all unit prices, order line prices and cumulative price
%% @spec prices(Context) -> {TotalPrice, NumberOfProducts, [{id, N, price1, priceN}]}
get_cart_prices(Context) ->
    Cart = zp_context:get_session(shop_cart, Context),
    Prices = prices(Cart, Context, []),
    {Count,Total} = lists:foldl(fun({_Id, N, _Price, NPrice}, {C,T}) -> {C+N,T+NPrice} end, {0, 0.0}, Prices),
    {Total, Count, Prices}.

prices(undefined, _Context, Acc) ->
    Acc;
prices([], _Context, Acc) ->
    Acc;
prices([{Id,N}|Rest], Context, Acc) ->
    P = case m_rsc:p(Id, price, Context) of
        undefined -> {Id, N, 0.0, 0.0};
        Price -> {Id, N, Price, N*Price}
    end,
    prices(Rest, Context, [P|Acc]).
    
    


%% @doc Format a price for displaying purposes, surpresses the .00
%% @spec format_price(float) -> String
format_price(Price) ->
    erlydtl_filters:format_price(Price).


%% @doc Get the shopping cart of the current user
%% @spec get_cart(Context) -> [{Id,N},..]
get_cart(Context) ->
    zp_context:get_session(shop_cart, Context).


%% @doc Add the Id to the context, do not add it when it was added within the last 2 seconds (prevent double clicks)
%% @spec add_product(Id, Context) -> NewCount
add_product(Id, Context) ->
    Cart  = zp_context:get_session(shop_cart, Context),
    {N, Cart1} = add_cart(Cart, Id),
    zp_context:set_session(shop_cart, Cart1, Context), 
    N.

decr_product(Id, Context) ->
    Cart = zp_context:get_session(shop_cart, Context),
    {N1, Cart1} = case proplists:get_value(Id, Cart) of
        N when N > 1 -> {N-1, set_cart(Cart, Id, N-1)};
        1 -> {1, Cart};
        _ -> {0, Cart}
    end,
    zp_context:set_session(shop_cart, Cart1, Context), 
    N1.

del_product(Id, Context) ->
    Cart = zp_context:get_session(shop_cart, Context),
    Cart1 = proplists:delete(Id, Cart),
    zp_context:set_session(shop_cart, Cart1, Context).


add_cart(undefined, Id) ->
    {1, [{Id,1}]};
add_cart(Cart, Id) ->
    add_cart(Cart, Id, []).

add_cart([], Id, Acc) ->
    {1, lists:reverse([{Id,1}|Acc])};
add_cart([{Id,N}|Rest], Id, Acc) ->
    {N+1, lists:reverse([{Id,N+1}|Acc]) ++ Rest};
add_cart([C|Rest], Id, Acc) ->
    add_cart(Rest, Id, [C|Acc]).

set_cart(Cart, Id, N) ->
    set_cart(Cart, Id, N, []).
set_cart([], Id, N, Acc) ->
    lists:reverse([{Id,N}|Acc]);
set_cart([{Id,_}|Rest], Id, N, Acc) ->
    lists:reverse([{Id,N}|Acc]) ++ Rest;
set_cart([C|Rest], Id, N, Acc) ->
    set_cart(Rest, Id, N, [C|Acc]).
