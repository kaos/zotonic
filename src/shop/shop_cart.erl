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
    tpl_cart_allocated/1,
    tpl_sync_cart_prices/1,
    tpl_sync_cart_info/1,
    tpl_sync_cart_info/3,
    get_cart/1,
    in_cart/2,
    add_product/2,
    decr_product/2,
    del_product/2,
    format_price/1
]).

-include_lib("zophrenic.hrl").

-record(cart, {id, n, sku, variant}).


%% @doc Return the expected allocation, backorders and price for the cart. This is done
%% by checking all skus in the database and running an allocation for the product ids.
%% @spec tpl_cart_allocated(Context) -> {Count, TotalPrice, Backorders, [ {id, n, backorder, price_avg, price_old total} ]}
tpl_cart_allocated(Context) ->
    Allocated = [ m_shop_product:allocate_sku_price(Id, N, Context) || #cart{id=Id, n=N} <- get_cart(Context) ],
    Cart = [
        [
            {id, Id},
            {n, N},
            {backorder, BackorderN},
            {price_avg, AvgPrice},
            {price_old, AvgOldPrice},
            {total, SumPrice}
        ] 
        || {Id, N, BackorderN, AvgPrice, AvgOldPrice, SumPrice} <- Allocated 
    ],
    {Count, Total, Backorders} = lists:foldl(
            fun({_Id, N, BackorderN, _AvgPrice, _AvgOldPrice, SumPrice}, {C,T,B}) ->
                {C + N, T + SumPrice, B + BackorderN}
            end,
            {0, 0, 0},
            Allocated),
    {Count, Total, Backorders, Cart}.


%% @doc Update the small cart information on the page with a new product count and total price
tpl_sync_cart_info(Context) ->
    {Total, Count} = get_cart_prices(Context),
    tpl_sync_cart_info(Total, Count, Context).

    %% Update the small cart info on the page
    tpl_sync_cart_info(_Total, 0, Context) ->
        zp_render:update("cart-info", "Uw winkelmand is leeg", Context);
    tpl_sync_cart_info(Total, 1, Context) ->
        zp_render:update("cart-info", "Eén product van &euro;"++format_price(Total), Context);
    tpl_sync_cart_info(Total, Count, Context) ->
        zp_render:update("cart-info", integer_to_list(Count)++" producten, &euro;"++format_price(Total), Context).
    


%% @doc Synchronize all prices shown on the cart page with the current cart, used inside event handlers
%% @spec tpl_sync_cart_prices(Context) -> Context
tpl_sync_cart_prices(Context) ->
    {Count, Total, Backorders, Cart} = tpl_cart_allocated(Context),
    C1 = zp_render:update("cart-price-total", format_price(Total), Context),
    C2 = tpl_sync_cart_info(Total, Count, C1),
    C3 = case Backorders of
        0 -> zp_render:wire("backorder-info", {slide_fade_out, []}, C2);
        _ -> zp_render:wire("backorder-info", {slide_fade_in, []}, C2)
    end,
    lists:foldl(
        fun(CartProd, C) ->
            Id = proplists:get_value(id, CartProd),
            TotalPrice = proplists:get_value(total, CartProd),
            AvgPrice = proplists:get_value(price_avg, CartProd),
            OldPrice = proplists:get_value(price_old, CartProd),
            Backorder = proplists:get_value(backorder, CartProd),
            
            ID = integer_to_list(Id),
            Cback = case Backorder of
                0 ->
                    zp_render:wire("cart-backorder-p-"++ID, {fade_out, []}, C);
                _ ->
                    Cbo = zp_render:update("cart-backorder-"++ID, integer_to_list(Backorder), C),
                    zp_render:wire("cart-backorder-p-"++ID, {fade_in, []}, Cbo)
            end,
            Cold = case AvgPrice == OldPrice of
                true ->
                    zp_render:update("cart-price-old-"++ID, "", Cback);
                false ->
                    zp_render:update("cart-price-old-"++ID, ["&euro;",format_price(OldPrice)], Cback)
            end,
            Ctotal = zp_render:update("cart-price-"++ID, format_price(TotalPrice), Cold),
            zp_render:update("cart-price-avg-"++ID, format_price(AvgPrice), Ctotal)
        end,
        C3,
        Cart).


%% @doc A simple calculation of the prices for the cart, using the best price for every product involved
%% @spec get_cart_prices(Context) -> {TotalPrice, NumberOfProducts}
get_cart_prices(Context) ->
    Cart = get_cart(Context),
    lists:foldl(
            fun(#cart{id=Id, n=N}, {T,C}) -> 
                Price = best_price(Id, Context),
                {T+(N * Price), C+N}
            end, 
            {0, 0},
            Cart).

    best_price(Id, Context) ->
        {Price, _OldPrice, _IsVariant} = m_shop_product:get_best_price(Id, Context),
        Price.


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
