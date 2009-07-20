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
    in_cart/2,
    in_cart/3,
    add_product/3,
    decr_product/3,
    del_product/3,
    format_price/1,
    get_cart/1,
    clear/1
]).

-include_lib("zotonic.hrl").
-include_lib("../include/shop.hrl").


%% @doc Return the expected allocation, backorders and price for the cart. This is done
%% by checking all skus in the database and running an allocation for the product ids.
%% @spec tpl_cart_allocated(Context) -> {Count, TotalPrice, Backorders, [ {id, n, backorder, price_avg, price_old, total} ]}
tpl_cart_allocated(Context) ->
    Allocated = [ 
        m_shop_product:allocate_sku_price(Id, Variant, N, Context) || #cart{idv={Id,Variant}, n=N} <- get_cart(Context)
    ],
    Cart = [
        [
            {id, Id},
            {variant, Variant},
            {n, N},
            {backorder, BackorderN},
            {price_avg, AvgPrice},
            {price_old, AvgOldPrice},
            {total, SumPrice},
            {media_id, MediaId}
        ] 
        || {Id, Variant, N, BackorderN, AvgPrice, AvgOldPrice, SumPrice, MediaId} <- Allocated 
    ],
    {Count, Total, Backorders} = lists:foldl(
            fun({_Id, _Variant, N, BackorderN, _AvgPrice, _AvgOldPrice, SumPrice, _MediaId}, {C,T,B}) ->
                case SumPrice of
                    undefined -> {C,T,B};
                    _ -> {C + N, T + SumPrice, B + BackorderN}
                end
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
        z_render:update("cart-info", "Uw winkelmand is leeg", Context);
    tpl_sync_cart_info(Total, 1, Context) ->
        z_render:update("cart-info", "Eén product van &euro;"++format_price(Total), Context);
    tpl_sync_cart_info(Total, Count, Context) ->
        z_render:update("cart-info", integer_to_list(Count)++" producten, &euro;"++format_price(Total), Context).
    


%% @doc Synchronize all prices shown on the cart page with the current cart, used inside event handlers
%% @spec tpl_sync_cart_prices(Context) -> Context
tpl_sync_cart_prices(Context) ->
    {Count, Total, Backorders, Cart} = tpl_cart_allocated(Context),
    C1 = z_render:update("cart-price-total", format_price(Total), Context),
    C2 = tpl_sync_cart_info(Total, Count, C1),
    C3 = case Backorders of
        0 -> z_render:wire("backorder-info", {slide_fade_out, []}, C2);
        _ -> z_render:wire("backorder-info", {slide_fade_in, []}, C2)
    end,
    lists:foldl(
        fun(CartProd, C) ->
            Id         = proplists:get_value(id, CartProd),
            Variant    = proplists:get_value(variant, CartProd),
            TotalPrice = proplists:get_value(total, CartProd),
            AvgPrice   = proplists:get_value(price_avg, CartProd),
            OldPrice   = proplists:get_value(price_old, CartProd),
            Backorder  = proplists:get_value(backorder, CartProd),
            
            ID = integer_to_list(Id) ++ [$- | z_string:to_slug(z_convert:to_list(Variant))],
            Cback = case Backorder of
                0 ->
                    z_render:wire("cart-backorder-p-"++ID, {fade_out, []}, C);
                _ ->
                    Cbo = z_render:update("cart-backorder-"++ID, integer_to_list(Backorder), C),
                    z_render:wire("cart-backorder-p-"++ID, {fade_in, []}, Cbo)
            end,
            Cold = case AvgPrice == OldPrice of
                true ->
                    z_render:update("cart-price-old-"++ID, "", Cback);
                false ->
                    z_render:update("cart-price-old-"++ID, ["&euro;",format_price(OldPrice)], Cback)
            end,
            Ctotal = z_render:update("cart-price-"++ID, format_price(TotalPrice), Cold),
            z_render:update("cart-price-avg-"++ID, format_price(AvgPrice), Ctotal)
        end,
        C3,
        Cart).


%% @doc A simple calculation of the prices for the cart, using the best price for every product involved
%% @spec get_cart_prices(Context) -> {TotalPrice, NumberOfProducts}
get_cart_prices(Context) ->
    Cart = get_cart(Context),
    lists:foldl(
            fun(#cart{idv={Id,Variant}, n=N}, {T,C}) -> 
                case best_price(Id, Variant, Context) of
                    undefined -> {T,C};
                    Price -> {T+(N * Price), C+N}
                end
            end, 
            {0, 0},
            Cart).

    best_price(Id, Variant, Context) ->
        {Price, _OldPrice, _IsVariant} = m_shop_product:get_best_price(Id, Variant, Context),
        Price.


%% @doc Format a price for displaying purposes, surpresses the ",00"
%% @spec format_price(float) -> String
format_price(undefined) -> 
    "-";
format_price(Price) ->
    erlydtl_filters:format_price(Price).


%% @doc Get the shopping cart of the current user
%% @spec get_cart(Context) -> [{Id,N},..]
get_cart(Context) ->
    case z_context:get_visitor(shop_cart, Context) of
        undefined -> [];
        <<>> -> [];
        Cart -> Cart
    end.

%% @doc Clear the shopping cart, used after receiving payments
clear(Context) ->
    z_context:set_visitor(shop_cart, [], Context).


%% @doc Get the count of the product in the cart, regardless of the variant
%% @spec in_cart(Id, #context) -> int()
in_cart(Id, Context) ->
    Cart = get_cart(Context),
    case lists:map(fun(#cart{idv={RscId, _V}, n=N}) when RscId == Id -> N; (_) -> 0 end, Cart) of
        [_N|_] = List -> lists:sum(List);
        [] -> 0
    end.

%% @doc Get the count of the product/variant in the cart
%% @spec in_cart(Id, Variant, #context) -> int()
in_cart(Id, Variant, Context) when is_binary(Variant) andalso is_integer(Id) ->
    Cart = get_cart(Context),
    case lists:filter(fun(#cart{idv={CartId, V}}) -> CartId == Id andalso Variant == V end, Cart) of
        [#cart{n=N}] -> N;
        [] -> 0
    end.
    
%% @doc Add the Id to the cart, increment when the id is already in the cart
%% @spec add_product(Id, Variant, Context) -> NewCount
add_product(Id, Variant, Context) when is_binary(Variant) andalso is_integer(Id) ->
    Cart = get_cart(Context),
    {N, Cart1} = add_cart(Cart, Id, Variant),
    z_context:set_visitor(shop_cart, Cart1, Context), 
    N.

    add_cart(Cart, Id, Variant) ->
        add_cart(Cart, Id, Variant, []).

    add_cart([], Id, Variant, Acc) ->
        {1, [ #cart{idv={Id, Variant},n=1} | lists:reverse(Acc)]};
    add_cart([#cart{idv={Id,Variant},n=N} = Cart |Rest], Id, Variant, Acc) ->
        {N+1, lists:reverse([ Cart#cart{n=N+1} | Acc], Rest)};
    add_cart([C|Rest], Id, Variant, Acc) ->
        add_cart(Rest, Id, Variant, [C|Acc]).


decr_product(Id, Variant, Context) when is_binary(Variant) andalso is_integer(Id) ->
    Cart = get_cart(Context),
    {N1, Cart1} = case lists:keysearch({Id, Variant}, #cart.idv, Cart) of
        {value, #cart{n=N}} when N > 1 -> {N-1, set_cart(Cart, Id, Variant, N-1)};
        {value, #cart{n=1}} -> {1, Cart};
        _ -> {0, Cart}
    end,
    z_context:set_visitor(shop_cart, Cart1, Context), 
    N1.


    set_cart(Cart, Id, Variant, N) ->
        set_cart(Cart, Id, Variant, N, []).

    set_cart([], Id, N, Variant, Acc) ->
        lists:reverse([ #cart{idv={Id,Variant},n=N} | Acc]);
    set_cart([#cart{idv={Id,Variant}} = Cart |Rest], Id, Variant, N, Acc) ->
        lists:reverse([ Cart#cart{n=N} | Acc]) ++ Rest;
    set_cart([C|Rest], Id, Variant, N, Acc) ->
        set_cart(Rest, Id, Variant, N, [C|Acc]).

del_product(Id, Variant, Context) when is_binary(Variant) andalso is_integer(Id) ->
    Cart = get_cart(Context),
    Cart1 = lists:keydelete({Id, Variant}, #cart.idv, Cart),
    z_context:set_visitor(shop_cart, Cart1, Context).



