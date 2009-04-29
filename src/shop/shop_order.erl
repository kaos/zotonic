%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-28
%%
%% @doc Interface functions for order processing.

-module(shop_order).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    get/2,
    order_from_cart/3,
    payment_completion/5
]).

-include_lib("zophrenic.hrl").
-include_lib("shop.hrl").



%% @doc Get the full details of an order, including the order lines.
%% @todo Add also the order history
%% @spec get(OrderId, Context) -> proplist
get(OrderId, Context) ->
    Order = zp_db:assoc_row("select * from shop_order where id = $1", [OrderId], Context),
    Lines = zp_db:assoc("
            select ol.*, 
                ol.quantity * ol.price_incl as total_price_incl, 
                ol.quantity * ol.price_excl as total_price_excl, 
                s.rsc_id, s.variant, s.article_nr, s.description1, s.description2
            from shop_order_line ol join shop_sku s on ol.shop_sku_id = s.id 
            where ol.shop_order_id = $1", [OrderId], Context),
    [{lines, Lines} | Order].


%% @doc Make an order from the cart.  Returns an error if the payment due is unequal to the amount the user ok'd.
%% Also adds the order and delivery costs to the order.
order_from_cart(TotalAmount, Address, Context) ->
    
    %% 1. Cancel all orders for this visitor that are still waiting for payment (should also do this on the cart and checkout page?)
    %% 2. Cleanup all orders that are past their waiting time

    Allocated = [
        m_shop_product:allocate_sku(Id, Variant, N, Context) || #cart{idv={Id,Variant}, n=N} <- shop_cart:get_cart(Context)
    ],
    
    % [ {[Sku], backorder} ] -> [Sku]
    % Collect together the orders and the backorders.
    Skus = lists:foldl(
        fun ({Sks, #sku_alloc{n=0}}, Acc) ->
                Sks ++ Acc;
            ({Sks, #sku_alloc{sku_nr=undefined}}, Acc) ->
                Sks ++ Acc;
            ({Sks, BoSku}, Acc) ->
                [BoSku | Sks ] ++ Acc
        end,
        [],
        Allocated),
    
    {SkuAmountIncl, SkuAmountExcl} = lists:foldl(
            fun(#sku_alloc{n=N, price_incl=PIncl, price_excl=PExcl}, {AccIncl, AccExcl}) -> 
                {N * PIncl + AccIncl, N * PExcl + AccExcl} 
            end, 
            {0,0},
            Skus),

    {VisitorId, Context1} = zp_context:ensure_visitor_id(Context),

    %% @todo: Calculate the order/delivery costs, determine the skus for the delivery costs.
    %% @todo Add the order and delivery costs skus to the list of Skus to be ordered.
    
    case SkuAmountIncl > TotalAmount of
        true ->
            {new_total, SkuAmountIncl, Context1};
        false ->
            F = fun(Ctx) ->
                insert_order(VisitorId, SkuAmountIncl, SkuAmountExcl, Skus, Address, Ctx)
            end,

            case zp_db:transaction(F, Context1) of
                {ok, OrderId} ->
                    {ok, OrderId, Context1};
                {rollback, {error, could_not_allocate}} ->
                    order_from_cart(TotalAmount, Address, Context1);
                {rollback, Reason} ->
                    {error, Reason, Context1}
            end
    end.


%% @doc Received a completion from the PSP, check the order if we can mark it as paid
payment_completion(OrderId, AuthResult, PaymentMethod, PspReference, Context) ->
    {ok, OrderId}.


%% @doc Allocate the order in the database. Throw error on failure.
insert_order(VisitorId, SkuAmountIncl, SkuAmountExcl, Skus, Address, Context) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    Expires = calendar:gregorian_seconds_to_datetime(NowSecs + ?SHOP_ORDER_EXPIRE),
    Args = [
        {visitor_id, VisitorId},
        {name, zp_ids:id(32)},
        {status, "new"},
        {expires, Expires},
        {total_price_incl, SkuAmountIncl},
        {total_price_excl, SkuAmountExcl}
        | Address
    ],
    {ok, Id} = zp_db:insert(shop_order, Args, Context),
    insert_order_lines(Id, Skus, Context).
    
    
insert_order_lines(Id, [], _Context) ->
    {ok, Id};
insert_order_lines(Id, [#sku_alloc{n=0}|T], Context) ->
    insert_order_lines(Id, T, Context);
insert_order_lines(Id, [Sku|T], Context) ->
    Args = [
        {shop_order_id, Id},
        {shop_sku_id, Sku#sku_alloc.sku_id},
        {quantity, Sku#sku_alloc.n},
        {price_incl, Sku#sku_alloc.price_incl},
        {price_excl, Sku#sku_alloc.price_excl}
    ],
    
    case {Sku#sku_alloc.order_costs, Sku#sku_alloc.backorder} of
        {true, _} ->
            % Order costs, endless supply
            Args1 = [
                {allocated, 0},
                {backorder, 0}
            ],
            {ok, _} = zp_db:insert(shop_order_line, Args1, Context),
            insert_order_lines(Id, T, Context);

        {_, true} ->
            % Backorder, do not allocate the stock
            Args1 = [
                {allocated, 0},
                {backorder, Sku#sku_alloc.n} | Args
            ],
            {ok, _} = zp_db:insert(shop_order_line, Args1, Context),
            insert_order_lines(Id, T, Context);

        {_, false} ->
            % Normal order, try to allocate
            
            InStock = zp_db:q1("select stock_avail from shop_sku where id = $1", [Sku#sku_alloc.sku_id], Context),
            Alloc = min(InStock, Sku#sku_alloc.n),

            Args1 = [
                {allocated, Alloc},
                {backorder, 0} | Args
            ],

            % Try to allocate the calculated maximum quantity from the available stock of the sku
            case zp_db:q("
                    update shop_sku 
                    set stock_avail = stock_avail - $1 
                    where id = $2 and stock_avail >= $1", 
                    [Alloc, Sku#sku_alloc.sku_id],
                    Context) of

                1 ->
                    {ok, _} = zp_db:insert(shop_order_line, Args1, Context),
                    
                    %% Check if we need to add a backorder
                    case Alloc < Sku#sku_alloc.n of
                        true ->
                            Args2 = [
                                {allocated, 0},
                                {backorder, Sku#sku_alloc.n - Alloc} | Args ],
                            {ok, _} = zp_db:insert(shop_order_line, Args2, Context);
                        false ->
                            ok
                    end,
                    insert_order_lines(Id, T, Context);

                0 ->
                    % Conflict: could not allocate the stock, retry with a completely new allocation
                    throw({error, could_not_allocate})
            end
    end.


min(A,B) when A < B -> A;
min(_A,B) -> B.

