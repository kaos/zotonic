%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-28
%%
%% @doc Definitions for the shop module

-define(SHOP_ORDER_EXPIRE, ?HOUR).
-define(SHOP_ORDER_SHIPMENT, ?WEEK).

-record(sku_alloc, {sku_id, sku_nr, n=0, media_id,
                price_excl=0, price_incl=0, old_price_incl=0, old_price_excl=0, 
                backorder=false, order_costs=false}).
-record(cart, {idv, n, sku}).
