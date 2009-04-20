%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-20
%%
%% @doc 

-module(m_shop_product).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    get_best_price/2,
    get_best_price_as_proplist/2,
    allocate_sku_price/3,
    allocate_sku/3
]).

-include_lib("zophrenic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Id, #m{value=undefined} = M, _Context) ->
    M#m{value=Id};
m_find_value(price, #m{value=Id}, Context) ->
    get_best_price_as_proplist(Id, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{}, _Context) ->
    undefined.


get_best_price_as_proplist(Id, Context) ->
    case get_best_price(Id, Context) of
        {undefined, undefined, undefined} -> 
            [];
        {Price, OldPrice, IsVariant} ->
            [{price, Price}, {old_price, OldPrice}, {is_variant, IsVariant}]
    end.

%% @doc Get the special price and the price. Special price is not defined if the product is not on sale.
%% @spec get_best_price(Id, Context) -> {SellingPrice, NormalPriceIfOnSale, IsVariant}
get_best_price(Id, Context) ->
     Skus = get_skus(Id, Context),
     skus_to_best_price(Skus).


get_skus(Id, Context) ->
    F = fun() ->
        zp_db:assoc("select * from shop_sku where rsc_id = $1 and available order by variant asc", [Id], Context)
    end,
    zp_depcache:memo(F, {skus, Id}, ?HOUR).
 

%% @doc Fetch the sku and its price that offers the best price for the product
%% @spec skus_to_best_price(Skus) -> {BestPrice, NormalPrice, IsVariant}
skus_to_best_price(Skus) ->
    {Date, _} = calendar:local_time(),
    skus_to_best_price(Skus, Date, {undefined, undefined, false}).
    
skus_to_best_price([], _Date, Best) ->
    Best;
skus_to_best_price([Sku|Rest], Date, Best) ->
    {Price, NormalPrice, IsVariant} = sku_to_price(Sku, Date),
    case Best of
        {undefined, _, _} ->
            skus_to_best_price(Rest, Date, {Price, NormalPrice, IsVariant});
        {BestPrice, _, _} when is_integer(BestPrice) andalso BestPrice > Price ->
            skus_to_best_price(Rest, Date, {Price, NormalPrice, IsVariant});
        _ ->
            skus_to_best_price(Rest, Date, Best)
    end.


%% @doc Fetch the current price of the sku and a flag if the sku is a variant
sku_to_price(Sku, Date) ->
    Price        = proplists:get_value(price_incl, Sku),
    SpecialPrice = proplists:get_value(special_price_incl, Sku),
    case SpecialPrice of
        N when N > 0 ->
            SpecialStart = proplists:get_value(special_start, Sku),
            SpecialEnd   = proplists:get_value(special_end, Sku),
            case SpecialStart =< Date andalso SpecialEnd >= Date of
                true -> {SpecialPrice, Price, is_variant(Sku)};
                false -> {Price, undefined, is_variant(Sku)}
            end;
        _ -> 
            {Price, undefined, is_variant(Sku)}
    end.


is_variant(Sku) ->
    Variant = proplists:get_value(variant, Sku),
    is_filled(Variant).

is_filled(<<>>) -> false;
is_filled([]) -> false;
is_filled(undefined) -> false;
is_filled(0) -> false;
is_filled(_) -> true.



%% @doc Allocate all skus.  Calculate the average allocated price and average old price.
%% @spec allocate_sku_price(Id, N, Context) -> {Id, N, BackorderN, AvgPrice, AvgOldPrice, SumPrice}
allocate_sku_price(Id, N, Context) ->
    {Skus, BoSku} = allocate_sku(Id, N, Context),
    case BoSku of
        {undefined, N, undefined, undefined} ->
            {error, no_sku_avail};
        {_, BoN, _, _} ->
            {SumPrice, SumOldPrice} = lists:foldl(
                    fun({_SNr, SN, SP, SOP}, {Tot, OldTot}) ->
                        {Tot + SN*SP, OldTot + SN * SOP}
                    end,
                    {0,0},
                    [BoSku|Skus]),
            AvgPrice    = round(SumPrice / N),
            AvgOldPrice = round(SumOldPrice / N),
            {
                Id,
                N,
                BoN,
                AvgPrice,
                AvgOldPrice,
                SumPrice
            }
    end.


%% @doc Try to allocate some skus for an order of a product. Returns the list of allocated skus and the to be 
%% back ordered sku. The skus are allocated starting with the cheapest.
%% @spec allocate_sku(Id, N, Context) -> {Allocated, {BoNr, BoN, BoPrice, BoOldPrice}}
allocate_sku(Id, N, Context) ->
    Skus = zp_db:q("
            select article_nr, stock_avail, price_incl, special_price_incl, special_start, special_end
            from shop_sku
            where stock > 0
              and rsc_id = $1
              and available", [Id], Context),
    {Date,_} = calendar:local_time(),
    PricedSkus = [ select_price(S, Date) || S <- Skus ],
    Sorted = lists:keysort(3, PricedSkus),
    % {Allocated, {BoNr, BoN, BoPrice, BoOldPrice}} = 
    allocate_sku1(Sorted, N, {[], undefined, undefined, undefined}).


    select_price({Nr, Stock, Price, SpecialPrice, Start, End}, Date) 
        when SpecialPrice > 0 andalso Start =< Date andalso End >= Date ->
            {Nr, Stock, SpecialPrice, Price};
    select_price({Nr, Stock, Price, _SpecialPrice, _Start, _End}, _Date) ->
        {Nr, Stock, Price, Price}.
    
    allocate_sku1([], N, {Alloc, LastNr, LastPrice, LastOldPrice}) ->
        {Alloc, {LastNr, N, LastPrice, LastOldPrice}};
    allocate_sku1([{Nr, Stock, Price, OldPrice}|_], N, {Alloc, _LastNr, _LastPrice, _LastOldPrice}) when Stock >= N ->
        {[{Nr, N, Price, OldPrice} | Alloc], {Nr, 0, Price, OldPrice}};
    allocate_sku1([{Nr, Stock, Price, OldPrice}|Rest], N, {Alloc, _LastNr, _LastPrice, _LastOldPrice}) ->
        allocate_sku1(Rest, N-Stock, {[{Nr, Stock, Price, OldPrice} | Alloc], Nr, Price, OldPrice}).
