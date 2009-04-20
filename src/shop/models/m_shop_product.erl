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
    get_best_price_as_proplist/2
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
    skus_to_best_price(Skus, Date, {undefined, undefined, []}).
    
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
