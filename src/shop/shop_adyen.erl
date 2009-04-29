%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-29
%%
%% @doc Interface to the payment solution provider Adyen


-module(shop_adyen).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    payment_start/2,
    payment_completion/1,
    test/0
]).

-include_lib("zophrenic.hrl").
-include_lib("shop.hrl").

-define(PAYMENT_PAGE, "https://test.adyen.com/hpp/pay.shtml").
-define(MERCHANT_ACCOUNT, "HansStruijkFietsenNL").
-define(SKINCODE, "OnCtxIfz").
-define(SECRET, "hemaworst!").


%% @doc Build the payment uri to be used for paying the order.
%% @spec payment_uri(Id, Context) -> String
payment_start(Id, Context) ->
    
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
    Order = zp_db:assoc_row("select * from shop_order where id = $1", [Id], Context),

    MerchantReference = integer_to_list(Id),
    PaymentAmount = integer_to_list(proplists:get_value(total_price_incl, Order)),
    CurrencyCode = "EUR",
    ShipBeforeDate = erlydtl_dateformat:format(calendar:gregorian_seconds_to_datetime(NowSecs + ?SHOP_ORDER_SHIPMENT), "Y-m-d"),
    SkinCode = ?SKINCODE,
    MerchantAccount = ?MERCHANT_ACCOUNT,
    ShopperLocale = case zp_context:language(Context) of en -> "en_GB"; Lang -> atom_to_list(Lang) end,
    SessionValidity = erlydtl_dateformat:format(calendar:universal_time_to_local_time(proplists:get_value(expires, Order)), "c"),
    ShopperEmail = proplists:get_value(email, Order),
    ShopperReference = integer_to_list(proplists:get_value(visitor_id, Order)),
    AllowedMethods = "paypal,card,ideal",
    BlockedMethods = "",
    OrderData = "",

    MerchantSig = sign(?SECRET, [PaymentAmount, CurrencyCode, ShipBeforeDate, MerchantReference, 
                                SkinCode, MerchantAccount, SessionValidity, ShopperEmail, ShopperReference, 
                                AllowedMethods, BlockedMethods]),

    Args = [
        % Signed
        {"paymentAmount", PaymentAmount},
        {"currencyCode", CurrencyCode},
        {"shipBeforeDate", ShipBeforeDate},
        {"merchantReference", MerchantReference},
        {"skinCode", SkinCode},
        {"merchantAccount", MerchantAccount},
        {"sessionValidity", SessionValidity},
        {"shopperEmail", ShopperEmail},
        {"shopperReference", ShopperReference},
        {"allowedMethods", AllowedMethods},
        {"blockedMethods", BlockedMethods},

        % Not signed
        {"shopperLocale", ShopperLocale},
        {"orderData", OrderData},
        {"merchantSig", MerchantSig}
    ],

    ?PAYMENT_PAGE ++ "?" ++ mochiweb_util:urlencode(Args).



%% @doc Called when the payment completion is received, parameter is the context which contains all information.
%% @spec payment_completion(Context) -> {ok, OrderId} | {error, Reason}
payment_completion(Context) ->
    % http://127.0.0.1:8000/adyen/result?merchantReference=16&skinCode=OnCtxIfz&shopperLocale=nl&paymentMethod=ideal&authResult=AUTHORISED&pspReference=8612409656055305&merchantSig=8AjEyaKiP1xz%2Bp8Ef3BK50T%2Fu1Y%3D
    MerchantReference = zp_context:get_q("merchantReference", Context, ""),
    SkinCode      = zp_context:get_q("skinCode", Context, ""),
    _ShopperLocale = zp_context:get_q("shopperLocale", Context, ""),
    PaymentMethod = zp_context:get_q("paymentMethod", Context, ""),
    AuthResult    = zp_context:get_q("authResult", Context, ""),
    PspReference  = zp_context:get_q("pspReference", Context, ""),
    MerchantSig   = zp_context:get_q("merchantSig", Context),

    CheckSig = try
        MerchantSig = binary_to_list(sign(?SECRET, [AuthResult, PspReference, MerchantReference, SkinCode]))
    catch
        error:{badmatch, _} -> {error, sig_invalid}
    end,
    
    case CheckSig of
        {error, Reason} -> {error, Reason};
        _ ->
            % Signature valid, go ahead with this order.
            OrderId = list_to_integer(MerchantReference),
            shop_order:payment_completion(OrderId, AuthResult, PaymentMethod, PspReference, Context)
    end.



sign(Secret, Data) ->
    base64:encode( crypto:sha_mac(Secret, Data) ).
    
test() ->
    <<"x58ZcRVL1H6y+XSeBGrySJ9ACVo=">> =
    sign("Kah942*$7sdp0)", "10000GBP2007-10-20Internet Order 123454aD37dJATestMerchant2007-10-11T11:00:00Z"),
    ok.
    