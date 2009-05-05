%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-13
%%
%% @doc 

-module(action_shop_cart_delete).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) -> 
    Id      = proplists:get_value(id, Args),
    Variant = zp_convert:to_binary(proplists:get_value(variant, Args)),
    ?ASSERT(is_integer(Id), {id_must_be_integer, Id}),
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback({shop_cart_delete, Id, Variant}, postback, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event({postback, {shop_cart_delete, Id, Variant}, _TriggerId, _TargetId}, Context) ->
    shop_cart:del_product(Id, Variant, Context),
    ContextDel = zp_render:wire(
                        "cart-product-"++integer_to_list(Id)++[$- | zp_string:to_slug(zp_convert:to_list(Variant))], 
                        {slide_fade_out, []}, 
                        Context),
    shop_cart:tpl_sync_cart_prices(ContextDel).
