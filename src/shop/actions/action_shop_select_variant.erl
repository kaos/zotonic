%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-22
%%
%% @doc Callback to set the html area with the variant select and price. 

-module(action_shop_select_variant).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) -> 
    Id      = proplists:get_value(id, Args),
    ?ASSERT(is_integer(Id), {id_must_be_integer, Id}),
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback({shop_select_variant, Id}, postback, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event({postback, {shop_select_variant, Id}, _TriggerId, _TargetId}, Context) ->
    Variant = zp_convert:to_binary(zp_context:get_q("triggervalue", Context)),
    Vars = [
        {rsc_id, Id},
        {variant, Variant}
    ],
    Html = zp_template:render("_product_price.tpl", Vars, Context),
    zp_render:update("product-price", Html, Context).
