%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-13
%%
%% @doc 

-module(action_shop_shop_cart_decr).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) -> 
    Id      = proplists:get_value(id, Args),
    Variant = z_convert:to_binary(proplists:get_value(variant, Args)),
    ?ASSERT(is_integer(Id), {id_must_be_integer, Id}),
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback({shop_cart_decr, Id, Variant}, postback, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event({postback, {shop_cart_decr, Id, Variant}, _TriggerId, _TargetId}, Context) ->
    N = shop_cart:decr_product(Id, Variant, Context),
    ContextDecr = z_render:update(
                            "count-"++integer_to_list(Id)++[$- | z_string:to_slug(z_convert:to_list(Variant))], 
                            integer_to_list(N),
                            Context),
    shop_cart:tpl_sync_cart_prices(ContextDecr).
