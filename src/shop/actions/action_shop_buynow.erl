%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-13
%%
%% @doc Add something to the shopping cart and redirect to the cart page for checkout

-module(action_shop_buynow).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) -> 
    Id = proplists:get_value(id, Args),
    ?ASSERT(is_integer(Id), {id_must_be_integer, Id}),
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback({shop_buynow, Id}, postback, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event({postback, {shop_buynow, Id}, _TriggerId, _TargetId}, Context) ->
    shop_cart:add_product(Id, Context),
    Url = zp_dispatcher:url_for(shop_cart),
    zp_render:wire({redirect, [{location,Url}]}, Context).
