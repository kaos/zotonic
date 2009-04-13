%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-13
%%
%% @doc 

-module(action_shop_cart_incr).
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
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback({shop_cart_incr, Id}, postback, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event({postback, {shop_cart_incr, Id}, _TriggerId, _TargetId}, Context) ->
    N = shop_cart:add_product(Id, Context),
    ContextIncr = zp_render:update("count-"++integer_to_list(Id), integer_to_list(N), Context),
    shop_cart:tpl_sync_cart_prices(ContextIncr).
    
