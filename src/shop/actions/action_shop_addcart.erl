%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-13
%%
%% @doc 

-module(action_shop_addcart).
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
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback({shop_addcart, Id}, postback, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event({postback, {shop_addcart, Id}, _TriggerId, _TargetId}, Context) ->
    shop_cart:add_product(Id, Context),
    Title = ?TR(m_rsc:p(Id, title, Context), Context),
    Text  = ?TR("is toegevoegd aan de winkelmand.", Context),
    C1 = zp_render:wire({growl, [{text,[Title, 32, Text]},{stay,0},{type, "notice"}]}, Context),
    C2 = zp_render:wire("product-notice", {fade_in, []}, C1),
    shop_cart:tpl_sync_cart_info(C2).

