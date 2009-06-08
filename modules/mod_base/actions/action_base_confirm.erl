%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Original code copyright (c) 2008-2009 Rusty Klophaus

-module(action_base_confirm).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
    Postback    = proplists:get_value(postback, Args),
    Actions     = proplists:get_all_values(action, Args),
    Text        = proplists:get_value(text, Args, ""),
    
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, confirm, TriggerId, TargetId, undefined, Context),
	{ActionJS,Context1} = zp_render:render_actions(TriggerId, TargetId, Actions, Context),
	Script           = [
	                        <<"if (confirm(\"">>,zp_utils:js_escape(Text),<<"\")) {">>,
    	                        PostbackMsgJS,
    	                        ActionJS,
	                        $}
	                    ],
	{Script,Context1}.
	