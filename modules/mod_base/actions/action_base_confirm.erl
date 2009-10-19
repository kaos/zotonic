%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Original code copyright (c) 2008-2009 Rusty Klophaus

-module(action_base_confirm).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
    Postback  = proplists:get_value(postback, Args),
    Actions   = proplists:get_all_values(action, Args),
    Text      = proplists:get_value(text, Args, ""),
	Delegate  = proplists:get_value(delegate, Args),
    
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, confirm, TriggerId, TargetId, Delegate, Context),
	{ActionJS,Context1} = z_render:render_actions(TriggerId, TargetId, Actions, Context),
	Script           = [
	                        <<"if (confirm(\"">>,z_utils:js_escape(Text),<<"\")) {">>,
    	                        PostbackMsgJS,
    	                        ActionJS,
	                        $}
	                    ],
	{Script,Context1}.
	