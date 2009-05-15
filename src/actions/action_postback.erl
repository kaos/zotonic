%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

-module(action_postback).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) -> 
	Postback  = proplists:get_value(postback, Args),
    Delegate  = proplists:get_value(delegate, Args),
    Actions   = proplists:get_all_values(action, Args),

	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, undefined, TriggerId, TargetId, Delegate, Context),
	{ActionsJS, Context1} = zp_render:render_actions(TriggerId, TargetId, Actions, Context),
	{[ PostbackMsgJS, ActionsJS ], Context1}.
