%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Original code copyright (c) 2008-2009 Rusty Klophaus

-module(action_base_slide_toggle).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
	action_base_jquery_effect:render_action(TriggerId, TargetId, [{type,slide_toggle}|Args], Context).
