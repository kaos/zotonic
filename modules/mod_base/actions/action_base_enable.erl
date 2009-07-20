%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Enable an element.  Removes the 'disabled' attribute and removes the class 'disabled'.

-module(action_base_enable).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
	action_base_jquery_effect:render_action(TriggerId, TargetId, [{type,enable}|Args], Context).
