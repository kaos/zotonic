%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Disable an element.  Adds the 'disabled' attribute and adds the class 'disabled'.

-module(action_base_disable).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, Args, Context) ->
	action_base_jquery_effect:render_action(TriggerId, TargetId, [{type,disable}|Args], Context).
