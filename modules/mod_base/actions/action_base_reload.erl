%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Refresh the current page.

-module(action_base_reload).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, _Args, Context) ->
	{"window.location.reload(true);", Context}.
