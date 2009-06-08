%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Logoff the current user

-module(action_base_logoff).
-include("zophrenic.hrl").
-export([render_action/4, event/2]).

render_action(TriggerId, TargetId, _Args, Context) ->
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(logoff, undefined, TriggerId, TargetId, ?MODULE, Context),
	{[PostbackMsgJS], Context}.

event({postback, logoff, _TriggerId, _TargetId}, Context) ->
    Context1 = zp_auth:logoff(Context),
    zp_render:wire({reload, []}, Context1).
