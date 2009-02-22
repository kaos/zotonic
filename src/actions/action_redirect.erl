%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Perform a redirect to another page.
%% @todo Make this a better redirect by including the host name of the site

-module(action_redirect).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) -> 
    Location = proplists:get_value(location, Args, "/"),
	Script   = [<<"window.location = \"">>,zp_utils:js_escape(Location),$",$;],
	{Script, Context}.
    