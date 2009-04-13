%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Perform a redirect to another page.
%% @todo Make this a better redirect by including the host name of the site

-module(action_redirect).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) -> 
    Location = case proplists:get_value(id, Args) of
        undefined -> proplists:get_value(location, Args, "/");
        Id -> m_rsc:p(Id, page_url, Context)
    end,
	Script   = [<<"window.location = \"">>,zp_utils:js_escape(Location),$",$;],
	{Script, Context}.
    