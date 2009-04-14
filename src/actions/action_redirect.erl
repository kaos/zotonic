%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Perform a redirect to another page.
%% @todo Make this a better redirect by including the host name of the site
%% @todo Allow the location/id types of redirect to have extra arguments

-module(action_redirect).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Location = case proplists:get_value(dispatch, Args) of
        undefined ->
            case proplists:get_value(id, Args) of
                undefined -> 
                    proplists:get_value(location, Args, "/");
                Id ->
                    m_rsc:p(Id, page_url, Context)
            end;
        DispatchString ->
            Dispatch = zp_convert:to_atom(DispatchString),
            Args1 = proplists:delete(dispatch, Args),
            zp_dispatcher:url_for(Dispatch, Args1, Context)
    end,
	Script   = [<<"window.location = \"">>,zp_utils:js_escape(Location),$",$;],
	{Script, Context}.
    