%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Perform a redirect to another page.
%% @todo Make this a better redirect by including the host name of the site
%% @todo Allow the location/id types of redirect to have extra arguments

-module(action_base_redirect).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Script = case proplists:get_value(back, Args) of
        true ->
            "history.go(-1);";
        _ ->
            Location = case proplists:get_value(dispatch, Args) of
                undefined ->
                    case proplists:get_value(id, Args) of
                        undefined -> 
                            proplists:get_value(location, Args, "/");
                        Id ->
                            m_rsc:p(Id, page_url, Context)
                    end;
                DispatchString ->
                    Dispatch = z_convert:to_atom(DispatchString),
                    Args1 = proplists:delete(dispatch, Args),
                    z_dispatcher:url_for(Dispatch, Args1, none, Context)
            end,
        	[<<"window.location = \"">>,z_utils:js_escape(Location),$",$;]
    end,
	{Script, Context}.
    