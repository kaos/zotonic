%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Submits a form.  Either a targeted form or form closest to an id or the trigger.

-module(action_base_submit).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, _TargetId, Args, Context) ->
    case proplists:get_value(closest, Args) of
		undefined ->
		    case proplists:get_value(id, Args, proplists:get_value(target, Args)) of
		        undefined -> 
	            	{["$(\"#", TriggerId, "\").closest(\"form\").submit();"], Context};
		        Id ->
		            {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(Id)), <<"\").submit();">> ], Context}
		    end;
        true ->
            {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(TriggerId)), <<"\").closest(\"form\").submit();">> ], Context};
        CloseId ->
            {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(CloseId)), <<"\").closest(\"form\").submit();">> ], Context}
    end.
