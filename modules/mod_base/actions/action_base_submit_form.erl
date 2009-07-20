%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Submit a form, or the closest form to an id

-module(action_base_submit_form).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    case proplists:get_value(id, Args) of
        undefined -> 
            case proplists:get_value(closest, Args) of
                undefined -> 
                    {[], Context};
                CloseId ->
                    {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(CloseId)), <<"\").closest(\"form\").submit();">> ], Context}
            end;
        Id ->
            {[ $$, $(, $", $#, z_utils:js_escape(z_convert:to_list(Id)), <<"\").submit();">> ], Context}
    end.
