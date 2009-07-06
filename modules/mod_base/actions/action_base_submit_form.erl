%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Submit a form, or the closest form to an id

-module(action_base_submit_form).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    case proplists:get_value(id, Args) of
        undefined -> 
            case proplists:get_value(closest, Args) of
                undefined -> 
                    {[], Context};
                CloseId ->
                    {[ $$, $(, $", $#, zp_utils:js_escape(zp_convert:to_list(CloseId)), <<"\").closest(\"form\").submit();">> ], Context}
            end;
        Id ->
            {[ $$, $(, $", $#, zp_utils:js_escape(zp_convert:to_list(Id)), <<"\").submit();">> ], Context}
    end.
