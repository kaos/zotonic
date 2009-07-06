%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Set the value of an input element

-module(action_base_set_value).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
    Id = proplists:get_value(id, Args, TargetId),
    Value = proplists:get_value(value, Args, ""),
    ValueString = zp_convert:to_list(Value),
    {[ $$, $(, $", $#, zp_utils:js_escape(Id), <<"\").val(\"">>, ValueString, $", $), $;  ], Context}.
