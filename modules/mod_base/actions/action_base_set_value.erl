%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Set the value of an input element

-module(action_base_set_value).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
    Id = proplists:get_value(id, Args, TargetId),
    Value = proplists:get_value(value, Args, ""),
    ValueString = z_convert:to_list(Value),
    {[ $$, $(, $", $#, z_utils:js_escape(Id), <<"\").val(\"">>, ValueString, $", $), $;  ], Context}.
