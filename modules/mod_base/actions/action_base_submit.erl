%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Submits a form

-module(action_base_submit).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(TriggerId, _TargetId, Args, Context) ->
    case proplists:get_value(id, Args) of
        undefined -> {["$(\"#", TriggerId, "\").closest(\"form\").submit();"], Context};
        Id -> {["$(\"#", Id, "\").submit();"], Context}
    end.
        
