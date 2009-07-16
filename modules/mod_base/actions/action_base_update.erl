%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-16
%% @doc Replace the content of the target with the result of a render action.

-module(action_base_update).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
    Update = proplists:get_value(update, Args, TargetId),
    {Html, ContextHtml} = case proplists:get_value(template, Args) of
        undefined ->
            { proplists:get_value(text, Args, ""), Context };
        Template ->
            zp_template:render_to_iolist(Template, Args, Context)
    end,
    {[], zp_render:update(Update, Html, ContextHtml)}.
