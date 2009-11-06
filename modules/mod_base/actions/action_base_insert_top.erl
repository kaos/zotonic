%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-06
%% @doc Insert the result of a render action at the top of an HTML element.

-module(action_base_insert_top).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
    Update = proplists:get_value(update, Args, TargetId),
    {Html, ContextHtml} = case proplists:get_value(template, Args) of
        undefined ->
            { proplists:get_value(text, Args, ""), Context };
        Template ->
            z_template:render_to_iolist(Template, Args, Context)
    end,
    {[], z_render:insert_top(Update, Html, ContextHtml)}.
