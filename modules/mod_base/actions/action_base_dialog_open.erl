%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-27
%%
%% @doc Open a dialog with content from a template.

-module(action_base_dialog_open).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback({dialog, Args}, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the resource
%% @spec event(Event, Context1) -> Context2
event({postback, {dialog, Args}, _TriggerId, _TargetId}, Context) ->
    Title = proplists:get_value(title, Args, ""),
    {template, Template} = proplists:lookup(template, Args),
    z_render:dialog(Title, Template, Args, Context).

