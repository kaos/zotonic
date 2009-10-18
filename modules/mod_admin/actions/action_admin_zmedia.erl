%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%%
%% @doc Add an edge between two resources

-module(action_admin_zmedia).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    Postback = {zmedia_choose, Args},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event({postback, {zmedia_choose, Args}, _TriggerId, _TargetId}, Context) ->
    z_render:dialog("Add/edit media", "_action_dialog_zmedia_choose.tpl", Args, Context).

%z_render:wire([{growl, [{text, "Yay."}]}], Context).


