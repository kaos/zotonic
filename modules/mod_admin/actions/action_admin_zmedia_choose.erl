%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%%
%% @doc A media item has been chosen for insertion in the body text.

-module(action_admin_zmedia_choose).
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
    z_render:wire({zmedia_has_chosen, Args}, Context).

%z_render:wire([{growl, [{text, "Yay."}]}], Context).


