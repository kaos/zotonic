%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%%
%% @doc Open a dialog where the user can select an object

-module(action_admin_dialog_link).
-author("Marc Worrell <marc@worrell.nl").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    SubjectId = z_convert:to_integer(proplists:get_value(subject_id, Args)),
    Predicate = proplists:get_value(predicate, Args),
    ElementId = proplists:get_value(element_id, Args),
    Actions   = proplists:get_all_values(action, Args),
    Postback = {dialog_link, SubjectId, Predicate, ElementId, Actions},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event({postback, {dialog_link, SubjectId, Predicate, ElementId, Actions}, _TriggerId, _TargetId}, Context) ->
    Pred = m_predicate:get(Predicate, Context),
    Title = ["Add a connection: ", ?TR(proplists:get_value(title, Pred), Context)],
    Vars = [
        {subject_id, SubjectId},
        {predicate, Predicate},
        {element_id, ElementId},
        {action, Actions}
    ],
    z_render:dialog(Title, "_action_dialog_link.tpl", Vars, Context).
