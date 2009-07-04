%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%%
%% @doc Open a dialog where the user can select an object

-module(action_admin_link_dialog).
-author("Marc Worrell <marc@worrell.nl").
-include("zophrenic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    SubjectId = zp_convert:to_integer(proplists:get_value(subject_id, Args)),
    Predicate = proplists:get_value(predicate, Args),
    Actions   = proplists:get_all_values(action, Args),
    Postback = {link_dialog, SubjectId, Predicate, Actions},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event({postback, {link_dialog, SubjectId, Predicate, Actions}, _TriggerId, _TargetId}, Context) ->
    Pred = m_predicate:get(Predicate, Context),
    Title = ["Add a connection: ", ?TR(proplists:get_value(title, Pred), Context)],
    Vars = [
        {subject_id, SubjectId},
        {predicate, Predicate},
        {action, Actions}
    ],
    Html = zp_template:render("_action_link_dialog.tpl", Vars, Context),
    {Html1, Context1} = zp_render:render_to_string(Html, Context),
    zp_render:wire({dialog, [{title, Title}, {text, Html1}]}, Context1).
