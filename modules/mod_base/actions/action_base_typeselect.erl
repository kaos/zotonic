%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%%
%% @doc Adds typeahead with a searchresult to an input box

-module(action_base_typeselect).
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
    Actions = proplists:get_all_values(action, Args),
    Postback = {typeselect, SubjectId, Predicate, Actions},
	{_PostbackMsgJS, PickledPostback} = zp_render:make_postback(Postback, key, TriggerId, TargetId, ?MODULE, Context),
	JS = [
	    <<"zp_typeselect(\"">>, TriggerId, $",$,,$", PickledPostback, <<"\");">>
	],
	{JS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event({postback, {typeselect, SubjectId, Predicate, Actions}, _TriggerId, TargetId}, Context) ->
    Text = zp_context:get_q("triggervalue", Context),
    SearchResult = zp_search:search({autocomplete, [{text, Text}]}, {1,20}, Context),
    Vars = [
        {subject_id, SubjectId},
        {predicate, Predicate},
        {result, SearchResult#search_result.result},
        {action, Actions}
    ],
    Html = zp_template:render("_action_typeselect_result.tpl", Vars, Context),
    zp_render:update(TargetId, Html, Context).
