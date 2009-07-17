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
    Actions = proplists:get_all_values(action, Args),
    ActionsWithId = proplists:get_all_values(action_with_id, Args),
    Cats = proplists:get_all_values(cat, Args),
    Template = proplists:get_value(template, Args, "_action_typeselect_result.tpl"),
    Postback = {typeselect, Cats, Template, Actions, ActionsWithId},
	{_PostbackMsgJS, PickledPostback} = zp_render:make_postback(Postback, key, TriggerId, TargetId, ?MODULE, Context),
	JS = [
	    <<"zp_typeselect(\"">>, TriggerId, $",$,,$", PickledPostback, <<"\");">>
	],
	{JS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event({postback, {typeselect, Cats, Template, Actions, ActionsWithId}, _TriggerId, TargetId}, Context) ->
?DEBUG(Cats),
    Text = zp_context:get_q("triggervalue", Context),
    SearchResult = zp_search:search({autocomplete, [{cat,Cats}, {text, Text}]}, {1,20}, Context),
    Vars = [
        {result, SearchResult#search_result.result},
        {action, Actions},
        {action_with_id, ActionsWithId}
    ],
    Html = zp_template:render(Template, Vars, Context),
    zp_render:update(TargetId, Html, Context).
