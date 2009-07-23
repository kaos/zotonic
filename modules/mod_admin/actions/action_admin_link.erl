%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%%
%% @doc Add an edge between two resources

-module(action_admin_link).
-author("Marc Worrell <marc@worrell.nl").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2,
    do_link/4
]).

render_action(TriggerId, TargetId, Args, Context) ->
    SubjectId = z_convert:to_integer(proplists:get_value(subject_id, Args)),
    ObjectId = z_convert:to_integer(proplists:get_value(object_id, Args)),
    Predicate = proplists:get_value(predicate, Args),
    ElementId = proplists:get_value(element_id, Args),
    Action = proplists:get_all_values(action, Args),
    
    Postback = {link, SubjectId, Predicate, ObjectId, ElementId, Action},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event({postback, {link, SubjectId, Predicate, ObjectId, ElementId, Action}, _TriggerId, _TargetId}, Context) ->
    do_link(SubjectId, Predicate, ObjectId, ElementId, Action, Context).


do_link(SubjectId, Predicate, ObjectId, Context) ->
    do_link(SubjectId, Predicate, ObjectId, undefined, [], Context).

do_link(SubjectId, Predicate, ObjectId, ElementId, Action, Context) ->
    case z_acl:rsc_editable(SubjectId, Context) of
        true ->
            {ok, _EdgeId} = m_edge:insert(SubjectId, Predicate, ObjectId, Context),
            Vars = [
                {subject_id, SubjectId},
                {predicate, Predicate},
                {object_id, ObjectId}
            ],
            Html  = z_template:render("_rsc_edge.tpl", Vars, Context),
            Title = z_html:strip(?TR(m_rsc:p(ObjectId, title, Context), Context)),
            ElementId1 = case ElementId of
                undefined -> "links-"++z_convert:to_list(SubjectId)++"-"++z_convert:to_list(Predicate);
                _ -> ElementId
            end,
            Context1 = z_render:insert_bottom(ElementId1, Html, Context),
            z_render:wire([{growl, [{text, "Added the connection to “"++z_convert:to_list(Title)++"”."}]} | Action], Context1);
        false ->
            z_render:growl_error("Sorry, you have no permission to add the connection.", Context)
    end.
    