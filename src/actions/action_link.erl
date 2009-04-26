%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%%
%% @doc Add an edge between two resources

-module(action_link).
-author("Marc Worrell <marc@worrell.nl").
-include("zophrenic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    SubjectId = zp_convert:to_integer(proplists:get_value(subject_id, Args)),
    ObjectId = zp_convert:to_integer(proplists:get_value(object_id, Args)),
    Predicate = proplists:get_value(predicate, Args),
    
    Postback = {link, SubjectId, Predicate, ObjectId},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event({postback, {link, SubjectId, Predicate, ObjectId}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:rsc_editable(SubjectId, Context) of
        true ->
            Vars = [
                {subject_id, SubjectId},
                {predicate, Predicate},
                {object_id, ObjectId}
            ],
            Html  = zp_template:render("_rsc_edge.tpl", Vars, Context),
            Title = zp_html:strip(m_rsc:p(ObjectId, title, Context)),
            ElementId = "links-"++zp_convert:to_list(SubjectId)++"-"++zp_convert:to_list(Predicate),
            Context1 = zp_render:insert_bottom(ElementId, Html, Context),
            zp_render:wire({growl, [{text, "Added the connection to “"++zp_convert:to_list(Title)++"”."}]}, Context1);
        false ->
            zp_render:wire({growl, [{text, "Sorry, you have no permission to add the connection."},{type, "error"}]}, Context)
    end.
