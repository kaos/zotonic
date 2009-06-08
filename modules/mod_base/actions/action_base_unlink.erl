%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%%
%% @doc Remove an edge between two resources

-module(action_base_unlink).
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
    Hide = proplists:get_value(hide, Args),
    
    Postback = {unlink, SubjectId, Predicate, ObjectId, Hide},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Unlink the edge, on success show an undo message in the element with id "unlink-message"
%% @spec event(Event, Context1) -> Context2
event({postback, {unlink, SubjectId, Predicate, ObjectId, Hide}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:rsc_editable(SubjectId, Context) of
        true ->
            ok = m_edge:delete(SubjectId, Predicate, ObjectId, Context),
            Vars = [
                {subject_id, SubjectId},
                {predicate, Predicate},
                {object_id, ObjectId}
            ],
            Html = zp_template:render("_action_unlink_undo.tpl", Vars, Context),
            Context1 = zp_render:update("unlink-message", Html, Context),
            case Hide of
                undefined -> Context1;
                _ -> zp_render:wire({fade_out, [{target, Hide}]}, Context1)
            end;
        false ->
            zp_render:wire({growl, [{text, "Sorry, you have no permission to edit this page."},{type, "error"}]}, Context)
    end.
    