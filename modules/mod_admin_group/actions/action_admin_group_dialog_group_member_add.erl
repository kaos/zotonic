%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-05
%%
%% @doc Open a dialog with some fields to add a member to a group

-module(action_admin_group_dialog_group_member_add).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = proplists:get_value(id, Args),
    Postback = {group_member_add_dialog, Id},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new group form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {group_member_add_dialog, Id}, _TriggerId, _TargetId}, Context) ->
    DTitle = "Add member to group",
    Vars = [
        {id, Id}
    ],
    Html = zp_template:render("_action_dialog_group_member_add.tpl", Vars, Context),
    {Html1, Context1} = zp_render:render_to_string(Html, Context),
    zp_render:wire({dialog, [{title, DTitle}, {text, Html1}]}, Context1);


%% @doc Add a member to a group.  The roles are in the request (they come from a form)
%% @spec event(Event, Context1) -> Context2
event({submit, group_member_add, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            GroupId  = zp_convert:to_integer(zp_context:get_q("id", Context)),
            MemberId = zp_convert:to_integer(zp_context:get_q("typeselect_id", Context)),
            case zp_context:get_q("member", Context) of
                "leader"   -> m_group:add_leader(GroupId, MemberId, Context);
                "observer" -> m_group:add_observer(GroupId, MemberId, Context);
                "member"   -> m_group:add_member(GroupId, MemberId, Context)
            end,

            zp_render:wire([
                    {growl, [{text, ["Set member status of ",?TR(m_rsc:p(MemberId, title, Context), Context)]}]},
                    {dialog_close, []},
                    {reload, []}], Context);

        false ->
            zp_render:wire({growl, [{text, "Only administrators can delete groups."}, {type, "error"}]}, Context)
    end.
