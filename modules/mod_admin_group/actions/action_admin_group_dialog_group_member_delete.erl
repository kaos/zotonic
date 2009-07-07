%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-07
%%
%% @doc Open a dialog that asks confirmation to delete a member from a group

-module(action_admin_group_dialog_group_member_delete).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    ?DEBUG(Args),
    Id = zp_convert:to_integer(proplists:get_value(id, Args)),
    MemberId = zp_convert:to_integer(proplists:get_value(member_id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {group_member_delete_dialog, Id, MemberId, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the member from the group.
%% @spec event(Event, Context1) -> Context2
event({postback, {group_member_delete_dialog, Id, MemberId, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_group_role(leader, Id, Context) of
        true ->
            DTitle = "Confirm removal of member",
            Vars = [
                {on_success, OnSuccess},
                {id, Id},
                {member_id, MemberId}
            ],
            Html = zp_template:render("_action_dialog_group_member_delete.tpl", Vars, Context),
            {Html1, Context1} = zp_render:render_to_string(Html, Context),
            zp_render:wire({dialog, [{title, DTitle}, {text, Html1}]}, Context1);
        false ->
            zp_render:wire({growl, [{text, "Only administrators or group leaders can remove members from groups."}, {type, "error"}]}, Context)
    end.
