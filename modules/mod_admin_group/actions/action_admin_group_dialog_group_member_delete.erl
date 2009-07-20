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

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    MemberId = z_convert:to_integer(proplists:get_value(member_id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {group_member_delete_dialog, Id, MemberId, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the member from the group.
%% @spec event(Event, Context1) -> Context2
event({postback, {group_member_delete_dialog, Id, MemberId, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case z_acl:has_group_role(leader, Id, Context) of
        true ->
            Vars = [
                {on_success, OnSuccess},
                {id, Id},
                {member_id, MemberId}
            ],
            z_render:dialog("Confirm removal of member", "_action_dialog_group_member_delete.tpl", Vars, Context);
        false ->
            z_render:growl_error("Only administrators or group leaders can remove members from groups.", Context)
    end.
