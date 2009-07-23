%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-28
%%
%% @doc Open a dialog that asks confirmation to delete user credentials.

-module(action_admin_identity_dialog_delete_username).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {dialog_delete_username, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the username from the user id.
%% @spec event(Event, Context1) -> Context2
event({postback, {dialog_delete_username, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case z_acl:has_role(admin, Context) of
        true ->
            case m_identity:get_username(Id, Context) of
                undefined ->
                    z_render:growl("There is no username coupled to this person.", Context);
                Username ->
                    Vars = [
                        {on_success, OnSuccess},
                        {id, Id},
                        {username, Username}
                    ],
                    z_render:dialog("Confirm user deletion.", "_action_dialog_delete_username.tpl", Vars, Context)
            end;
        false ->
            z_render:growl_error("Only an administrator can delete usernames.", Context)
    end.
