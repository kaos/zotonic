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

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = zp_convert:to_integer(proplists:get_value(id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {dialog_delete_username, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the username from the user id.
%% @spec event(Event, Context1) -> Context2
event({postback, {dialog_delete_username, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            case m_identity:get_username(Id, Context) of
                undefined ->
                    zp_render:wire({growl, [{text, "There is no username coupled to this person."}]}, Context);
                Username ->
                    DTitle = "Confirm delete",
                    Vars = [
                        {on_success, OnSuccess},
                        {id, Id},
                        {username, Username}
                    ],
                    {Html, Context1} = zp_template:render_to_iolist("_action_dialog_delete_username.tpl", Vars, Context),
                    zp_render:wire({dialog, [{title, DTitle}, {text, Html}]}, Context1)
            end;
        false ->
            zp_render:wire({growl, [{text, "Only an administrator can delete usernames."}, {type, "error"}]}, Context)
    end.
