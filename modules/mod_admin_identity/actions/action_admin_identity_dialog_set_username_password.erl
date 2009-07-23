%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-05-12
%%
%% @doc Open a dialog with some fields to add or change an username/password identity

-module(action_admin_identity_dialog_set_username_password).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

-define(PASSWORD_DOTS, "••••••").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    OnDelete = proplists:get_all_values(on_delete, Args),
    Postback = {set_username_password, Id, OnDelete},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {set_username_password, Id, OnDelete}, _TriggerId, _TargetId}, Context) ->
    {Username, Password} = case m_identity:get_username(Id, Context) of
                                undefined -> {[], []};
                                Name -> {Name, ?PASSWORD_DOTS}
                            end,
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {id, Id},
        {username, Username},
        {password, Password},
        {on_delete, OnDelete}
    ],
    z_render:dialog("Set username/ password.", "_action_dialog_set_username_password.tpl", Vars, Context);

event({submit, set_username_password, _TriggerId, _TargetId}, Context) ->
    Id = z_convert:to_integer(z_context:get_q("id", Context)),
    Username = z_context:get_q_validated("new_username", Context),
    Password = z_context:get_q_validated("new_password", Context),
    
    case z_acl:has_role(admin, Context) orelse z_acl:user(Context) == Id of
        true ->
            case Password of
                ?PASSWORD_DOTS ->
                    % Only change the username
                    case m_identity:set_username(Id, Username, Context) of
                        ok ->
                            z_render:wire([
                                {dialog_close, []},
                                {growl, [{text, "Changed the username."}]}
                                ], Context);
                        {error, eexist} ->
                            z_render:wire({growl, [{text, "The username is already in use, please try another."},{type, "error"}]}, Context)
                    end;
                _Password ->
                    case m_identity:set_username_pw(Id, Username, Password, Context) of
                        {error, _} ->
                            %% Assume duplicate key violation, user needs to pick another username.
                            z_render:growl_error("The username is in use, please supply an unique username.", Context);
                        ok ->
                            z_render:wire([
                                {dialog_close, []},
                                {growl, [{text, "The new username/ password has been set."}]}
                                ], Context)
                    end
            end;
        false ->
            z_render:growl_error("Only an administrator or the user him/herself can set a password.", Context)
    end.
