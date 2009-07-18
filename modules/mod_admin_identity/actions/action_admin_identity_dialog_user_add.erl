%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-13
%%
%% @doc Add a complete new person and make it into an user.

-module(action_admin_identity_dialog_user_add).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {dialog_user_add, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


event({postback, {dialog_user_add, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            Vars = [
                {on_success, OnSuccess}
            ],
            zp_render:dialog("Add a new user", "_action_dialog_user_add.tpl", Vars, Context);
        false ->
            zp_render:growl_error("Only administrators can add users.", Context)
    end;

%% @doc Delete an username from an user.
%% @spec event(Event, Context1) -> Context2
event({submit, {user_add, Props}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            NameFirst = zp_context:get_q_validated("name_first", Context),
            NamePrefix = zp_context:get_q("surprefix", Context),
            NameSur = zp_context:get_q_validated("name_surname", Context),
            Title = case NamePrefix of
                [] -> [ NameFirst, " ", NameSur ];
                _ -> [ NameFirst, " ", NamePrefix, " ", NameSur ]
            end,

            PersonProps = [
                {is_published, true},
                {category, person},
                {title, lists:flatten(Title)},
                {name_first, NameFirst},
                {name_surname_prefix, NamePrefix},
                {name_surname, NameSur},
                {email, zp_context:get_q_validated("email", Context)},
                {group_id, list_to_integer(zp_context:get_q("group_id", Context))}
            ],
            
            F = fun(Ctx) ->
                case m_rsc:insert(PersonProps, Ctx) of
                    {ok, PersonId} ->
                        Username = zp_context:get_q_validated("new_username", Ctx),
                        Password = zp_context:get_q_validated("new_password", Ctx),
                        MemberId = list_to_integer(zp_context:get_q("member_id", Ctx)),
                        case m_group:add_member(MemberId, PersonId, Ctx) of
                            {ok, _RscGroupId} -> nop;
                            {error, MemberReason} -> throw({error, MemberReason})
                        end,
                        case m_identity:set_username_pw(PersonId, Username, Password, Ctx) of
                            ok -> {ok, PersonId};
                            {error, PWReason} -> throw({error, PWReason})
                        end;
                    {error, InsReason} ->
                        throw({error, InsReason})
                end
            end,
            
            case zp_db:transaction(F, Context) of
                {ok, _PersonId} ->
                    Context1 = zp_render:growl(["Created the user ",zp_html:escape(Title), "."], Context),
                    zp_render:wire(proplists:get_all_values(on_success, Props), Context1);
                {rollback, {Error, _CallStack}} ->
                    case Error of
                        {error, eexist} ->
                            zp_render:growl_error("Duplicate username, please choose another username.", Context);
                        {error, eacces} ->
                            zp_render:growl_error("You are not allowed to create the person page.", Context);
                        _OtherError ->
                            ?DEBUG(Error),
                            zp_render:growl_error("Could not create the user. Sorry.", Context)
                    end
            end;
        false ->
            zp_render:growl_error("Only administrators can add users.", Context)
    end.
