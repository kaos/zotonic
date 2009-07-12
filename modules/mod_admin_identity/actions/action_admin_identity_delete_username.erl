%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-07
%%
%% @doc Delete username from an user, no confirmation.

-module(action_admin_identity_delete_username).
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
    Postback = {delete_username, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Delete an username from an user.
%% @spec event(Event, Context1) -> Context2
event({postback, {delete_username, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            m_identity:delete_username(Id, Context),
            zp_render:wire([{growl, [{text, "Username has been deleted."}]} | OnSuccess], Context);
        false ->
            zp_render:growl_error("Only administrators can delete usernames.", Context)
    end.
