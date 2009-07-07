%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-07
%%
%% @doc Delete a member from a group, no confirmation.

-module(action_admin_group_group_member_delete).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = zp_convert:to_integer(proplists:get_value(id, Args)),
    MemberId = zp_convert:to_integer(proplists:get_value(member_id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    ?DEBUG(OnSuccess),
    Postback = {group_member_delete, Id, MemberId, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Delete a predicate.
%% @spec event(Event, Context1) -> Context2
event({postback, {group_member_delete, Id, MemberId, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_group_role(leader, Id, Context) of
        true ->
            ok = m_group:delete_member(Id, MemberId, Context),
            zp_render:wire(OnSuccess, Context);
        false ->
            zp_render:wire({growl, [{text, "Only administrators or group leaders can remove members from groups."}, {type, "error"}]})
    end.
