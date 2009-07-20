%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-04
%%
%% @doc Delete a group, no confirmation.

-module(action_admin_group_group_delete).
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
    Postback = {group_delete, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Delete a group.
%% @spec event(Event, Context1) -> Context2
event({postback, {group_delete, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case z_acl:has_role(admin, Context) of
        true ->
            ok = m_rsc:delete(Id, Context),
            lists:foldl(
                fun (Act, Ctx) ->
                    z_render:wire(Act, Ctx)
                end,
                Context,
                lists:flatten(OnSuccess));
        false ->
            z_render:growl_error("Only administrators can delete groups.", Context)
    end.
