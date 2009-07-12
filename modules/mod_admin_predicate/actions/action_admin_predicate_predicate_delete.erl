%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-28
%%
%% @doc Delete a predicate, no confirmation.

-module(action_admin_predicate_predicate_delete).
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
    Postback = {predicate_delete, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Delete a predicate.
%% @spec event(Event, Context1) -> Context2
event({postback, {predicate_delete, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            ok = m_rsc:delete(Id, Context),
            lists:foldl(
                fun (Act, Ctx) ->
                    zp_render:wire(Act, Ctx)
                end,
                Context,
                lists:flatten(OnSuccess));
        false ->
            zp_render:growl_error("Only administrators can delete predicates.", Context)
    end.
