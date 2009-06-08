%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-28
%%
%% @doc Delete a resource, no confirmation.

-module(action_base_delete_rsc).
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
    Postback = {delete_rsc, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Delete a resource.  After the deletion the user is redirected, and/or some items on the page are faded out.
%% @spec event(Event, Context1) -> Context2
event({postback, {delete_rsc, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:rsc_editable(Id, Context) of
        true ->
            ok = m_rsc:delete(Id, Context),
            lists:foldl(
                fun (Act, Ctx) ->
                    zp_render:wire(Act, Ctx)
                end,
                Context,
                lists:flatten(OnSuccess));
        false ->
            zp_render:wire({growl, [{text, "You are not allowed to delete this page."}, {type, "error"}]})
    end.
