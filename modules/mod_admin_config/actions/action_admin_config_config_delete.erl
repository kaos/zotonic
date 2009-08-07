%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-08-07
%%
%% @doc Delete a configuration, no confirmation.

-module(action_admin_config_config_delete).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Module = proplists:get_value(module, Args),
    Key = proplists:get_value(key, Args),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {config_delete, Module, Key, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Delete a group.
%% @spec event(Event, Context1) -> Context2
event({postback, {config_delete, Module, Key, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case z_acl:has_role(admin, Context) of
        true ->
            ok = m_config:delete(Module, Key, Context),
            z_render:wire(OnSuccess, Context);
        false ->
            z_render:growl_error("Only administrators can delete configurations.", Context)
    end.
