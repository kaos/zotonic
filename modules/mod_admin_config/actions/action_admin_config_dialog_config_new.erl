%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-08-07
%%
%% @doc Open a dialog with some fields to make a new configuration.

-module(action_admin_config_dialog_config_new).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {config_new_dialog, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new group form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {config_new_dialog, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {on_success, OnSuccess}
    ],
    z_render:dialog("Add configuration key.", "_action_dialog_config_new.tpl", Vars, Context);


event({submit, {config_new, Args}, _TriggerId, _TargetId}, Context) ->
    case z_acl:has_role(admin, Context) of
        true ->
            Module = z_string:to_name(z_context:get_q_validated("module", Context)),
            Key = z_string:to_name(z_context:get_q_validated("key", Context)),
            Value = z_context:get_q("val", Context, ""),
            OnSuccess = proplists:get_all_values(on_success, Args),

            case m_config:get_id(Module, Key, Context) of
                undefined ->
                    m_config:set_value(Module, Key, Value, Context),
                    z_render:wire([{dialog_close, []} | OnSuccess], Context);
                _ ->
                    z_render:growl_error("The config key already exists, please choose another key name.", Context)
            end;
        false ->
            z_render:growl_error("Only an administrator can add configuration keys.", Context)
    end.

