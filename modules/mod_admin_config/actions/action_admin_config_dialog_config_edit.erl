%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-08-07
%%
%% @doc Open a dialog to change the value of a config key.

-module(action_admin_config_dialog_config_edit).
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
    Postback = {config_edit_dialog, Module, Key, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new group form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {config_edit_dialog, Module, Key, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    Vars = [
        {module, Module},
        {key, Key},
        {on_success, OnSuccess}
    ],
    z_render:dialog("Edit config value.", "_action_dialog_config_edit.tpl", Vars, Context);


%% @doc Add a member to a group.  The roles are in the request (they come from a form)
%% @spec event(Event, Context1) -> Context2
event({submit, {config_edit, Args}, _TriggerId, _TargetId}, Context) ->
    case z_acl:has_role(admin, Context) of
        true ->
            Value = z_context:get_q("val", Context, ""),
            Module = proplists:get_value(module, Args),
            Key = proplists:get_value(key, Args),
            OnSuccess = proplists:get_all_values(on_success, Args),
            m_config:set_value(Module, Key, Value, Context),
            z_render:wire([{dialog_close, []} | OnSuccess], Context);
        false ->
            z_render:growl_error("Only administrators can change the configuration.", Context)
    end.
