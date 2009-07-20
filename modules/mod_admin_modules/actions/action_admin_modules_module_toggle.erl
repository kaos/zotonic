%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-06-11
%%
%% @doc Activate/dactivate a module

-module(action_admin_modules_module_toggle).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Module = proplists:get_value(module, Args),
    Postback = {module_toggle, Module},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Delete a media.  After the deletion the user is redirected, and/or some items on the page are faded out.
%% @spec event(Event, Context1) -> Context2
event({postback, {module_toggle, Module}, TriggerId, _TargetId}, Context) ->
    case z_acl:has_role(admin, Context) of
        true ->
            Active = z_module_sup:active(Context),
            case lists:member(Module, Active) of
                true ->
                    z_module_sup:deactivate(Module, Context),
                    Context1 = z_render:update(TriggerId, "Activate", Context),
                    z_render:growl(["Deactivated ", atom_to_list(Module), "."], Context1);
                false ->
                    z_module_sup:activate(Module, Context),
                    Context1 = z_render:update(TriggerId, "Deactivate", Context),
                    z_render:growl(["Activated ", atom_to_list(Module), "."], Context1)
            end;
        false ->
            z_render:growl_error("You are not allowed to activate or deactivate modules.", Context)
    end.
