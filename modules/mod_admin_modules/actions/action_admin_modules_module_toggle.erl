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

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Module = proplists:get_value(module, Args),
    Postback = {module_toggle, Module},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Delete a media.  After the deletion the user is redirected, and/or some items on the page are faded out.
%% @spec event(Event, Context1) -> Context2
event({postback, {module_toggle, Module}, TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            Active = zp_module_sup:active(Context),
            case lists:member(Module, Active) of
                true ->
                    zp_module_sup:deactivate(Module, Context),
                    Context1 = zp_render:update(TriggerId, "Activate", Context),
                    zp_render:wire({growl, [{text, "Deactivated "++atom_to_list(Module)++"."}]}, Context1);
                false ->
                    zp_module_sup:activate(Module, Context),
                    Context1 = zp_render:update(TriggerId, "Deactivate", Context),
                    zp_render:wire({growl, [{text, "Activated "++atom_to_list(Module)++"."}]}, Context1)
            end;
        false ->
            zp_render:wire({growl, [{text, "You are not allowed to activate or deactivate modules."}, {type, "error"}]}, Context)
    end.
