%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-04
%%
%% @doc Force a rescan of all modules, actions, templates etc. This is needed after a template, action or 
%% validation has been added.  It will also tell the dispatcher to reload all dispatch rules.

-module(action_admin_modules_module_rescan).
-author("Marc Worrell <marc@worrell.nl").
-include("zotonic.hrl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

render_action(TriggerId, TargetId, Args, Context) ->
    Actions = proplists:get_all_values(action, Args),
    Postback = {module_rescan, Actions},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Signal the module indexer to rescan all modules for actions, templates etc.
%% @spec event(Event, Context1) -> Context2
event({postback, {module_rescan, Actions}, _TriggerId, _TargetId}, Context) ->
    z_notifier:notify({module_ready}, Context),
    z_dispatcher:reload(Context),
    Context1 = z_render:growl("Module rescan is in progress.", Context),
    z_render:wire(Actions, Context1).
