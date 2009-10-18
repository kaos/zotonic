%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse

-module(action_admin_zmedia_has_chosen).
-include("zotonic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Id   = z_convert:to_list(proplists:get_value(id, Args, "")),
	Script = [<<"z_dialog_close();window.z_choose_zmedia(\"">>,z_utils:js_escape(Id),<<"\");">>],
	{Script, Context}.
