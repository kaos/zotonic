%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-27
%%
%% @doc 

-module(action_base_dialog).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4
]).

-include("zophrenic.hrl").

render_action(_TriggerId, _TargetId, Args, Context) -> 
    Title  = proplists:get_value(title, Args, ""),
    Text   = proplists:get_value(text, Args, ""),
	Script = [<<"zp_dialog_open(\"">>,
	          zp_utils:js_escape(Title), $", $,, $",
	          zp_utils:js_escape(Text), $", $), $; ],
	{Script, Context}.
