%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Original code copyright (c) 2008-2009 Rusty Klophaus

-module(action_alert).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) -> 
    Text   = proplists:get_value(text, Args, ""),
	Script = io_lib:format("alert(\"~s\");", [zp_utils:js_escape(Text)]),
	{Script, Context}.
