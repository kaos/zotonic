%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

-module(action_base_growl).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Text   = proplists:get_value(text, Args, ""),
    Stay   = proplists:get_value(stay, Args, 0),
    Type   = proplists:get_value(type, Args, "notice"),

    TextJS = zp_utils:js_escape(Text),
    StayJS = if 
                Stay > 0 -> $1;
                true     -> $0
             end,
	TypeJS = zp_utils:js_escape(Type),
	Script = [<<"zp_growl_add(\"">>,TextJS,<<"\", ">>, StayJS,<<",\"">>, TypeJS, $", $), $;],
	{Script, Context}.
