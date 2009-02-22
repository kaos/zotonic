%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

-module(action_growl).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, _TargetId, Args, Context) ->
    Text   = proplists:get_value(text, Args, ""),
    Stay   = proplists:get_value(stay, Args, 0),

    TextJS = zp_utils:js_escape(Text),
    StayJS = if 
                Stay > 0 -> $1;
                true     -> $0
             end,
    Script = [<<"zp_growl_add(\"">>,TextJS,<<"\", ">>, StayJS, $), $;],
	{Script, Context}.
