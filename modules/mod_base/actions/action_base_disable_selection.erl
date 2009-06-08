%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Original code copyright (c) 2008-2009 Rusty Klophaus

-module(action_base_disable_selection).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, TargetId, _Args, Context) -> 
	Script = [<<"zp_disable_selection($('#">>, TargetId, <<"'));">>],
    {Script,Context}.
