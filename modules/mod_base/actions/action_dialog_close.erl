%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-27
%%
%% @doc Close the dialog

-module(action_dialog_close).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4
]).

-include("zophrenic.hrl").

render_action(_TriggerId, _TargetId, _Args, Context) -> 
	{<<"zp_dialog_close();">>, Context}.
