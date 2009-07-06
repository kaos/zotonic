%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Create a callback where extra name/value are merged with the other actions before they are performed.

-module(action_base_with_args).
-include("zophrenic.hrl").
-export([
    render_action/4
]).

render_action(TriggerId, TargetId, Args, Context) -> 
    Actions   = proplists:get_all_values(action, Args),
    ArgList   = proplists:get_all_values(arg, Args),
    ArgValue  = [ lookup_arg(Arg, Args) || Arg <- ArgList, Arg /= undefined ],
    Actions1  = [ append_args(Action, ArgValue) || Action <- lists:flatten(Actions), Action /= undefined ],
	zp_render:render_actions(TriggerId, TargetId, Actions1, Context).    

lookup_arg({ArgName, [{ArgValue,true}]}, Args) ->
    {ArgName, proplists:get_value(ArgValue, Args)}.
    
append_args({Action, ActionArgs}, Args) ->
    {Action, ActionArgs ++ Args}.
