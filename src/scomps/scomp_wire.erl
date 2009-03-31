%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Wire an action to an element.

-module(scomp_wire).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/3]).

-include("zophrenic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, Context, _State) ->
    Id        = proplists:get_value(id, Params, <<>>),
    Type      = proplists:get_value(type,Params,click),
    TargetId  = proplists:get_value(target,Params,Id),
    Actions   = proplists:get_all_values(action,Params),
    Postback  = proplists:get_value(postback,Params),
    Delegate  = proplists:get_value(delegate,Params),

    Options   = [{action,X} || X <- Actions],
    Options1  = case Postback of
                	undefined -> Options;
                	Postback  -> [{postback,Postback} | Options]
                end,

    case Options1 of
        [] -> {error, "scomp wire: please give either an <em>action</em> or a <em>postback</em> parameter."};
        _  -> {ok, zp_render:wire(Id, TargetId, {event,[{type,Type},{delegate,Delegate}|Options1]}, Context)}
    end.
