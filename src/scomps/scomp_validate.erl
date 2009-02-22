%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Add a validation to an element

-module(scomp_validate).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_scomp).
-export([init/1, depends/2, code_change/3, terminate/1, render/3]).

-include("zophrenic.hrl").

init(_Args) -> {ok, []}.
depends(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, Context, _State) ->
    Id       = proplists:get_value(id, Params, <<>>),
    TargetId = proplists:get_value(target,Params,Id),
    Context1 = zp_render:validator(Id, TargetId, Params, Context),
    {ok, Context1}.
