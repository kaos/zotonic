%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Add a validation to an element

-module(scomp_base_validate).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_scomp).
-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, _Vars, Context, _State) ->
    Id       = proplists:get_value(id, Params, <<>>),
    TargetId = proplists:get_value(target,Params,Id),
    Context1 = z_render:validator(Id, TargetId, Params, Context),
    {ok, Context1}.
