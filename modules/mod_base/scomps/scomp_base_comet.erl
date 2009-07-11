%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Start the comet loop, enabling push notifications from the server.

-module(scomp_base_comet).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zophrenic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(_Params, _Vars, Context, _State) ->
    {ok, zp_render:add_script(<<"zp_comet_start();">>, Context)}.
