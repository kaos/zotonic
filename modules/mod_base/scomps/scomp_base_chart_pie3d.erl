%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-16
%%
%% @doc Simple interface to the pie3d type of Google chart.  Allows for simpler data entry.
%% Parameters:  data=[{label,value}, ...] colors=some_color
%% and then all parameters

-module(scomp_base_chart_pie3d).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, Vars, Context, State) ->
    scomp_base_chart_pie:render([{threed,true}|Params], Vars, Context, State).
