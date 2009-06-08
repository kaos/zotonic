%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-16
%%
%% @doc Simple interface to the pie type of Google chart.  Allows for simpler data entry.
%% Parameters:  data=[{label,value}, ...] colors=some_color
%% and then all parameters

-module(scomp_base_chart_pie).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zophrenic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, Vars, Context, State) ->
    Data   = proplists:get_value(data, Params, []),
    Colors = proplists:get_value(colors, Params),
    ThreeD = proplists:get_value(threed, Params, false),
    
    Params1 = proplists:delete(labels, 
                proplists:delete(data,
                    proplists:delete(colors,
                        proplists:delete(threed, Params)))),

    {Labels,Values} = case Data of
        [] -> {[],[]};
        [{_,_}|_] -> lists:unzip(Data);
        [[_,_]|_] -> lists:foldr(fun([A,B],{Acc,Bcc}) -> {[A|Acc],[B|Bcc]} end, {[],[]}, Data)
    end,
    Axes  = [ {axis, [{position,"bottom"},{labels,Labels}]} ],
    Type  = case erlydtl_runtime:is_false(ThreeD) of
        true -> "pie";
        false -> "pie3d"
    end,
    
    Data2 = case is_list(Colors) of
        true ->  [{data, [{values, Values}, {color, Colors}]}];
        false -> [{data, [{values, Values}]}]
    end,
    
    Params2 = [{axes,Axes}, {type,Type}, {data,Data2} | Params1],
    scomp_base_google_chart:render(Params2, Vars, Context, State).
