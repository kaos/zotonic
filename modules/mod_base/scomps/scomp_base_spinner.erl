%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Show the spinner element

-module(scomp_base_spinner).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, _Vars, _Context, _State) ->
    Image = proplists:get_value(image, Params, <<"/lib/images/spinner.gif">>),
    {ok, <<"<div id=\"spinner\" class=\"spinner\" style=\"display: none\"><image alt=\"activity indicator\" src=\"">>,Image,<<"\" /></div>">>}.
