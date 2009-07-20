%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-15
%%
%% @doc Add the tabs UI to an element

-module(scomp_base_tabs).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, _Vars, Context, _State) ->
    Id = proplists:get_value(id, Params),
    Script = [ "$('#", z_convert:to_list(Id), "').tabs();" ],
    {ok, z_render:wire({script, [{script, Script}]}, Context)}.

