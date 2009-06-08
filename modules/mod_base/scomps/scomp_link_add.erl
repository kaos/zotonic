%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Create a button for opening a dialog where the user can select an object for a new edge.

-module(scomp_link_add).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zophrenic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, _Vars, Context, _State) ->
    Id        = zp_ids:optid(proplists:get_value(id, Params)),
    SubjectId = zp_convert:to_integer(proplists:get_value(subject_id, Params)), 
    Predicate = proplists:get_value(predicate, Params), 

    Anchor = zp_tags:render_tag(
                        <<"a">>,
                        [
                    		{<<"id">>,    Id},
                    		{<<"href">>,  <<"javascript:void(0)">>},
                    		{<<"title">>, "Add a connection"},
                    		{<<"class">>, "link-add"}
                    	],
                    	"Add a connection"),

    Html = [<<"<span>">>, Anchor, <<"</span>">>],
    Context1 = zp_render:render(Html, Context),
    Context2 = zp_render:wire(
                Id, 
                {event,[
                        {type, click}, 
                        {action, {link_dialog, [{subject_id, SubjectId}, {predicate, Predicate}]} }
                ]}, Context1),
    {ok, Context2}.
