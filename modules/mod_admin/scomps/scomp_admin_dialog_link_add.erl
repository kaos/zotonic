%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Create a button for opening a dialog where the user can select an object for a new edge.

-module(scomp_admin_dialog_link_add).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, _Vars, Context, _State) ->
    Id        = z_ids:optid(proplists:get_value(id, Params)),
    SubjectId = z_convert:to_integer(proplists:get_value(subject_id, Params)), 
    Predicate = proplists:get_value(predicate, Params), 
    ElementId = proplists:get_value(element_id, Params),
    Anchor = z_tags:render_tag(
                        <<"a">>,
                        [
                    		{<<"id">>,    Id},
                    		{<<"href">>,  <<"javascript:void(0)">>},
                    		{<<"title">>, "add a connection"},
                    		{<<"class">>, "link-add"}
                    	],
                    	"+ add a connection"),

    Html = [<<"<span class=\"add-connection\">">>, Anchor, <<"</span>">>],
    Context1 = z_render:render(Html, Context),
    Context2 = z_render:wire(
                Id, 
                {event,[
                        {type, click}, 
                        {action, {dialog_link, [{subject_id, SubjectId}, {predicate, Predicate}, {element_id, ElementId}]} }
                ]}, Context1),
    {ok, Context2}.
