%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus
%%
%% @doc Mark an element as a sortable.  A sortable is sorted inside a sorter

-module(scomp_base_sortable).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zotonic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

% -record(sortitem, {?ELEMENT_BASE(element_sortitem), tag, body=[] }).

render(Params, _Vars, Context, _State) ->
    Id       = proplists:get_value(id, Params),
    Tag      = proplists:get_value(tag, Params),
    Delegate = proplists:get_value(delegate, Params),
    Class    = proplists:get_all_values(class, Params),
    
	% Get properties...
	Delegate1    = case Delegate of
	                undefined -> z_context:get_resource_module(Context);
	                _ -> Delegate
	               end,

   case Id of
       undefined ->
           {error, "sortable scomp, please give the id of the sortable element"};
       _ ->
        	PickledTag  = z_utils:pickle({Tag,Delegate1,Id}),
        	Script      = io_lib:format("z_sortable($('#~s'), '~s');", [Id, PickledTag]),

            Actions = [
                        {script,    [{script, Script}]},
                        {add_class, [{class, ["sortable "|Class]}]}
                    ],

        	{ok, z_render:wire(Id, Actions, Context)}
    end.
