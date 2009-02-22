%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus
%%
%% @doc Make an element draggable
%%      {% @draggable id="xxx" tag="sometag" group="group1" group="group2" handle="selector" %}

-module(scomp_draggable).
-behaviour(gen_scomp).

-export([init/1, depends/2, code_change/3, terminate/1, render/3]).

-include("zophrenic.hrl").

init(_Args) -> {ok, []}.
depends(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

%% -record(draggable, {?ELEMENT_BASE(element_draggable), tag, body=[], group, handle, clone=true, revert=true}).

render(Params, Context, _State) ->
    Id      = proplists:get_value(id, Params),
    Tag     = proplists:get_value(tag, Params),
    Clone   = proplists:get_value(clone, Params, true),
    Revert  = proplists:get_value(revert, Params, "true"),
    Handle  = proplists:get_value(handle, Params),
    Groups  = proplists:get_all_values(group, Params),
    Opacity = proplists:get_value(opacity, Params, "0.8"),
    Delegate= proplists:get_value(delegate, Params),
    
    Groups1 = case Groups of
                [] -> ["dragdrop"];
                _ -> Groups
               end,
               
	% Get properties...
	Delegate1    = case Delegate of
	                undefined -> zp_context:get_resource_module(Context);
	                _ -> Delegate
	               end,
	PickledTag   = zp_utils:pickle({Tag,Delegate1,Id}),
	GroupClasses = groups_to_classes(Groups1),

	Helper       =  case zp_utils:is_true(Clone) of
            	        true  -> "'clone'";
            		    false -> "'original'"
            	    end,
	
	Revert       =  case zp_convert:to_list(Revert) of
                		"true"    -> "true";
                		"false"   -> "false";
                		"valid"   -> "'valid'";
                		"invalid" -> "'invalid'"
                    end,
    
    HandleText   = case Handle of
                        undefined -> "null";
                        []        -> "null";
                        <<>>      -> "null";
                        _ -> [$',zp_utils:js_escape(Handle),$']
                    end,

	% Write out the script to make this element draggable...
	Script = io_lib:format("zp_draggable($('#~s'), { handle: ~s, helper: ~s, revert: ~s, opacity: ~s, scroll: true, cursor: 'pointer' }, '~s');", [
            		Id, 
            		HandleText, 
            		Helper, 
            		Revert,
            		Opacity,
            		PickledTag
            	]),

    % Hook the actions to the element
    Actions = [
                {script,    [{script, Script}]},
                {add_class, [{class, GroupClasses}]}
            ],
    {ok, zp_render:wire(Id, Actions, Context)}.

	
groups_to_classes([]) -> "";
groups_to_classes(undefined) -> "";
groups_to_classes(Groups) ->
	Groups1 = ["drag_group_" ++ zp_convert:to_list(X) || X <- Groups],
	string:join(Groups1, " ").
	
