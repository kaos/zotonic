%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus
%%
%% @doc Make an element draggable
%%      {% @draggable id="xxx" tag="sometag" group="group1" group="group2" handle="selector" %}

-module(scomp_droppable).
-behaviour(gen_scomp).

-export([init/1, depends/2, code_change/3, terminate/1, render/3, event/2]).

-include("zophrenic.hrl").

init(_Args) -> {ok, []}.
depends(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

%% -record(droppable, {?ELEMENT_BASE(element_droppable), tag, body=[], accept_groups=all, active_class=active, hover_class=hover}).

render(Params, Context, _State) ->
    Id           = proplists:get_value(id, Params),
    Tag          = proplists:get_value(tag, Params),
    ActiveClass  = proplists:get_value(active_class, Params, "active"),
    HoverClass   = proplists:get_value(active_class, Params, "hover"),
    AcceptGroups = proplists:get_all_values(accept, Params),
    Delegate     = proplists:get_value(delegate, Params),
    
    case Id of
        undefined ->
            {error, "droppable scomp, please give the id of the droppable"};
        _ ->
            Delegate1 = case Delegate of
                            undefined -> zp_context:get_resource_module(Context);
                            _ -> Delegate
                        end,

        	AcceptGroups1       = groups_to_accept(AcceptGroups),
        	PickledPostbackInfo = zp_render:make_postback_info({Tag,Delegate1}, sort, Id, Id, ?MODULE, Context),
            
        	Script = io_lib:format( "zp_droppable($('#~s'), { activeClass: '~s', hoverClass: '~s', accept: '~s' }, '~s');", 
        	                        [Id, ActiveClass, HoverClass, AcceptGroups1, PickledPostbackInfo]),

            % Hook the actions to the element
            Actions = {script, [{script, Script}]},
            {ok, zp_render:wire(Id, Actions, Context)}
    end.


%% @doc Drops will be delegated to this event handler, which will call the postback resource.
event({postback, {DropTag,DropDelegate}, TriggerId, _TargetId}, Context) ->
	DragItem = zp_context:get_q("drag_item", Context),
	{DragTag,DragDelegate,DragId} = zp_utils:depickle(DragItem),

    Drop = #dragdrop{tag=DropTag, delegate=DropDelegate, id=TriggerId},
    Drag = #dragdrop{tag=DragTag, delegate=DragDelegate, id=DragId},

	try
	    Context1 = DropDelegate:event({drop, Drag, Drop}, Context),
	    
	    % also notify the dragged element that it has been dragged
	    try
	        DragDelegate:event({drag, Drag, Drop}, Context1)
	    catch
	        _M1:_E1 -> Context1
	    end

    catch
        _M2:E ->
            Error = io_lib:format("Error in routing drop to module \"~s\"; error: \"~p\"", [DropDelegate,E]),
            zp_render:wire({growl, [{text,Error}, {stay,1}]}, Context)
    end.


groups_to_accept([]) -> "*";
groups_to_accept(["all"]) -> "*";
groups_to_accept(["none"]) -> "";
groups_to_accept(Groups) ->
	Groups1 = lists:flatten([Groups]),
	Groups2 = [".drag_group_" ++ zp_convert:to_list(X) || X <- Groups1],
	string:join(Groups2, ", ").
