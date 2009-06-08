%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Original code copyright (c) 2008-2009 Rusty Klophaus

-module(action_base_buttonize).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(TriggerId, TargetId, _Record, Context) -> 
	Actions = [
		{event, [
		            {type,mouseover}, 
		            {actions, {add_class, [{class,hover}]} }
		        ]},
		{event, [
		            {type,mouseout}, 
		            {actions, {remove_class, [{class,hover}]} }
		        ]},
		{event, [
		            {type,mousedown}, 
		            {actions, {add_class, [{class,clicked}]} }
		        ]},
		{event, [
		            {type,mouseup}, 
		            {actions, {remove_class, [{class,clicked}]} }
		        ]}
	],
	zp_render:render_actions(TriggerId, TargetId, Actions, Context).
