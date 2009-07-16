% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2009 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module(zp_script).
-include("zophrenic.hrl").
-export ([
	add_script/2,
	get_script/1,
	get_page_startup_script/1,
	add_content_script/2
]).


add_content_script([], Context) -> 
    Context;
add_content_script(Script, Context) ->
    Context#context{content_scripts=[Script, "\n" | Context#context.content_scripts]}.

add_script([], Context) -> 
    Context;
add_script(Script, Context) ->
    Context#context{scripts=[Script, "\n" | Context#context.scripts]}.

get_page_startup_script(Context) ->
    case Context#context.page_id of
        undefined ->
            %% No page id, so no comet loop started and generated random page id for postback loop
            [   
                ?SESSION_PAGE_Q, $=, $", zp_ids:id(), $", $;, 
                <<"\nzp_postback_loop();\n">>
            ];
        PageId ->
            %% When the browsers start to support enough connections then we can also start the comet loop here.
            %% To start the comet loop, call: zp_comet_start().
            [   ?SESSION_PAGE_Q, $=, $", PageId, $", $;, 
                <<"\nzp_postback_loop();\n">>
            ]
    end.

get_script(Context) -> 
    Context1 = Context#context{scripts=[], content_scripts=[]},

	% Translate updates to content scripts
	Update2Script = fun({TargetId, Terms, JSFormatString}, C) ->
            		    {Html,C1} = zp_render:render_to_iolist(Terms, C),
            		    Script    = io_lib:format(JSFormatString, [TargetId, zp_utils:js_escape(Html)]),
            		    add_content_script(Script, C1)
            	    end,

    Context2 = lists:foldl(Update2Script, Context1#context{updates=[]}, lists:flatten(Context1#context.updates)),

	% Translate actions to scripts
	Action2Script = fun({TriggerID, TargetID, Actions}, C) ->
		                {Script,C1} = zp_render:render_actions(TriggerID, TargetID, Actions, C),
		                add_script(Script, C1)
	                end,

    Context3 = lists:foldl(Action2Script, Context2#context{actions=[]}, lists:flatten(Context2#context.actions)),
    
	% Translate validators to scripts
    Validator2Script = fun({TriggerId, TargetId, Validator}, C) ->
                            {Script,C1} = zp_render:render_validator(TriggerId, TargetId, Validator, C),
                            add_script(Script, C1)
                       end,

    Context4 = lists:foldl(Validator2Script, Context3, lists:flatten(Context3#context.validators)),
    
    % Finally fetch any updates that resulted from the actions or validators
    Context5 = lists:foldl(Update2Script, Context4, lists:flatten(Context4#context.updates)),
    
    [   
        lists:reverse(Context#context.content_scripts),
        lists:reverse(Context#context.scripts),
        lists:reverse(Context5#context.content_scripts),
        lists:reverse(Context5#context.scripts)
    ].

