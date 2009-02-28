%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Original code copyright (c) 2008-2009 Rusty Klophaus

-module(action_jquery_effect).
-include("zophrenic.hrl").
-export([render_action/4]).

render_action(_TriggerId, TargetId, Args, Context) ->
	Type    = proplists:get_value(type, Args),
	Effect  = proplists:get_value(effect, Args),
	Speed   = proplists:get_value(speed, Args, "normal"),
	Class   = proplists:get_value(class, Args),
	Easing  = proplists:get_value(easing, Args),
	Effect  = proplists:get_value(effect, Args),
	Options = to_js(proplists:get_value(options, Args, [])),

	Script = case Type of
	    'show'   when Speed=="normal" -> "show();";
		'hide'   when Speed=="normal" -> "hide();";
		'show'          -> io_lib:format("show(~p);", [Speed]);
		'hide'          -> io_lib:format("hide(~p);", [Speed]);
		'slide_toggle'  -> io_lib:format("slideToggle(~p);", [Speed]);
		'toggle'        -> <<"toggle();">>;
		'add_class'     -> io_lib:format("addClass('~s');", [Class]);
		'remove_class'  -> io_lib:format("removeClass('~s');", [Class]);
		'fade_in'       -> io_lib:format("fadeIn(~p);", [Speed]);
		'fade_out'      -> io_lib:format("fadeOut(~p);", [Speed]);

        %% @todo check these, i think that with jQuery 1.3 they should be 'animate' with a js_object/2 output
		'effect'        -> io_lib:format("effect('~s', ~s, ~p);", [Effect, Options, Speed]);
		'animate'       -> io_lib:format("animate(~s, ~p, '~s');", [Options, Speed, Easing])
	end,

	Script2 = [<<"$('#">>,TargetId,<<"').">>,Script],
	
	{Script2, Context}.

	
to_js(Options) ->
	F = fun({Key, Value}) ->
		case Value of 
    		true ->
    			io_lib:format("~s: true", [Key]);
        	false ->
        		io_lib:format("~s: false", [Key]);
			V ->
				io_lib:format("~s: '~s'", [Key, zp_utils:js_escape(V)])
		end
	end,
	Options1 = [F(X) || X <- Options],
	Options2 = string:join(Options1, ","),
	io_lib:format("{ ~s }", [Options2]).
