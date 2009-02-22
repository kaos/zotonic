%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Original code copyright (c) 2008-2009 Rusty Klophaus

-module(action_validation_error).
-include("zophrenic.hrl").
-export([render_action/4]).


render_action(_TriggerId, _TargetId, Args, Context) -> 
	Text   = zp_utils:js_escape(proplists:get_value(text,Args,"")),
	Script = [
		    <<"var v = new LiveValidation(obj('me'), { onlyOnSubmit: true }); ">>,
		    <<"v.add(Validate.Custom, { against: wf_return_false, failureMessage: \"">>,Text,<<"~s\" }); ">>,
		    <<"v.validate(); ">>
	        ],
	{Script,Context}.
