%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Validator for checking if an input value evaluates to "true"

-module(validator_acceptance).
-include("zophrenic.hrl").
-export([render_validator/5, validate/5]).

render_validator(acceptance, TriggerId, _TargetId, Args, Context)  ->
	JsObject   = zp_utils:js_object(Args), 
	Script     = [<<"zp_add_validator(\"">>,TriggerId,<<"\", \"acceptance\", ">>, JsObject, <<");\n">>],
	{[], Script, Context}.

%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptableValues} | {error,Id,Error}
%%          Error -> invalid | novalue | {script, Script}
validate(acceptance, Id, Value, _Args, _Context) ->
    case zp_utils:is_true(Value) of
        true  -> {ok, "1"};
        false -> {error, Id, invalid}
    end.
