%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Validator for checking if the input is the same as another input.

-module(validator_base_confirmation).
-include("zophrenic.hrl").
-export([render_validator/5, validate/5]).

render_validator(confirmation, TriggerId, _TargetId, Args, Context)  ->
    MatchField = zp_utils:get_value(match, Args),
	JsObject   = zp_utils:js_object(Args), 
	Script     = [<<"zp_add_validator(\"">>,TriggerId,<<"\", \"confirmation\", ">>, JsObject, <<");\n">>],
	{[MatchField], Script, Context}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptableValues} | {error,Id,Error}
%%          Error -> invalid | novalue | {script, Script}
validate(confirmation, Id, Value, [MatchField], Context) ->
    MatchValue = zp_context:get_q(MatchField, Context),
    if 
        MatchValue =:= Value -> {ok, Value};
        true -> {error, Id, invalid}
    end.
