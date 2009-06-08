%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Validator for checking if an input value is within a certain length range

-module(validator_base_length).
-include("zophrenic.hrl").
-export([render_validator/5, validate/5]).

render_validator(length, TriggerId, _TargetId, Args, Context)  ->
    Min        = proplists:get_value(minimum, Args),
    Max        = proplists:get_value(maximum, Args),
	JsObject   = zp_utils:js_object(Args),
	Script     = [<<"zp_add_validator(\"">>,TriggerId,<<"\", \"length\", ">>, JsObject, <<");\n">>],
	{[to_number(Min),to_number(Max)], Script, Context}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptableValues} | {error,Id,Error}
%%          Error -> invalid | novalue | {script, Script}
validate(length, Id, Value, [Min,Max], _Context) ->
    Len   = length(Value),
    MinOK = (Min == -1 orelse Len >= Min),
    MaxOK = (Max == -1 orelse Len =< Max),
    case MinOK andalso MaxOK of
        true  -> {ok, Value};
        false -> {error, Id, invalid}
    end.

to_number(undefined) -> 
    -1;
to_number(N) when is_integer(N) -> 
    N;
to_number(N) -> 
    {I,_Rest} = string:to_integer(N),
    I.
