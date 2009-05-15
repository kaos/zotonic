%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Validator for checking if a input has been filled/checked in.

-module(validator_presence).
-include("zophrenic.hrl").
-export([render_validator/5, validate/5]).

render_validator(presence, TriggerId, _TargetId, Args, Context)  ->
	JsObject = zp_utils:js_object(Args), 
	Script   = [<<"zp_add_validator(\"">>,TriggerId,<<"\", \"presence\", ">>, JsObject, <<");\n">>],
	{[], Script, Context}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptableValues} | {error,Id,Error}
%%          Error -> invalid | novalue | {script, Script}
validate(presence, Id, undefined, _Args, _Context) -> {error, Id, novalue};
validate(presence, Id, [],        _Args, _Context) -> {error, Id, novalue};
validate(presence, _Id, #upload{} = Value, _Args, _Context) ->
    {ok, Value};
validate(presence, Id, Value,     _Args, _Context) ->
    case zp_string:trim(Value) of
        [] -> {error, Id, invalid};
        _Trimmed -> {ok, Value}
    end.
