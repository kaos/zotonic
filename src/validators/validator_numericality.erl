%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Validator for checking if an input value is a number and within a certain range.
%%      At the moment this function only accepts integers

-module(validator_numericality).
-include("zophrenic.hrl").
-export([render_validator/5, validate/5]).

render_validator(numericality, TriggerId, _TargetId, Args, Context)  ->
    Is = proplists:get_value(is, Args),
    {Min,Max} = case Is of
                    undefined -> { proplists:get_value(minimum, Args), proplists:get_value(maximum, Args) };
                    _ -> {Is,Is}
                end,
	JsObject   = zp_utils:js_object([{onlyInt,true}|Args]),
	Script     = [<<"zp_add_validator(\"">>,TriggerId,<<"\", \"numericality\", ">>, JsObject, <<");\n">>],
	{[to_number(Min),to_number(Max)], Script, Context}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptableValues} | {error,Id,Error}
%%          Error -> invalid | novalue | {script, Script}
validate(numericality, Id, Value, [Min,Max], _Context) ->
    Trimmed = zp_utils:trim(Value),
    case string:to_integer(Trimmed) of
        {error,_Error} -> 
            % Not a number
            {error, Id, invalid};
        {Num,[]} ->
            MinOK = (Min == -1 orelse Num >= Min),
            MaxOK = (Max == -1 orelse Num =< Max),
            case MinOK andalso MaxOK of
                true  -> {ok, Value};
                false -> {error, Id, invalid}
            end;
        {_Num, _} ->
            % Has some trailing information 
            {error, Id, invalid}
    end.


to_number(undefined) -> 
    -1;
to_number(N) when is_float(N) -> 
    round(N);
to_number(N) when is_integer(N) -> 
    N;
to_number(N) -> 
    {I,_Rest} = string:to_integer(N),
    I.
