%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Validator for checking if an input value matches a regular expression

-module(validator_base_format).
-include("zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(format, TriggerId, _TargetId, Args, Context)  ->
    Pattern  = proplists:get_value(pattern, Args),
    Negate   = proplists:get_value(negate, Args, false),
	JsObject = z_utils:js_object(Args),
	Script   = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"format\", ">>, JsObject, <<");\n">>],
	{[z_utils:is_true(Negate),Pattern], Script, Context}.


%% @spec validate(Type, TriggerId, Value, Args, Context) -> {ok,AcceptableValues} | {error,Id,Error}
%%          Error -> invalid | novalue | {script, Script}
validate(format, Id, Value, [Negate,Pattern], _Context) ->
    {Re,Options} = pattern_to_re(Pattern),
    Ok = not Negate,
    Match = case re:run(Value, Re, Options) of
                {match, _} -> true;
                nomatch -> false
            end,
    case Match of
        Ok -> {ok, Value};
        _  -> {error, Id, invalid}
    end.
    
%% @doc Translate a regular expression in javascript format to erlang re module format
pattern_to_re([$/|Rest]=Pattern) ->
    case string:rchr(Rest, $/) of
        0 -> 
            {Pattern,[]};
        N -> 
            {Re, [$/|Options]} = lists:split(N-1,Rest),
            ReOptions = [anycrlf|trans_options(Options, [])],
            {Re, ReOptions}
    end;
pattern_to_re(Pattern) ->
    {Pattern, []}.
    
trans_options([], Acc) -> 
    Acc;
trans_options([$i|T], Acc) -> 
    trans_options(T, [caseless|Acc]);
trans_options([$m|T], Acc) -> 
    trans_options(T, [multiline|Acc]);
trans_options([$s|T], Acc) -> 
    trans_options(T, [dotall|Acc]);
trans_options([$x|T], Acc) -> 
    trans_options(T, [extended|Acc]);
trans_options([$A|T], Acc) -> 
    trans_options(T, [anchored|Acc]);
trans_options([$D|T], Acc) -> 
    trans_options(T, [dollar_endonly|Acc]);
trans_options([$U|T], Acc) -> 
    trans_options(T, [ungreedy|Acc]);
trans_options([_|T], Acc) -> 
    trans_options(T, Acc).

