%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code by Rusty Klophaus

-module(validator_base_email).
-include("zotonic.hrl").
-export([render_validator/5, validate/5]).

render_validator(email, TriggerId, _TargetId, Args, Context)  ->
	JsObject = z_utils:js_object(Args), 
	Script   = [<<"z_add_validator(\"">>,TriggerId,<<"\", \"email\", ">>, JsObject, <<");\n">>],
	{[], Script, Context}.


%% @spec validate(Type, TriggerId, Values, Args, Context) -> {ok,AcceptableValues} | {error,Id,Error}
%%          Error -> invalid | novalue | {script, Script}
validate(email, Id, Value, _Args, _Context) ->
    case z_string:trim(Value) of
        [] -> {ok, []};
        Trimmed ->
            case re:run(Trimmed, re(), [extended]) of
                nomatch   -> {error, Id, invalid};
                {match,_} -> {ok, Trimmed}
            end
	end.


re() ->
    "^(
            (\"[^\"\\f\\n\\r\\t\\v\\b]+\")
        |   ([\\w\\!\\#\\$\\%\\&\'\\*\\+\\-\\~\\/\\^\\`\\|\\{\\}]+
                (\\.[\\w\\!\\#\\$\\%\\&\\'\\*\\+\\-\\~\\/\^\`\\|\\{\\}]+)*
            )
    )
    @
    (
        (
            ([A-Za-z0-9\\-])+\\.
        )+
        [A-Za-z\\-]{2,}
    )$".

%% Rusty's regexp: "([^@\"\'\\s]+)@(([-a-z0-9]+\\.)+[a-z]{2,})".
%% Complete regexp:
%%   ^((\"[^\"\f\n\r\t\v\b]+\")|([\w\!\#\$\%\&\'\*\+\-\~\/\^\`\|\{\}]+(\.[\w\!\#\$\%\&\'\*\+\-\~\/\^\`\|\{\}]+)*))@((\[(((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9])))\])|(((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9]))\.((25[0-5])|(2[0-4][0-9])|([0-1]?[0-9]?[0-9])))|((([A-Za-z0-9\-])+\.)+[A-Za-z\-]+))$
