%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009  Marc Worrell
%%
%% @doc Handle parameter validation of a request. Checks for the presence of zp_v elements containing validation information.

-module(zp_validation).
-export([validate_query_args/1]).

-include_lib("zophrenic.hrl").

%% @todo Translate unique id-names to base names (after validation)   #name -> fghw-name in postback+qs -> name in validated result

%% @spec validate_query_args(Context) -> {ok, NewContext} | {error, NewContext}
%% @doc Checks for zp_v arguments, performs enclosed checks and adds the validated terms to the zophrenic_qs_validated list.
%%      Errors are reported back to the user agent
validate_query_args(Context) ->
    case zp_context:get_context(zophrenic_qs_validated, Context) of
        undefined ->
            Validations = zp_context:get_q_all("zp_v", Context),
            Validated   = lists:map(fun(X) -> validate(X,Context) end, Validations),

            % format is like: [{"email",{ok,"me@example.com"}}]
            % Grep all errors, make scripts for the context var
            % Move all ok values to the zophrenic_qs_validated dict
            IsError  = fun 
                            ({_Id, {error, _, _}}) -> true;
                            (_X) -> false
                       end,
            GetValue = fun
                            ({Id, {ok, Value}}) -> {Id, lists:flatten(Value)}
                       end,

            {Errors,Values} = lists:partition(IsError, Validated),
            QsValidated     = dict:from_list(lists:map(GetValue, Values)),

            Context1 = zp_context:set_context(zophrenic_qs_validated, QsValidated, Context),
            Context2 = report_errors(Errors, Context1),
            
            case Errors of
                [] -> {ok, Context2};
                _  -> {error, Context2}
            end;
        _ ->
            {ok, Context}
    end.


%% @doc Add all errors as javascript message to the request result.
report_errors([], Context) -> 
    Context;
report_errors([{_Id, {error, _ErrId, {script, Script}}}|T], Context) ->
    Context1 = zp_script:add_script(Script, Context),
    report_errors(T, Context1);
report_errors([{_Id, {error, ErrId, Error}}|T], Context) ->
    Script   = [<<"zp_validation_error('">>, ErrId, <<"', \"">>, zp_utils:js_escape(atom_to_list(Error)),<<"\");\n">>],
    Context1 = zp_script:add_script(Script, Context),
    report_errors(T, Context1).


%% @doc Perform all validations
validate(Val, Context) ->
    [Id,Pickled]      = string:tokens(Val, ":"),
    {Id,Validations}  = zp_utils:depickle(Pickled),
    Value             = zp_context:get_q(Id, Context),

    %% Fold all validations, propagate errors
    ValidateF = fun
                    (_Validation,{error, _, _}=Error) -> Error;
                    (Validation,{ok, V}) ->
                        case Validation of
                            {Type,Module,Args} -> Module:validate(Type, Id, V, Args, Context);
                            {Type,Module}      -> Module:validate(Type, Id, V, [], Context)
                        end
                end,
    Validated = lists:foldl(ValidateF, {ok,Value}, Validations),
    {Id, Validated}.
