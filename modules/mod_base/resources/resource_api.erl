%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse <arjan@scherpenisse.net>
%% @date 2009-09-27
%%
%% @doc Entrypoint for API requests.

-module(resource_api).

-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([
    init/1,
    resource_exists/2,
    allowed_methods/2,
    process_post/2,
         is_authorized/2,
    content_types_provided/2,
    to_json/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init([]) -> {ok, []}.


allowed_methods(ReqData, _Context) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:ensure_qs(Context),
    Method   = z_context:get_q("method", Context1),
    TheMod   = z_context:get_q("module", Context1),
    Module  = list_to_atom("service_" ++ TheMod ++ "_" ++ Method),
    Context2 = z_context:set("module", Module, Context1),
    Context3 = z_context:set("partial_method", Method, Context2),
    try
        {z_service:http_methods(Module), ReqData, Context3}
    catch
        _X:_Y ->
            {['GET', 'HEAD', 'POST'], ReqData, Context3}
    end.
    

is_authorized(ReqData, Context) ->
    Module = z_context:get("module", Context),
    case mod_oauth:check_request_logon(ReqData, Context) of
        {none, Context2} ->
            case z_service:needauth(Module) of
                false ->
                    %% No auth needed; so we're authorized.
                    {true, ReqData, Context2};
                true ->
                    %% Authentication is required for this module...
                    mod_oauth:authenticate(z_service:method(Module) ++ ": " ++ z_service:title(Module) ++ "\n\nThis API call requires authentication.", ReqData, Context2)
            end;

        {true, Context2} ->
            %% OAuth succeeded; check whether we are allowed to exec this module
            ConsumerId = proplists:get_value(id, z_context:get("oauth_consumer", Context2)),
            case mod_oauth:is_allowed(ConsumerId, Module, Context2) of
                true ->
                    {true, ReqData, Context2};
                false ->
                    ReqData1 = wrq:set_resp_body("You are not authorized to execute this API call.\n", ReqData),
                    {{halt, 403}, ReqData1, Context2}
            end;
                
        {false, Response} ->
            Response
    end.


resource_exists(ReqData, Context) ->
    Module = z_context:get("module", Context),
    {lists:member(Module, z_service:all(Context)), ReqData, Context}.


content_types_provided(ReqData, Context) ->
    {[{"application/json", to_json}], ReqData, Context}.


api_error(HttpCode, ErrCode, Message, ReqData, Context) ->
    R = {struct, [{error, {struct, [{code, ErrCode}, {message, Message}]}}]},
    {{halt, HttpCode}, wrq:set_resp_body(mochijson:encode(R), ReqData), Context}.


api_result(ReqData, Context, Result) ->
    case Result of 
        {error, missing_arg, Arg} ->
            api_error(400, missing_arg, "Missing argument: " ++ Arg, ReqData, Context);

        {error, not_exists, Arg} ->
            api_error(400, not_exists, "Resource does not exist: " ++ Arg, ReqData, Context);

        {error, Err, _Arg} ->
            api_error(500, Err, "Generic error", ReqData, Context);

        Result2 ->
            try
                {{halt, 200}, wrq:set_resp_body(mochijson:encode(Result2), ReqData), Context}
            catch
                _E: R ->
                    ?DEBUG(R),
                    ReqData1 = wrq:set_resp_body("Internal JSON encoding error.\n", ReqData),
                    {{halt, 500}, ReqData1, Context}
            end
    end.
    

to_json(ReqData, Context) ->
    Module = z_context:get("module", Context),
    api_result(ReqData, Context, Module:process_get(ReqData, Context)).


process_post(ReqData, Context) ->
    Method       = list_to_atom(z_context:get_q("method", Context)),
    {ok, Module} = z_module_indexer:find(service, Method, Context),
    case Module:process_post(ReqData, Context) of
        ok ->
            {true, ReqData, Context};
        Result ->
            api_result(ReqData, Context, Result)
    end.
            
