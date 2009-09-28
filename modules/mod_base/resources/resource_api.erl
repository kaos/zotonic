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
    content_types_provided/2,
    to_json/2,
    to_jsonp/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init([]) -> {ok, []}.


allowed_methods(ReqData, Context) ->
    {['POST', 'GET', 'HEAD'], ReqData, Context}.
    

resource_exists(ReqData, _Context) ->
    Context  = z_context:new(ReqData, ?MODULE),
    Context1 = z_context:ensure_qs(Context),
    Method   = z_context:get_q("method", Context1),
    Module   = z_context:get_q("module", Context1),
    All      = z_module_indexer:find_all(service, list_to_atom(Method), Context1),
    ToCheck  = list_to_atom("service_" ++ Module ++ "_" ++ Method),
    {Found, Context2} = case lists:any(fun(X) -> ToCheck == X end, All) of
                            true ->
                                {true, z_context:set("module", ToCheck, Context1)};
                            false -> 
                                {false, Context1}
                        end,
    ?WM_REPLY(Found, Context2).


content_types_provided(ReqData, Context) ->
   {[{"text/x-json", to_json},
     {"application/json", to_json},
     {"text/javascript", to_jsonp},
     {"text/xml", to_xml}], ReqData, Context}.


to_json(ReqData, Context) ->
    Module = z_context:get("module", Context),
    Result = Module:process_get(ReqData, Context),
    {rfc4627:encode(Result), ReqData, Context}.


to_jsonp(ReqData, Context) ->
    Module   = z_context:get("module", Context),
    Callback = z_context:get_q("callback", Context),
    Result = Module:process_get(ReqData, Context),
    {Callback ++ "(" ++ rfc4627:encode(Result) ++ ");", ReqData, Context}.


process_post(ReqData, Context) ->
    Method       = list_to_atom(z_context:get_q("method", Context)),
    {ok, Module} = z_module_indexer:find(service, Method, Context),
    ?DEBUG(rfc4627:decode(wrq:req_body(ReqData))),
    Module:process_post(ReqData, Context),
    {true, ReqData, Context}.
