%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Handles all ajax postback calls

%% Copyright 2009 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(resource_postback).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1, 
    forbidden/2,
    malformed_request/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2
    ]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zotonic.hrl").

init([]) -> {ok, []}.

malformed_request(ReqData, _Context) ->
    Context1 = z_context:new(ReqData, ?MODULE),
    Context2 = z_context:ensure_qs(Context1),
    case z_context:get_q("postback", Context2) of
        undefined ->
            ?WM_REPLY(true, Context2);
        _ ->
            ?WM_REPLY(false, Context2)
    end.

forbidden(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    %% TODO: prevent that we make a new ua session or a new page session, fail when a new session is needed
    Context2 = z_context:ensure_all(Context1),
    ?WM_REPLY(false, Context2).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) -> 
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"application/x-javascript", false}], ReqData, Context }.

process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Postback = z_context:get_q("postback", Context1),
    {EventType, TriggerId, TargetId, Tag, Module} = z_utils:depickle(Postback, Context),

    TriggerId1 = case TriggerId of
        undefined -> z_context:get_q("z_trigger_id", Context1);
        _         -> TriggerId
    end,

    ContextRsc   = z_context:set_resource_module(Module, Context1),
    EventContext = case EventType of
        "submit" -> 
            case z_validation:validate_query_args(ContextRsc) of
                {ok, ContextEval} ->   
                    Module:event({submit, Tag, TriggerId1, TargetId}, ContextEval);
                {error, ContextEval} ->
                    ContextEval
            end;
        _ -> 
            Module:event({postback, Tag, TriggerId1, TargetId}, ContextRsc)
    end,

    Script      = z_script:get_script(EventContext),
    CometScript = z_session_page:get_scripts(EventContext#context.page_pid),

    RD  = z_context:get_reqdata(EventContext),
    RD1 = case wrq:get_req_header("content-type", ReqData) of
        "multipart/form-data" ++ _ ->
            RDct = wrq:set_resp_header("Content-Type", "text/html; charset=utf-8", RD),
            wrq:append_to_resp_body(["<textarea>", Script, CometScript, "</textarea>"], RDct);
        _ ->
            wrq:append_to_resp_body([Script, CometScript], RD)
    end,
    ReplyContext = z_context:set_reqdata(RD1, EventContext),
    ?WM_REPLY(true, ReplyContext).
