%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Handles all ajax postback calls

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
-include_lib("include/zophrenic.hrl").

init([]) -> {ok, []}.

malformed_request(ReqData, _Context) ->
    Context1 = zp_context:new(ReqData, ?MODULE),
    Context2 = zp_context:ensure_qs(Context1),
    case zp_context:get_q("postback", Context2) of
        undefined ->
            ?WM_REPLY(true, Context2);
        _ ->
            ?WM_REPLY(false, Context2)
    end.

forbidden(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    %% TODO: prevent that we make a new ua session or a new page session, fail when a new session is needed
    Context2 = zp_context:ensure_all(Context1),
    ?WM_REPLY(false, Context2).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) -> 
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"application/x-javascript", false}], ReqData, Context }.

process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Postback = zp_context:get_q("postback", Context1),
    {EventType, TriggerId, TargetId, Tag, Module} = zp_utils:depickle(Postback),

    TriggerId1 = case TriggerId of
        undefined -> zp_context:get_q("zp_trigger_id", Context1);
        _         -> TriggerId
    end,

    ContextRsc   = zp_context:set_resource_module(Module, Context1),
    EventContext = case EventType of
        "submit" -> 
            case zp_validation:validate_query_args(ContextRsc) of
                {ok, ContextEval} ->   
                    Module:event({submit, Tag, TriggerId1, TargetId}, ContextEval);
                {error, ContextEval} ->
                    ContextEval
            end;
        _ -> 
            Module:event({postback, Tag, TriggerId1, TargetId}, ContextRsc)
    end,

    Script      = zp_script:get_script(EventContext),
    CometScript = zp_session_page:get_scripts(EventContext#context.page_pid),

    RD  = zp_context:get_reqdata(EventContext),
    RD1 = wrq:append_to_resp_body(Script, RD),
    RD2 = wrq:append_to_resp_body(CometScript, RD1),

    ReplyContext = zp_context:set_reqdata(RD2, EventContext),
    ?WM_REPLY(true, ReplyContext).
