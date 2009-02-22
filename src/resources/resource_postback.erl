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

malformed_request(ReqProps, _Context) ->
    Context  = zp_context:new(ReqProps),
    Context1 = zp_context:ensure_qs(Context),
    case zp_context:get_q("postback", Context1) of
        undefined ->
            {true, Context1};
        _ ->
            {false, Context1}
    end.

forbidden(_ReqProps, Context) ->
    %% TODO: prevent that we make a new ua session or a new page session, fail when a new session is needed
    Context1 = zp_context:ensure_all(Context),
    {false, Context1}.

allowed_methods(_ReqProps, Context) ->
    {['POST'], Context}.

content_types_provided(_ReqProps, Context) -> 
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"application/x-javascript", false}], Context }.

process_post(ReqProps, Context) ->
    Postback = zp_context:get_q("postback", Context),
    {EventType, TriggerId, TargetId, Tag, Module} = zp_utils:depickle(Postback),

    TriggerId1 = case TriggerId of
                    undefined -> zp_context:get_q("zp_trigger_id", Context);
                    _         -> TriggerId
                 end,

    ContextRsc   = zp_context:set_resource_module(Module, Context),
    EventContext = case EventType of
                    "submit" -> 
                        case zp_validation:validate_query_args(ContextRsc) of
                            {ok, ContextEval} ->    Module:event({submit, Tag, TriggerId1, TargetId}, ContextEval);
                            {error, ContextEval} -> ContextEval
                        end;
                    _ -> 
                        Module:event({postback, Tag, TriggerId1, TargetId}, ContextRsc)
                   end,

    Script      = zp_script:get_script(EventContext),
    CometScript = zp_session_page:get_scripts(Context#context.page_pid),

    Req = ?REQ(ReqProps),
    Req:append_to_response_body(Script),
    Req:append_to_response_body(CometScript),

    {true, EventContext}.
