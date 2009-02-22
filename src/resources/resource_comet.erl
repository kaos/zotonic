%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Handles comet long polls from the user agent

-module(resource_comet).
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


%% Timeout for comet flush when there is no data, webmachine has a timeout of 60 seconds, so leave after 55
-define(COMET_FLUSH_EMPTY, 55000).

%% Timeout for comet flush when there is data, allow for 50 msec more to gather extra data before flushing
-define(COMET_FLUSH_DATA,  50).


init([]) -> {ok, []}.

malformed_request(ReqProps, _Context) ->
    Context = zp_context:new(ReqProps),
    {false, Context}.

forbidden(_ReqProps, Context) ->
    %% TODO: prevent that we make a new ua session or a new page session, fail when a new session is needed
    Context1 = zp_context:ensure_all(Context),
    {false, Context1}.

allowed_methods(_ReqProps, Context) ->
    {['POST'], Context}.

content_types_provided(_ReqProps, Context) -> 
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"application/x-javascript", false}], Context }.


%% @doc Collect all scripts to be pushed back to the user agent
process_post(ReqProps, Context) ->
    erlang:monitor(process, Context#context.page_pid),
    zp_session_page:comet_attach(self(), Context#context.page_pid),
    TRef = start_timer(?COMET_FLUSH_EMPTY),
    process_post_loop(ReqProps, {Context, TRef, false}).


%% @doc Wait for all scripts to be pushed to the user agent.
process_post_loop(ReqProps, {Context, TRef, HasData}) ->
    receive
        flush ->
            timer:cancel(TRef),
            zp_session_page:comet_detach(Context#context.page_pid),
            {true, Context};
        script_queued ->
            Scripts = zp_session_page:get_scripts(Context#context.page_pid),
            Req = ?REQ(ReqProps),
            Req:append_to_response_body(Scripts),
            TRef1 = case HasData of
                        true  -> TRef;
                        false -> reset_timer(?COMET_FLUSH_DATA, TRef)
                    end,
            process_post_loop(ReqProps, {Context,TRef1,true});
        {'DOWN', _MonitorRef, process, Pid, _Info} when Pid == Context#context.page_pid ->
            self() ! flush,
            process_post_loop(ReqProps, {Context,TRef,HasData})
    end.


start_timer(Delta) ->
    timer:send_after(Delta, flush).

reset_timer(Delta, TRef) ->
    timer:cancel(TRef),
    start_timer(Delta).
