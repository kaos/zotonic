%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-05-01
%%
%% @doc Handles notifications received from the payment service provider Adyen.  The notifications are sent using a POST.

-module(resource_shop_adyen).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    dispatch/0,
    init/1, 
    is_authorized/2,
    allowed_methods/2,
    content_types_provided/2,
    process_post/2
]).


-include_lib("webmachine_resource.hrl").
-include_lib("include/zophrenic.hrl").


dispatch() ->
    [
        {shop_adyen_notification,  ["shop","adyen","notification"], ?MODULE, []}
    ].


init([]) -> {ok, []}.


%% Check the HTTP basic authentication supplied by Adyen
is_authorized(ReqData, _Context) ->
    Context1 = zp_context:new(ReqData, ?MODULE),
    Context2 = zp_context:ensure_qs(Context1),
    ?WM_REPLY(true, Context2).

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

content_types_provided(ReqData, Context) -> 
    %% When handling a POST the content type function is not used, so supply false for the function.
    { [{"text/plain", false}], ReqData, Context }.

process_post(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    QArgs = zp_context:get_q_all(Context1),

    % Handle the notification, save it in the database
    shop_adyen:notification(zp_context:get_q_all(Context1), Context1),

    RD  = zp_context:get_reqdata(Context1),
    RD1 = wrq:append_to_resp_body(<<"[accepted]">>, RD),
    ReplyContext = zp_context:set_reqdata(RD1, Context1),
    ?WM_REPLY(true, ReplyContext).


