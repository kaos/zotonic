%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell <marc@worrell.nl>
%% @date 2009-04-28
%%
%% @doc Redirect to resources depening on the content type requested.
%% @todo Split the redirect function to z_context and check for injection in the host header

-module(resource_id).

-author("Tim Benniks <tim@timbenniks.com>").
-author("Marc Worrell <marc@worrell.nl>").

-export([
    init/1,
    resource_exists/2,
    content_types_provided/2,
    to_html/2
]).

-include_lib("webmachine_resource.hrl").
-include_lib("zotonic.hrl").

init([]) -> {ok, []}.

resource_exists(ReqData, _Context) ->
    Context = z_context:new(ReqData, ?MODULE),
    ContextQs = z_context:ensure_qs(Context),
    Id = z_context:get_q("id", ContextQs),
    ?WM_REPLY(m_rsc:exists(Id, ContextQs), ContextQs).

content_types_provided(ReqData, Context) ->
   {[{"text/html", to_html}], ReqData, Context}.
 
to_html(ReqData, Context) ->
    Id       = z_context:get_q("id", Context),
    Location = m_rsc:p(Id, page_url, Context),
    Url = "http://" ++ wrq:get_req_header("host", ReqData) ++ Location,
    ReqData1 = wrq:set_resp_header("Location", Url, ReqData),
    ReqData2 = wrq:set_response_code(303, ReqData1),
    {{halt, 303}, ReqData2, Context}.
