%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell <marc@worrell.nl>
%% @date 2009-04-28
%%
%% @doc Redirects to a resource when the path matches the page_path.
%% @todo Consult the dispatch lists and simulate the controller that would have been selected
%% for the default url. When doing so we prevent the uri from being changed, giving a nicer
%% user experience.

-module(resource_page_path).

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
    case m_rsc:page_path_to_id(wrq:disp_path(ReqData), Context) of
        {ok, Id} ->
            Context2 = z_context:set(id, Id, Context),
            ?WM_REPLY(true, Context2);
        {error, enoent} ->
            ?WM_REPLY(false, Context)
    end.

content_types_provided(ReqData, Context) ->
   {[{"text/html", to_html}], ReqData, Context}.
 
to_html(ReqData, Context) ->
    Id       = z_context:get(id, Context),
    Location = m_rsc:p(Id, default_page_url, Context),
    Url = "http://" ++ wrq:get_req_header("host", ReqData) ++ Location,
    ReqData1 = wrq:set_resp_header("Location", Url, ReqData),
    ReqData2 = wrq:set_response_code(303, ReqData1),
    {{halt, 303}, ReqData2, Context}.
