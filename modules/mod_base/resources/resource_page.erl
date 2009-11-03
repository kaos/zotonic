%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Basic page

-module(resource_page).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    resource_exists/2,
    is_authorized/2
]).

-include_lib("resource_html.hrl").

%% @doc Check if the id in the request (or dispatch conf) exists.
resource_exists(ReqData, Context) ->
    Context1  = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    try
        ?WM_REPLY(m_rsc:exists(get_id(ContextQs), ContextQs), ContextQs)
    catch
        _:_ -> ?WM_REPLY(false, ContextQs)
    end.


%% @doc Check if the current user is allowed to view the resource. 
is_authorized(ReqData, Context) ->
    Context1  = ?WM_REQ(ReqData, Context),
    ContextQs = z_context:ensure_qs(Context1),
    z_auth:wm_is_authorized(false, visible, get_id(ContextQs), ReqData, Context).


%% @doc Show the page.  Add a noindex header when requested by the editor.
html(Context) ->
	Id = get_id(Context),
    Context1 = case z_convert:to_bool(m_rsc:p(Id, seo_noindex, Context)) of
        true ->  z_context:set_resp_header("X-Robots-Tag", "noindex", Context);
        false -> Context
    end,

	RenderArgs = [ {id, Id} | z_context:get_all(Context) ],
	RenderFunc = fun() ->
		Template = z_context:get(template, Context1, "page.tpl"),
	    z_template:render(Template, RenderArgs, Context1)
	end,

	%% EXPERIMENTAL:
	%%
	%% When the 'cache_anonymous_maxage' flag is set then we enable simple page caching.
	%% This does not take into account any query args and vary headers.
	%% @todo Add the 'vary' headers to the cache key
	MaxAge = z_context:get(cache_anonymous_maxage, Context1, 0),
	Html = case not z_auth:is_auth(Context1) andalso MaxAge > 0 of
		true -> 
		    z_depcache:memo(RenderFunc, {page_template_anonymous, RenderArgs}, MaxAge, [Id], Context1);
		false ->
			RenderFunc()
	end,

	z_context:output(Html, Context1).


%% @doc Fetch the id from the request or the dispatch configuration.
%% @spec get_id(Context) -> int() | false
get_id(Context) ->
    ReqId = case z_context:get(id, Context) of
        undefined -> z_context:get_q("id", Context);
        ConfId -> ConfId
    end,
    case m_rsc:name_to_id(ReqId, Context) of
        {ok, RscId} -> RscId;
        _ -> false
    end.
