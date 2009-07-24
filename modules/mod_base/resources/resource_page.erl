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
    z_auth:wm_is_authorized(false, visible, id, ReqData, Context).


%% @doc Show the page.  Add a noindex header when requested by the editor.
html(Context) ->
	Id = get_id(Context),
    Context1 = case z_convert:to_bool(m_rsc:p(Id, seo_noindex, Context)) of
        true ->  z_context:set_resp_header("X-Robots-Tag", "noindex", Context);
        false -> Context
    end,
	Template = z_context:get(template, Context1, "page.tpl"),
    Html = z_template:render(Template, [ {id, Id} | z_context:get_all(Context) ], Context1),
	z_context:output(Html, Context1).


%% @doc Fetch the id from the request or the dispatch configuration.
%% @spec get_id(Context) -> int()
get_id(Context) ->
    case z_context:get(id, Context) of
        undefined -> 
            z_convert:to_integer(z_context:get_q("id", Context));
        ConfId ->
            case m_rsc:name_to_id(ConfId, Context) of
                {ok, Id} -> Id;
                _ -> false
            end
    end.
