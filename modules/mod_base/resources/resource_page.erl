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

%% @doc Check if the id in the request exists.
resource_exists(ReqData, Context) ->
    Context1  = ?WM_REQ(ReqData, Context),
    ContextQs = zp_context:ensure_qs(Context1),
    Id = zp_convert:to_integer(zp_context:get_q("id", ContextQs)),
    try
        ?WM_REPLY(m_rsc:exists(Id, ContextQs), ContextQs)
    catch
        _:_ -> ?WM_REPLY(false, ContextQs)
    end.


%% @doc Check if the current user is allowed to view the resource. 
is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(false, visible, "id", ReqData, Context).


%% @doc Show the page.
html(Context) ->
	Id = zp_convert:to_integer(zp_context:get_q("id", Context)),
	Template = zp_context:get(template, Context, "page.tpl"),
    Html = zp_template:render(Template, [ {id, Id} | zp_context:get_all(Context) ], Context),
	zp_context:output(Html, Context).

