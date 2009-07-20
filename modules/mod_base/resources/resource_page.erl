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
    ContextQs = z_context:ensure_qs(Context1),
    Id = z_convert:to_integer(z_context:get_q("id", ContextQs)),
    try
        ?WM_REPLY(m_rsc:exists(Id, ContextQs), ContextQs)
    catch
        _:_ -> ?WM_REPLY(false, ContextQs)
    end.


%% @doc Check if the current user is allowed to view the resource. 
is_authorized(ReqData, Context) ->
    z_auth:wm_is_authorized(false, visible, "id", ReqData, Context).


%% @doc Show the page.
html(Context) ->
	Id = z_convert:to_integer(z_context:get_q("id", Context)),
	Template = z_context:get(template, Context, "page.tpl"),
    Html = z_template:render(Template, [ {id, Id} | z_context:get_all(Context) ], Context),
	z_context:output(Html, Context).

