%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Basic page

-module(resource_page).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    resource_exists/2
]).

-include_lib("resource_html.hrl").

resource_exists(ReqData, Context) ->
    Context1  = ?WM_REQ(ReqData, Context),
    ContextQs = zp_context:ensure_qs(Context1),
    try
        Id = list_to_integer(zp_context:get_q("id", ContextQs)),
        Ctx = zp_context:set(id, Id, ContextQs),
        ?WM_REPLY(m_rsc:exists(Id, Ctx), Ctx)
    catch
        _:_ -> ?WM_REPLY(false, ContextQs)
    end.

html(Context) ->
	Id = zp_context:get(id, Context),
	Vars = [
        {id, Id}
	],
    Html = zp_template:render("product.tpl", Vars, Context),
	zp_context:output(Html, Context).

