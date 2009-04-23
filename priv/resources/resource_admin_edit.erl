%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_resource.

-module(resource_admin_edit).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    resource_exists/2
]).

-include_lib("resource_html.hrl").

resource_exists(_ReqProps, Context) ->
    Context1 = zp_context:ensure_all(Context),
    Id = zp_context:get_q("id", Context1),
    try
        IdN = list_to_integer(Id),
        {m_rsc:exists(IdN, Context1), zp_context:set(id, IdN, Context1)}
    catch
        _:_ ->
            {false, Context1}
    end.


html(_ReqProps, Context) ->
    Vars = [
        {id, zp_context:get(id, Context)}
    ],
    F = fun(Ctx) ->
        zp_template:render("admin_edit.tpl", Vars, Ctx)
    end,
    Html = zp_acl:sudo(F, Context),
	zp_context:output(Html, Context).
