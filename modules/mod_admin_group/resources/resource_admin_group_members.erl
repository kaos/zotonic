%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Overview of all members of a group.

-module(resource_admin_group_members).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2,
    resource_exists/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(ReqData, Context).


resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = zp_context:ensure_all(Context1),
    Id = zp_context:get_q("id", Context2),
    try
        IdN = list_to_integer(Id),
        Context3 = zp_context:set(id, IdN, Context2),
        ?WM_REPLY(m_rsc:exists(IdN, Context3) andalso m_rsc:is_a(IdN, group, Context), Context3)
    catch
        _:_ -> ?WM_REPLY(false, Context2)
    end.

html(Context) ->
    Vars = [
        {page_admin_group, true},
        {id, zp_context:get(id, Context)}
    ],
	Html = zp_template:render("admin_group_members.tpl", Vars, Context),
	zp_context:output(Html, Context).
