%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Overview of all known grooups, used in the admin.

-module(resource_admin_group).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(ReqData, Context).


html(Context) ->
	Html = zp_template:render("admin_group.tpl", [{page_admin_group, true}], Context),
	zp_context:output(Html, Context).
