%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_resource.

-module(resource_admin_overview).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(ReqData, Context).


html(Context) ->
	Html = zp_template:render("admin_overview.tpl", [{page_overview, true}], Context),
	zp_context:output(Html, Context).