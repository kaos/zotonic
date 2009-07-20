%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_resource.

-module(resource_admin).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    z_auth:wm_is_authorized(ReqData, Context).


html(Context) ->
    Html = z_template:render("admin.tpl", [{page_dashboard, true}], Context),
	z_context:output(Html, Context).
