%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_resource.

-module(resource_admin_logon).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->
	
    Html = zp_template:render("admin_logon.tpl", [], Context),
	zp_context:output(Html, Context).