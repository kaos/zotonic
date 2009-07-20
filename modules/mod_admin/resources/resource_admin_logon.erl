%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_resource.

-module(resource_admin_logon).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(Context) ->
    Html = z_template:render("admin_logon.tpl", [], Context),
	z_context:output(Html, Context).
