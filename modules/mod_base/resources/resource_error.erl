%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_error).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(Context) ->
	ErrorCode = "404 Page not found",
    Html = z_template:render("error.tpl", [{error_code, ErrorCode}], Context),
	z_context:output(Html, Context).