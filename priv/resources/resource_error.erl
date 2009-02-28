%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_error).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->
	ErrorCode = "404 Page not found",
	Context1 = zp_context:set_context(error_code, ErrorCode, Context),
    
    Html = zp_template:render("error.tpl", Context1),
	zp_context:output(Html, Context1).