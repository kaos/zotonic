%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_bike).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(Context) ->
    Html = zp_template:render("bike.tpl", [], Context),
	zp_context:output(Html, Context).