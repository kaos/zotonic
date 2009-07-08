%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_shop_page).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").

html(Context) ->
    Html = zp_template:render("page.tpl", [], Context),
	zp_context:output(Html, Context).