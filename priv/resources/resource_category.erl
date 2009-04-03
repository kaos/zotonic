%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_category).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").
 
html(_ReqProps, Context) ->
    Html = zp_template:render("category.tpl", [], Context),
    zp_context:output(Html, Context).