%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Generic template controller, serves the template mentioned in the dispatch configuration.

-module(resource_template).
-author("Marc Worrell <marc@worrell.nl>").

-export([html/1]).

-include_lib("resource_html.hrl").

html(Context) ->
    Template = z_context:get(template, Context),
    Rendered = z_template:render(Template, z_context:get_all(Context), Context),
    z_context:output(Rendered, Context).
