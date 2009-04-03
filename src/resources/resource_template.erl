%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Generic template controller, serves the template mentioned in the dispatch configuration.

-module(resource_template).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, to_html/2, service_available/2, charsets_provided/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zophrenic.hrl").

init(Settings) -> {ok, Settings}.

service_available(ReqProps, Settings) ->
    {template, Template} = proplists:lookup(template, Settings),
    Context  = zp_context:new(ReqProps),
    Context1 = zp_context:set_context(template, Template, Context),
    {true, Context1}.

charsets_provided(_ReqProps, Context) ->
    {[{"utf-8", fun(X) -> X end}], Context}.

to_html(_ReqProps, Context) ->
    Context1 = zp_context:ensure_all(Context),
    Template = zp_context:get_context(template, Context1),
    Html     = zp_template:render(Template, [], Context1),
    zp_context:output(Html, Context1).
