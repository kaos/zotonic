%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Generic template controller, serves the template mentioned in the dispatch configuration.

-module(resource_template).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, to_html/1, service_available/2, charsets_provided/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zophrenic.hrl").

init(Settings) -> {ok, Settings}.

service_available(ReqData, Settings) ->
    {template, Template} = proplists:lookup(template, Settings),
    Context  = zp_context:new(ReqData, ?MODULE),
    Context1 = zp_context:set(template, Template, Context),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

to_html(Context) ->
    Context1 = zp_context:ensure_all(Context),
    Template = zp_context:get(template, Context1),
    Html     = zp_template:render(Template, [], Context1),
    zp_context:output(Html, Context1).
