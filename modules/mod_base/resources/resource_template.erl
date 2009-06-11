%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Generic template controller, serves the template mentioned in the dispatch configuration.

-module(resource_template).
-author("Marc Worrell <marc@worrell.nl>").

-export([init/1, to_html/2, service_available/2, charsets_provided/2]).

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

to_html(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = zp_context:ensure_all(Context1),
    Template = zp_context:get(template, Context2),
    Rendered = zp_template:render(Template, [], Context2),
    {Html, Context3} = zp_context:output(Rendered, Context2),
    ?WM_REPLY(Html, Context3).
