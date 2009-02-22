%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Empty HTML resource, defining the basic callbacks needed for a html page.  All is needed is the 'html' function.
%% Make a new resource (resource_plop.erl) by:
%%
%% -module(resource_plop).
%% -author("Your Name <me@example.com>").
%% -include_lib("resource_html.hrl").
%% 
%% html(_ReqProps, Context) ->
%%    Html = zp_template:render("plop.tpl", Context),
%%    zp_context:output(Html, Context).

-export([init/1, to_html/2, service_available/2, charsets_provided/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zophrenic.hrl").

init([]) -> {ok, []}.

service_available(ReqProps, _Context) ->
    {true, zp_context:new(ReqProps)}.

charsets_provided(_ReqProps, Context) ->
    {[{"utf-8", fun(X) -> X end}], Context}.

to_html(ReqProps, Context) ->
    Context1 = zp_context:ensure_all(Context),
    html(ReqProps, Context1).

