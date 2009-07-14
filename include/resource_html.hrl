%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Empty HTML resource, defining the basic callbacks needed for a html page.  All is needed is the 'html' function.
%% Make a new resource (resource_plop.erl) by:
%%
%% -module(resource_plop).
%% -author("Your Name <me@example.com>").
%% -include_lib("resource_html.hrl").
%% 
%% html(Context) ->
%%    Html = zp_template:render("plop.tpl", Context),
%%    zp_context:output(Html, Context).

-export([init/1, to_html/2, service_available/2, charsets_provided/2]).

-include_lib("webmachine_resource.hrl").
-include_lib("include/zophrenic.hrl").

init(DispatchArgs) -> {ok, DispatchArgs}.

service_available(ReqData, DispatchArgs) when is_list(DispatchArgs) ->
    Context  = zp_context:new(ReqData, ?MODULE),
    Context1 = zp_context:set(DispatchArgs, Context),
    ?WM_REPLY(true, Context1).

charsets_provided(ReqData, Context) ->
    {[{"utf-8", fun(X) -> X end}], ReqData, Context}.

to_html(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = zp_context:ensure_all(Context1),
    {Html, HtmlContext} = html(Context2),
    ?WM_REPLY(Html, HtmlContext).
