%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009  Marc Worrell
%%
%% @doc Error handler for webmachine HTTP errors. The result varies depending on the content type being served.
%% @todo Mail the error to the webadmin

-module(z_webmachine_error_handler).
-author("Marc Worrell <marc@worrell.nl>").
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-export([render_error/3]).

-include_lib("zotonic.hrl").

render_error(Code=404, Req, _Reason) ->
    ErrorDump = mochiweb_html:escape(lists:flatten(io_lib:format("Resource not found: ~p", [Req:raw_path()]))),
    Type = list_to_atom(Req:get_metadata('content-type')),
    error_handler(Type, Req, Code, ErrorDump);

render_error(Code=500, Req, Reason) ->
    error_logger:error_msg("webmachine error: path=~p~n~p~n", [Req:path(), Reason]),
    ErrorDump = mochiweb_html:escape(lists:flatten(io_lib:format("~p", [Reason]))),
    Type = list_to_atom(Req:get_metadata('content-type')),
    error_handler(Type, Req, Code, ErrorDump).


error_handler('application/json', Req, Code, ErrorDump) ->
    Req:add_response_header("Content-Type", "application/json; charset=utf-8"),
    Req:add_response_header("Content-Encoding", "identity"),
    JS = {struct, [{error_code, Code}, {error_dump, ErrorDump}]},
    mochijson:encode(JS);

error_handler(_Default, Req, Code, ErrorDump) ->
    Req:add_response_header("Content-Type", "text/html; charset=utf-8"),
    Req:add_response_header("Content-Encoding", "identity"),
    Context = z_context:new(Req:get_metadata('host')),
    Vars = [{error_code, Code}, {error_dump, ErrorDump}],
    Html = z_template:render("error.tpl", Vars, Context),
    {Output, _} = z_context:output(Html, Context),
    Output.

