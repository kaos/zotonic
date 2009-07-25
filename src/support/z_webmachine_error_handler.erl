%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009  Marc Worrell
%%
%% @doc Error handler for webmachine HTTP errors.
%% @todo Mail the error to the webadmin

-module(z_webmachine_error_handler).
-author("Marc Worrell <marc@worrell.nl>").

-export([render_error/3]).

render_error(404, Req, _Reason) ->
    Req:add_response_header("Content-Type", "text/html; charset=utf-8"),
    Req:add_response_header("Content-Encoding", "identity"),
    ErrorDump = mochiweb_html:escape(lists:flatten(io_lib:format("Resource not found: ~p", [Req:path()]))),
    Context   = z_context:new(),
	Vars      = [{error_code, 404}, {error_dump, ErrorDump}],
    Html      = z_template:render("error.tpl", Vars, Context),
    {Output, _Context} = z_context:output(Html, Context),
    Output;

render_error(500, Req, Reason) ->
    Req:add_response_header("Content-Type", "text/html; charset=utf-8"),
    Req:add_response_header("Content-Encoding", "identity"),
    error_logger:error_msg("webmachine error: path=~p~n~p~n", [Req:path(), Reason]),
    ErrorDump = mochiweb_html:escape(lists:flatten(io_lib:format("~p", [Reason]))),
    Context   = z_context:new(),
	Vars      = [{error_code, 500}, {error_dump, ErrorDump}],
    Html      = z_template:render("error.tpl", Vars, Context),
    {Output, _Context} = z_context:output(Html, Context),
    Output.
