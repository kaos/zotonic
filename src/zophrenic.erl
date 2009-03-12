%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Start/stop functions for Zophrenic

-module(zophrenic).
-author('Marc Worrell <marc@worrell.nl>').
-export([start/0, stop/0]).
-revision("$Id$").

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start() -> ok
%% @doc Start the zophrenic server.
start() -> start([]).
	
%% @spec start(_Args) -> ok
%% @doc Start the zophrenic server.
start(_Args) ->
    zophrenic_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(zophrenic).

%% @spec stop() -> ok
%% @doc Stop the zophrenic server.
stop() ->
    Res = application:stop(zophrenic),
    application:stop(webmachine),
    application:stop(crypto),
    Res.


%% @spec stop([Node]) -> void()
%% @doc Stop a zophrenic server on a specific node
stop([Node]) ->
    io:format("Stop:~p~n",[Node]),
    case net_adm:ping(Node) of
    	pong -> rpc:cast(Node, init, stop, []);
    	pang -> io:format("There is no node with this name~n")
    end,
    init:stop().
