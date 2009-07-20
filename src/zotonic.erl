%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Start/stop functions for Zotonic

-module(zotonic).
-author('Marc Worrell <marc@worrell.nl>').
-export([start/0, start/1, stop/0, stop/1]).
-revision("$Id$").

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start() -> ok
%% @doc Start the zotonic server.
start() -> start([]).
	
%% @spec start(_Args) -> ok
%% @doc Start the zotonic server.
start(_Args) ->
    zotonic_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(zotonic).

%% @spec stop() -> ok
%% @doc Stop the zotonic server.
stop() ->
    Res = application:stop(zotonic),
    application:stop(webmachine),
    application:stop(crypto),
    Res.


%% @spec stop([Node]) -> void()
%% @doc Stop a zotonic server on a specific node
stop([Node]) ->
    io:format("Stop:~p~n",[Node]),
    case net_adm:ping(Node) of
    	pong -> rpc:cast(Node, init, stop, []);
    	pang -> io:format("There is no node with this name~n")
    end,
    init:stop().
