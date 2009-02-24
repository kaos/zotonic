%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Basic functions for Zophrenic

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
start() ->
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

