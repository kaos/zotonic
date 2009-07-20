%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Callbacks for the zotonic application.

-module(zotonic_app).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(application).
-export([start/2,stop/1]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for zotonic.
start(_Type, _StartArgs) ->
    ensure_started(crypto),
    ensure_started(ssl),
    zotonic_deps:ensure(),
    zotonic_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for zotonic.
stop(_State) ->
    ok.
