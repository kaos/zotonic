%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Callbacks for the zophrenic application.

-module(zophrenic_app).
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
%% @doc application start callback for zophrenic.
start(_Type, _StartArgs) ->
    ensure_started(crypto),
    ensure_started(ssl),
    zophrenic_deps:ensure(),
    zophrenic_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for zophrenic.
stop(_State) ->
    ok.
