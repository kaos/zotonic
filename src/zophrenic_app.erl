%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Callbacks for the zophrenic application.

-module(zophrenic_app).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for zophrenic.
start(_Type, _StartArgs) ->
    zophrenic_deps:ensure(),
    zophrenic_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for zophrenic.
stop(_State) ->
    ok.
