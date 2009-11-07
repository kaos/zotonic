%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2009-10-06
%%
%% @doc Remotely recompile and flush.

-module(service_development_recompile).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Remotely code reload / recompile and flush.").
-svc_needauth(false).

-export([process_get/2]).

-include_lib("zotonic.hrl").


process_get(_ReqData, _Context) ->
    try
        z:m()
    catch
        _:_ ->
            undefined
    end,
    "OK".
