%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2009-10-03
%%
%% @doc Get information about the system.

-module(service_base_meta).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Meta-information about all API calls.").
-svc_needauth(false).

-export([process_get/2]).

-include_lib("zotonic.hrl").


process_get(_ReqData, Context) ->
    M = z_service:all(info, Context),
    {array, [ {struct, [ {Key, z_convert:to_atom(Value)} || {Key, Value} <- L] } || L <- M]}.
