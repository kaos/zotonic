%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2009-10-03
%%
%% @doc Retrieve the list of all objects in the system.

-module(service_base_everything).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Retrieve the list of all objects in the system.").
-svc_needauth(true).

-export([process_get/2]).

-include_lib("zotonic.hrl").


process_get(_ReqData, Context) ->
    F = fun() ->
                Ids = z_db:q("SELECT id FROM rsc ORDER BY id", Context),
                Ids2 = lists:filter(fun({Id}) -> m_rsc:is_visible(Id, Context) end, Ids),
                {array, [Id || {Id} <- Ids2]}
        end,
    z_depcache:memo(F, {everything}, ?HOUR, [], Context).

