%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2009 Arjan Scherpenisse
%% @date 2009-10-03
%%
%% @doc Retrieve a full dump of an object.

-module(service_base_export).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-svc_title("Retrieve a full export of an object.").
-svc_needauth(false).

-export([process_get/2]).

-include_lib("zotonic.hrl").


process_get(_ReqData, Context) ->
    Id = m_rsc:rid(z_context:get_q("id", Context), Context),
    case m_rsc:exists(Id, Context) of 
        false ->
            [];
        true ->
            Export = m_rsc:get_raw(Id, Context),
            %% This should probably be encapsulated in m_edges.
            Edges = z_db:assoc("
                select e.id, e.subject_id, e.predicate_id, p.name, e.object_id, e.seq 
                from edge e join rsc p on p.id = e.predicate_id 
                where e.subject_id = $1 
                order by e.predicate_id, e.seq, e.id", [Id], Context),
            Ex = Export ++ [{connections, Edges}],
            z_convert:to_json(Ex)
    end.


