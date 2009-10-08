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
    case z_context:get_q("id", Context) of
        undefined ->
            {error, missing_arg, "id"};
        [] ->
            {error, missing_arg, "id"};
        Id ->
            case m_rsc:exists(Id, Context) of 
                true ->
                    case m_rsc:is_visible(Id, Context) of
                        true ->
                            Rsc = m_rsc:get_raw(m_rsc:rid(Id, Context), Context),
                            %% This should probably be encapsulated in m_edges.
                            Edges = z_db:assoc("
                                select e.id, e.subject_id, e.predicate_id, p.name, e.object_id, e.seq 
                                from edge e join rsc p on p.id = e.predicate_id 
                                where e.subject_id = $1 
                                order by e.predicate_id, e.seq, e.id", [Id], Context),

                            {ok, Category} = z_db:select(category, Id, Context),
                            {ok, Medium} = z_db:select(medium, Id, Context),
                            {ok, Group} = z_db:select(group, Id, Context),

                            %% Build the export structure
                            Ex = [ {rsc, Rsc}, {connections, Edges}, {category, Category}, {medium, Medium}, {group, Group} ],
                            %% Filter out empty sublists
                            J = z_convert:to_json(lists:filter(fun({_, X}) -> not(X == []) end, Ex)),
                            J;
                        false ->
                            {error, access_denied, undefined}
                    end;
                false ->
                    {error, not_exists, Id}
            end
    end.


