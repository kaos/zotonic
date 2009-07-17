%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-09
%%
%% @doc 

-module(m_edge).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    get/2,
    get_id/4,
    get_edges/2,
    insert/4,
    delete/2,
    delete/4,
    object/4,
    subject/4,
    objects/3,
    subjects/3,
    objects/2,
    subjects/2,
    object_predicates/2,
    subject_predicates/2,
    object_predicate_ids/2,
    subject_predicate_ids/2
]).

-include_lib("zophrenic.hrl").



%% @doc Get the complete edge with the id
get(Id, Context) ->
    zp_db:assoc_row("select * from edge where id = $1", [Id], Context).

%% @doc Get the edge id of a subject/pred/object combination
get_id(SubjectId, Pred, ObjectId, Context) ->
    PredId = m_predicate:name_to_id_check(Pred, Context),
    zp_db:q1("select id from edge where subject_id = $1 and object_id = $2 and predicate_id = $3", [SubjectId, PredId, ObjectId], Context).

%% @doc Return the full description of all edges from a subject, grouped by predicate
get_edges(SubjectId, Context) ->
    case zp_depcache:get({edges, SubjectId}) of
        {ok, Edges} -> 
            Edges;
        undefined ->
            Edges = zp_db:assoc("
                select e.id, e.subject_id, e.predicate_id, p.name, e.object_id, e.seq 
                from edge e join predicate p on p.id = e.predicate_id 
                where e.subject_id = $1 
                order by e.predicate_id, e.seq, e.id", [SubjectId], Context),
            Edges1 = zp_utils:group_proplists(name, Edges),
            zp_depcache:set({edges, SubjectId}, Edges1, ?DAY, [SubjectId]),
            Edges1
    end.

%% Insert a new edge
insert(SubjectId, PredId, ObjectId, Context) when is_integer(PredId) ->
    case m_predicate:is_predicate(PredId, Context) of
        true -> insert1(SubjectId, PredId, ObjectId, Context);
        false -> throw({error, {enoent, predicate, PredId}})
    end;
insert(SubjectId, Pred, ObjectId, Context) ->
    PredId = m_predicate:name_to_id_check(Pred, Context),
    insert1(SubjectId, PredId, ObjectId, Context).
    
    insert1(SubjectId, PredId, ObjectId, Context) ->
        case zp_db:q1("select id from edge where subject_id = $1 and object_id = $2 and predicate_id = $3", [SubjectId, ObjectId, PredId], Context) of
            undefined ->
                {ok, EdgeId} = zp_db:insert(edge, [{subject_id, SubjectId}, {object_id, ObjectId}, {predicate_id, PredId}], Context),
                zp_depcache:flush(SubjectId),
                zp_depcache:flush(ObjectId),
                {ok, EdgeId};
            EdgeId ->
                % Edge exists - skip
                {ok, EdgeId}
        end.


%% @doc Delete an edge by Id
delete(Id, Context) ->
    case zp_db:q("select subject_id, object_id from edge where id = $1", [Id], Context) of
        [{SubjectId,ObjectId}] ->
            zp_db:delete(edge, Id, Context),
            zp_depcache:flush(SubjectId),
            zp_depcache:flush(ObjectId),
            ok;
        [] -> 
            ok
    end.

%% @doc Delete an edge by subject, object and predicate id
delete(SubjectId, Pred, ObjectId, Context) ->
    PredId = m_predicate:name_to_id_check(Pred, Context),
    zp_db:q("delete from edge where subject_id = $1 and object_id = $2 and predicate_id = $3",  [SubjectId, ObjectId, PredId], Context),
    zp_depcache:flush(SubjectId),
    zp_depcache:flush(ObjectId),
    ok.
    

%% @doc Return the Nth object with a certaing predicate of a subject.
object(Id, Pred, N, Context) ->
    Ids = objects(Id, Pred, Context),
    try
        lists:nth(N, Ids)
    catch 
        _:_ -> undefined
    end.

%% @doc Return the Nth subject with a certaing predicate of an object.
subject(Id, Pred, N, Context) ->
    Ids = subjects(Id, Pred, Context),
    try
        lists:nth(N, Ids)
    catch 
        _:_ -> undefined
    end.

%% @doc Return all object ids of an id with a certain predicate.  The order of the ids is deterministic.
%% @spec objects(Id, Pred, Context) -> List
objects(_Id, undefined, _Context) ->
    [];
objects(Id, Pred, Context) when is_atom(Pred) ->
    case m_predicate:name_to_id(Pred, Context) of
        {error,{enoent,predicate,_}} -> [];
        {ok, PredId} -> objects(Id, PredId, Context)
    end;
objects(Id, Pred, Context) ->
    case zp_depcache:get({objects, Pred, Id}) of
        {ok, Objects} ->
            Objects;
        undefined ->
            Ids = zp_db:q("select object_id from edge where subject_id = $1 and predicate_id = $2 order by seq,id", [Id, Pred], Context),
            Objects = [ ObjId || {ObjId} <- Ids ],
            zp_depcache:set({objects, Pred, Id}, Objects, ?DAY, [Id]),
            Objects
    end.


%% @doc Return all subject ids of an object id with a certain predicate.   The order of the ids is deterministic.
%% @spec subjects(Id, Pred, Context) -> List
subjects(_Id, undefined, _Context) ->
    [];
subjects(Id, Pred, Context) when is_atom(Pred) ->
    case m_predicate:name_to_id(Pred, Context) of
        {error,{enoent,predicate,_}} -> [];
        {ok, PredId} -> subjects(Id, PredId, Context)
    end;
subjects(Id, Pred, Context) ->
    case zp_depcache:get({subjects, Pred, Id}) of
        {ok, Objects} ->
            Objects;
        undefined ->
            Ids = zp_db:q("select subject_id from edge where object_id = $1 and predicate_id = $2 order by id", [Id, Pred], Context),
            Subjects = [ SubjId || {SubjId} <- Ids ],
            zp_depcache:set({subjects, Pred, Id}, Subjects, ?HOUR, [Id]),
            Subjects
    end.


%% @doc Return all object ids of the resource
%% @spec objects(Id, Context) -> list()
objects(Id, Context) ->
    F = fun() ->
        Ids = zp_db:q("select object_id from edge where subject_id = $1 order by predicate_id, seq, id", [Id], Context),
        [ ObjId || {ObjId} <- Ids]
    end,
    zp_depcache:memo(F, {objects, Id}, ?DAY, [Id]).

%% @doc Return all subject ids of the resource
%% @spec subjects(Id, Context) -> list()
subjects(Id, Context) ->
    F = fun() ->
        Ids = zp_db:q("select subject_id from edge where object_id = $1 order by predicate_id, id", [Id], Context),
        [ SubjId || {SubjId} <- Ids]
    end,
    zp_depcache:memo(F, {subjects, Id}, ?HOUR, [Id]).


%% @doc Return the list of predicates in use by edges to objects from the id
%% @spec object_preds(Id, Context) -> List
object_predicates(Id, Context) ->
    Ps = zp_db:q("select distinct p.name from edge e join rsc p on e.predicate_id = p.id where e.subject_id = $1 order by name", [Id], Context),
    [ list_to_atom(binary_to_list(P)) || {P} <- Ps ].

%% @doc Return the list of predicates is use by edges from subjects to the id
%% @spec object_preds(Id, Context) -> List
subject_predicates(Id, Context) ->
    Ps = zp_db:q("select distinct p.name from edge e join rsc p on e.predicate_id = p.id where e.object_id = $1 order by name", [Id], Context),
    [ list_to_atom(binary_to_list(P)) || {P} <- Ps ].

%% @doc Return the list of predicate ids in use by edges to objects from the id
%% @spec object_preds(Id, Context) -> List
object_predicate_ids(Id, Context) ->
    Ps = zp_db:q("select distinct predicate_id from edge where subject_id = $1", [Id], Context),
    [ P || {P} <- Ps ].

%% @doc Return the list of predicates is use by edges from subjects to the id
%% @spec object_preds(Id, Context) -> List
subject_predicate_ids(Id, Context) ->
    Ps = zp_db:q("select distinct predicate_id from edge where object_id = $1", [Id], Context),
    [ P || {P} <- Ps ].
