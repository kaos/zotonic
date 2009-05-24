%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-09
%%
%% @doc Model for predicates

-module(m_predicate).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    is_predicate/2,
    id_to_name/2,
    name_to_id/2,
    name_to_id_check/2,
    objects/2,
    subjects/2,
    all/1,
    get/2,
    insert/2,
    delete/2,
    update/3
]).

-include_lib("zophrenic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(all, #m{value=undefined}, Context) ->
    all(Context);
m_find_value(Key, #m{value=undefined}, Context) ->
    get(Key, Context).

%% @doc Transform a model value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=undefined}, Context) ->
    all(Context);
m_to_list(#m{}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{}, Context) ->
    all(Context).


%% @doc Test if the property is the name of a predicate
%% @spec is_predicate(Pred, Context) -> bool()
is_predicate(Pred, Context) ->
    case name_to_id(Pred, Context) of
        {ok, _Id} -> true;
        _ -> false
    end.


%% @doc Lookup the name of a predicate with an id
%% @spec id_to_name(Id, Context) -> {ok, atom()} | {error, Reason}
id_to_name(Id, Context) when is_integer(Id) ->
    F = fun() ->
        case zp_db:q1("select name from predicate where id = $1", [Id], Context) of
            undefined -> {error, {enoent, predicate, Id}};
            Name -> {ok, zp_convert:to_atom(Name)}
        end
    end,
    zp_depcache:memo(F, {predicate_name, Id}, ?DAY, [predicate]).

    
%% @doc Return the id of the predicate
%% @spec name_to_id(Pred, Context) -> {ok, int()} | {error, Reason}
name_to_id(Pred, Context) when is_atom(Pred) ->
    F = fun() ->
        case zp_db:q1("select id from predicate where name = $1", [Pred], Context) of
            undefined -> {error, {enoent, predicate, Pred}};
            Id -> {ok, Id}
        end
    end,
    zp_depcache:memo(F, {predicate_id, Pred}, ?DAY, [predicate]);
name_to_id(Pred, _Context) when is_integer(Pred) ->
    {ok, Pred};
name_to_id(Pred, Context) ->
    name_to_id(zp_convert:to_atom(Pred), Context).


%% @doc Return the id of the predicate, crash when it can't be found
%% @spec name_to_id_check(Pred, Context) -> int()
name_to_id_check(Name, Context) ->
    {ok, Id} = name_to_id(Name, Context),
    Id.


%% @doc Return the definition of the predicate
%% @spec predicate(Pred, Context) -> PredicatePropList | undefined
get(PredId, Context) when is_integer(PredId) ->
    case id_to_name(PredId, Context) of
        {ok, Pred} -> get(Pred, Context);
        {error, {enoent, predicate, _Id}} -> undefined
    end;
get(Pred, Context) when is_list(Pred) orelse is_binary(Pred) ->
    get(list_to_atom(string:to_lower(Pred)), Context);
get(Pred, Context) ->
    case zp_depcache:get(predicate, Pred) of
        {ok, undefined} ->
            undefined;
        {ok, Value} ->
            Value;
        undefined ->
            proplists:get_value(Pred, all(Context))
    end.

%% @doc Return the category ids that are valid as objects
objects(Id, Context) ->
    Objects = zp_db:q("select category_id from predicate_category where predicate_id = $1 and is_subject = false", [Id], Context),
    [ R || {R} <- Objects  ].

%% @doc Return the category ids that are valid as subjects
subjects(Id, Context) ->
    Subjects = zp_db:q("select category_id from predicate_category where predicate_id = $1 and is_subject = true", [Id], Context),
    [ R || {R} <- Subjects  ].


%% @doc Return the list of all predicates
%% @spec all(Context) -> PropList
all(Context) ->
    case zp_depcache:get(predicate) of
        {ok, Preds} -> 
            Preds;
        undefined ->
            Preds = zp_db:assoc_props("select * from predicate order by name", Context),
            FSetPred = fun(Pred) ->
                Id = proplists:get_value(id, Pred),
                Atom = list_to_atom(binary_to_list(proplists:get_value(name, Pred))),
                {Atom, [{pred, Atom},{subject,subjects(Id,Context)},{object,objects(Id,Context)}|Pred]}
            end,
            Preds1 = [ FSetPred(Pred) || Pred <- Preds],
            zp_depcache:set(predicate, Preds1, ?DAY, [predicate]),
            Preds1
    end.

%% @doc Insert a new predicate
%% @spec insert(Props, Context) -> {ok, Id}
insert(Props, Context) ->
    true = zp_acl:has_role(admin, Context),
    Props1 = lists:filter(fun valid_prop/1, Props),
    Props2 = [ {zp_convert:to_atom(N), V} || {N,V} <- Props1 ],
    {ok, Id} = zp_db:insert(predicate, Props2, Context),
    zp_depcache:flush(predicate),
    {ok, Id}.

%% @doc Delete a predicate, crashes when the predicate is in use
%% @spec delete(Props, Context) -> void()
delete(Id, Context) ->
    true = zp_acl:has_role(admin, Context),
    zp_db:delete(predicate, Id, Context),
    zp_depcache:flush(predicate).


%% @doc Update a predicate
%% @spec update(Props, Props, Context) -> void()
update(Id, Props, Context) ->
    true = zp_acl:has_role(admin, Context),
    Subjects = proplists:get_all_values("subject", Props),
    Objects = proplists:get_all_values("object", Props),
    ?DEBUG(Subjects),
    SubjectIds = [ list_to_integer(N) || N <- Subjects, N /= [] ],
    ObjectIds = [ list_to_integer(N) || N <- Objects, N /= [] ],
    Props1 = lists:filter(fun valid_prop/1, Props),
    Props2 = [ {zp_convert:to_atom(N), V} || {N,V} <- Props1 ],
    F = fun(Ctx) ->
        zp_db:update(predicate, Id, Props2, Ctx),
        update_predicate_category(Id, true, SubjectIds, Ctx),
        update_predicate_category(Id, false, ObjectIds, Ctx),
        ok
    end,
    ok = zp_db:transaction(F, Context),
    zp_depcache:flush(predicate).


valid_prop({Name, _Value}) when is_atom(Name) -> true;
valid_prop({"title", _Value}) -> true;
valid_prop({"descr", _Value}) -> true;
valid_prop({"name", _Value}) -> true;
valid_prop({"uri", _Value}) -> true;
valid_prop({"reversed", _Value}) -> true;
valid_prop(_) -> false.



update_predicate_category(Id, IsSubject, CatIds, Context) ->
    OldIdsR = zp_db:q("select category_id from predicate_category where predicate_id = $1 and is_subject = $2", [Id, IsSubject], Context),
    OldIds  = [ N || {N} <- OldIdsR ],
    % Delete the ones that are not there anymore
    [ zp_db:q("delete from predicate_category where predicate_id = $1 and category_id = $2 and is_subject = $3", [Id, OldId, IsSubject], Context)
    || OldId <- OldIds, not lists:member(OldId, CatIds)
    ],
    [ zp_db:insert(predicate_category, [{predicate_id, Id}, {category_id, NewId}, {is_subject, IsSubject}], Context)
    || NewId <- CatIds, not lists:member(NewId, OldIds)
    ],
    ok.
    

