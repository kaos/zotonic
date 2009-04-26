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
    name_to_id/2,
    name_to_id_check/2,
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
get(Pred, Context) when is_binary(Pred) ->
    get(list_to_atom(zp_string:to_lower(Pred)), Context);
get(Pred, Context) when is_list(Pred) ->
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


%% @doc Return the list of all predicates
%% @spec all(Context) -> PropList
all(Context) ->
    case zp_depcache:get(predicate) of
        {ok, Preds} -> 
            Preds;
        undefined ->
            Preds = zp_db:assoc_props("select * from predicate order by name", Context),
            FSetPred = fun(Pred) ->
                Atom = list_to_atom(binary_to_list(proplists:get_value(name, Pred))),
                {Atom, [{pred, Atom}|Pred]}
            end,
            Preds1 = [ FSetPred(Pred) || Pred <- Preds],
            zp_depcache:set(predicate, Preds1, ?DAY, [predicate]),
            Preds1
    end.

%% @doc Insert a new predicate
%% @spec insert(Props, Context) -> {ok, Id}
insert(Props, Context) ->
    {ok, Id} = zp_db:insert(predicate, Props, Context),
    zp_depcache:flush(predicate),
    {ok, Id}.

%% @doc Delete a predicate, crashes when the predicate is in use
%% @spec delete(Props, Context) -> void()
delete(Id, Context) ->
    zp_db:delete(predicate, Id, Context),
    zp_depcache:flush(predicate).


%% @doc Update a predicate
%% @spec update(Props, Props, Context) -> void()
update(Id, Props, Context) ->
    zp_db:update(predicate, Id, Props, Context),
    zp_depcache:flush(predicate).

