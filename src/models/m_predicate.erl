%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-09
%%
%% @doc Model for predicates

-module(m_predicate).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    is_predicate/2,
    id/2,
    all/1,
    predicate/2
]).

-include_lib("zophrenic.hrl").


%% @doc Test if the property is the name of a predicate
%% @spec is_predicate(Pred, Context) -> bool()
is_predicate(Pred, Context) ->
    case id(Pred, Context) of
        undefined -> false;
        _Id -> true
    end.

%% @doc Return the id of the predicate
%% @spec id(Pred, Context) -> int()
id(Pred, Context) when is_atom(Pred) ->
    case zp_depcache:get({predicate_id, Pred}) of
        {ok, PredId} ->
            PredId;
        undefined ->
            case zp_db:q1("select id from predicate where name = $1", [Pred], Context) of
                undefined ->
                    zp_depcache:set({predicate_id, Pred}, undefined, ?DAY, [predicate]),
                    undefined;
                Id ->
                    zp_depcache:set({predicate_id, Pred}, Id, ?DAY, [predicate]),
                    Id
            end
    end;
id(Pred, _Context) when is_integer(Pred) ->
    Pred;
id(Pred, Context) ->
    id(zp_utils:to_atom(Pred), Context).


%% @doc Return the definition of the predicate
%% @spec predicate(Pred, Context) -> PredicatePropList | undefined
predicate(Pred, Context) when is_binary(Pred) ->
    predicate(list_to_atom(zp_utils:to_lower(Pred)), Context);
predicate(Pred, Context) when is_list(Pred) ->
    predicate(list_to_atom(string:to_lower(Pred)), Context);
predicate(Pred, Context) ->
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
            Preds = zp_db:assoc("select * from predicate order by name", Context),
            FSetPred = fun(Pred) ->
                Atom = list_to_atom(binary_to_list(proplists:get_value(name, Pred))),
                {Atom, [{pred, Atom}|Pred]}
            end,
            Preds1 = [ FSetPred(Pred) || Pred <- Preds],
            zp_depcache:set(predicate, Preds1, ?DAY, [predicate]),
            Preds1
    end.

