%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Model for resource data. Interfaces between zophrenic, templates and the database.

-module(m_rsc).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	rsc/0,
	exists/2, 
	is_readable/2, is_writeable/2, is_owner/2, is_ingroup/2, is_me/2,
	p/3, 
	op/2, o/2, o/3, o/4,
	sp/2, s/2, s/3, s/4,
	media/2, media/3
]).

-include_lib("zophrenic.hrl").

rsc() -> fun(Id, _Context) -> #rsc{id=Id} end.


exists([C|_] = UniqueName, Context) when is_list(UniqueName) and is_integer(C) ->
    case rid_unique_name(UniqueName, Context) of
        undefined -> false;
        _ -> true
    end;
exists(UniqueName, Context) when is_binary(UniqueName) ->
    case rid_unique_name(UniqueName, Context) of
        undefined -> false;
        _ -> true
    end;
exists(Id, Context) -> 
    case rid(Id, Context) of
        #rsc{id=Rid} = Rsc ->
            case zp_depcache:get({exists, Rid}) of
                undefined -> 
                    Exists = case zp_db:q1("select id from rsc where id = $1", [Rid], Context) of
                        undefined -> false;
                        _ -> true
                    end,
                    zp_depcache:set({exists, Rid}, Exists, Rsc),
                    Exists;
                Exists ->
                    Exists
            end;
        undefined -> false
    end.
    
is_readable(Id, Context) -> true.
is_writeable(Id, Context) -> true.
is_owner(Id, Context) -> true.
is_ingroup(Id, Context) -> true.
is_me(Id, Context) -> true.

%% @todo Perform access control checks, return 'undefined' on an error or permission denial

%% Unknown properties will be checked against the predicates, returns o(Predicate).
p(Id, media, Context) -> media(Id, Context);
p(Id, o, Context)  -> o(Id, Context);
p(Id, s, Context)  -> s(Id, Context);
p(Id, m, Context)  -> m(Id, Context);
p(Id, op, Context) -> op(Id, Context);
p(Id, sp, Context) -> sp(Id, Context);

p(#rsc{id=Id} = Rsc, Predicate, Context) -> 
    Value = case zp_depcache:get(Rsc, Predicate) of
        {ok, V} -> 
            V;
        undefined ->
            case zp_db:select(rsc, Id, Context) of
                {ok, Record} ->
                    zp_depcache:set(Rsc, Record, ?WEEK, [Rsc]),
                    proplists:get_value(Predicate, Record);
                _ ->
                    zp_depcache:set(Rsc, undefined, ?WEEK, [Rsc]),
                    undefined
            end
    end,
    case Value of
        undefined ->
            case m_predicate:is_predicate(Predicate, Context) of
                true -> o(Rsc, Predicate, Context);
                false -> undefined
            end;
        _ ->
            Value
    end;
p(undefined, _Predicate, _Context) ->
    undefined;
p(Id, Predicate, Context) ->
    p(rid(Id, Context), Predicate, Context).


%% Fetch some predicate from a module.
m(Id, _Context) ->
    fun(Predicate, Context) -> m(Id, Predicate, Context) end.

m(#rsc{id=Id}, Predicate, Context) ->
    undefined;
m(undefined, _Predicate, _Context) ->
    undefined;
m(Id, Predicate, Context) ->
    m(rid(Id, Context), Predicate, Context).


%% Return a list of all edge predicates of this resource
op(#rsc{id=Id}, Context) ->
    m_edge:object_predicates(Id, Context);
op(undefined, _Context) -> 
    [];
op(Id, Context) ->
    op(rid(Id, Context), Context).

%% Used for dereferencing object edges inside template expressions
o(Id, _Context) ->
	fun(P, Context) -> o(Id, P, Context) end.

%% Return the list of objects with a certain predicate
o(#rsc{id=Id}, Predicate, Context) ->
	{rsc_list, m_edge:objects(Id, Predicate, Context)};
o(undefined, _Predicate, _Context) ->
    {rsc_list, []};
o(Id, Predicate, Context) ->
    o(rid(Id, Context), Predicate, Context).


%% Return the nth object in the predicate list
o(#rsc{id=Id}, Predicate, N, Context) ->
    case m_edge:object(Id, Predicate, N, Context) of
        undefined -> undefined;
        ObjectId -> #rsc{id=ObjectId}
    end;
o(undefined, _Predicate, _N, _Context) ->
    undefined;
o(Id, Predicate, N, Context) ->
    o(rid(Id, Context), Predicate, N, Context).

	
%% Return a list of all edge predicates to this resource
sp(#rsc{id=Id}, Context) ->
    m_edge:subject_predicates(Id, Context);
sp(undefined, _Context) -> 
    [];
sp(Id, Context) ->
    sp(rid(Id, Context), Context).

%% Used for dereferencing subject edges inside template expressions
s(Id, _Context) ->
	fun(P, Context) -> s(Id, P, Context) end.

%% Return the list of subjects with a certain predicate
s(#rsc{id=Id}, Predicate, Context) ->
	{rsc_list, m_edge:subjects(Id, Predicate, Context)};
s(undefined, _Predicate, _Context) ->
    {rsc_list, []};
s(Id, Predicate, Context) ->
    s(rid(Id, Context), Predicate, Context).

%% Return the nth object in the predicate list
s(#rsc{id=Id}, Predicate, N, Context) ->
    case m_edge:subject(Id, Predicate, N, Context) of
        undefined -> undefined;
        SubjectId -> #rsc{id=SubjectId}
    end;
s(undefined, _Predicate, _N, _Context) ->
    undefined;
s(Id, Predicate, N, Context) ->
    s(rid(Id, Context), Predicate, N, Context).


%% Return the list of all media attached to the resource
media(#rsc{id=Id}, Context) -> 
    m_media:get_rsc(Id, Context);
media(undefined, _Context) -> 
	[];
media(Id, Context) -> 
	media(rid(Id, Context), Context).


media(#rsc{id=Id}, N, Context) ->
    m_media:get_rsc(Id, N, Context);
media(undefined, _N, _Context) ->
	undefined;
media(Id, N, Context) ->
    media(rid(Id, Context), N, Context).
	
	
%% @doc Fetch a #rsc{} from any input
rid(#rsc{id=Id} = Rsc, _Context) ->
    Rsc;
rid(Id, _Context) when is_integer(Id) ->
	#rsc{id=Id};
rid({rsc_list, [R|_]}, _Context) ->
	R;
rid({rsc_list, []}, _Context) ->
	undefined;
rid([C|_] = UniqueName, Context) when is_list(UniqueName) and is_integer(C) ->
    rid_unique_name(UniqueName, Context);
rid(UniqueName, Context) when is_binary(UniqueName) ->
    rid_unique_name(binary_to_list(UniqueName), Context);
rid(undefined, _Context) -> 
	undefined;
rid(UniqueName, Context) when is_atom(UniqueName) -> 
    rid_unique_name(atom_to_list(UniqueName), Context);
rid(<<>>, _Context) -> 
	undefined.


%% @doc Return the id of the resource with a certain unique name.
%% rid_unique_name(UniqueName, Context) -> #rsc{} | undefined
rid_unique_name(UniqueName, Context) ->
    Name = zp_utils:to_lower(UniqueName),
    case zp_depcache:get({rsc_unique_name, Name}) of
        {ok, undefined} ->
            undefined;
        {ok, Id} ->
            Id;
        undefined ->
            Id = case zp_db:q1("select id from rsc where unique_name = $1", [Name], Context) of
                undefined -> undefined;
                Value -> #rsc{id=Value}
            end,
            zp_depcache:set({rsc_unique_name, Name}, Id, ?DAY, [Id,{rsc_unique_name, Name}]),
            Id
    end.

