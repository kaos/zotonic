%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Model for resource data. Interfaces between zophrenic, templates and the database.

-module(m_rsc).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_model).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    name_to_id/2,
    name_to_id_check/2,
    
    get/2,
    get_acl_props/2,
    insert/2,
    delete/2,
    update/3,
        
	rsc/0,
	rsc/2,
	exists/2, 
	
	is_visible/2, is_editable/2, is_ingroup/2, is_me/2, 
	is_a/3, is_a_list/2,
	
	p/3, 
	op/2, o/2, o/3, o/4,
	sp/2, s/2, s/3, s/4,
	media/2, media/3,
	page_url/2
]).

-include_lib("zophrenic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Id, #m{value=undefined}, _Context) ->
    #rsc{id=Id};
m_find_value(Key, #m{value=#rsc{}} = M, Context) ->
    p(M#m.value, Key, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=#rsc_list{list=List}}, _Context) ->
    List;
m_to_list(#m{}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined;
m_value(#m{value=V}, _Context) ->
    V.

%% @doc Return the id of the resource with the name
% @spec name_to_id(NameString, Context) -> int() | undefined
name_to_id(Name, Context) ->
    case rid_name(Name, Context) of
        #rsc{id=Id} -> {ok, Id};
        _ -> {error, {enoent, rsc, Name}}
    end.

name_to_id_check(Name, Context) ->
    {ok, Id} = name_to_id(Name, Context),
    Id.


%% @doc Read a whole resource
%% @spec get(Id, Context) -> 
get(Id, Context) ->
    case rid(Id, Context) of
        #rsc{id=Rid} = Rsc ->
            case zp_depcache:get(Rsc) of
                {ok, Resource} -> 
                    Resource;
                undefined ->
                    case zp_db:select(rsc, Rid, Context) of
                        {ok, Record} ->
                            zp_depcache:set(Rsc, Record, ?WEEK, [Rsc]),
                            Record;
                        _ ->
                            zp_depcache:set(Rsc, undefined, ?WEEK, [Rsc]),
                            undefined
                    end
            end
    end.


%% @doc Get the ACL fields for the resource with the id. The id must be an integer
%% @spec get_acl_fields(Id, #context) -> #acl_props
get_acl_props(Id, Context) when is_integer(Id) ->
    F = fun() ->
        case zp_db:q_row("
            select is_published, visible_for, group_id, publication_start, publication_end 
            from rsc 
            where id = $1", [Id], Context) of
    
            {IsPub, Vis, Group, PubS, PubE} ->
                #acl_props{is_published=IsPub, visible_for=Vis, group_id=Group, publication_start=PubS, publication_end=PubE};
            false ->
                #acl_props{is_published=false, visible_for=3, group_id=0}
        end
    end,
    zp_depcache:memo(F, {rsc_acl_fields, Id}, ?DAY, [#rsc{id=Id}]).


%% @doc Insert a new resource
%% @spec insert(Props, Context) -> {ok, Id}
insert(Props, Context) ->
    zp_db:insert(rsc, Props, Context).

%% @doc Delete a resource
%% @spec delete(Props, Context) -> void()
delete(Id, Context) when is_integer(Id) ->
    zp_db:delete(rsc, Id, Context),
    zp_depcache:flush(#rsc{id=Id}).


%% @doc Update a predicate
%% @spec update(Props, Props, Context) -> void()
update(Id, Props, Context) when is_integer(Id) ->
    zp_db:update(rsc, Id, Props, Context),
    zp_depcache:flush(#rsc{id=Id}).


%% Function used in template contexts to return a #rsc{id=Id} --> not used anymore???
rsc() -> fun(Id, _Context) -> #rsc{id=Id} end.
rsc(Id, _Context) -> #rsc{id=Id}.

exists([C|_] = Name, Context) when is_list(Name) and is_integer(C) ->
    case rid_name(Name, Context) of
        undefined -> false;
        _ -> true
    end;
exists(Name, Context) when is_binary(Name) ->
    case rid_name(Name, Context) of
        undefined -> false;
        _ -> true
    end;
exists(Id, Context) -> 
    case rid(Id, Context) of
        #rsc{id=Rid} = Rsc ->
            case zp_depcache:get({exists, Rid}) of
                {ok, Exists} ->
                    Exists;
                undefined -> 
                    Exists = case zp_db:q1("select id from rsc where id = $1", [Rid], Context) of
                        undefined -> false;
                        _ -> true
                    end,
                    zp_depcache:set({exists, Rid}, Exists, ?DAY, [Rsc]),
                    Exists
            end;
        undefined -> false
    end.
    
is_visible(Id, Context) ->
    case rid(Id, Context) of
        #rsc{id=RscId} ->
            zp_acl:rsc_visible(RscId, Context);
        _ ->
            false
    end.

is_editable(Id, Context) -> 
    case rid(Id, Context) of
        #rsc{id=RscId} ->
            zp_acl:rsc_editable(RscId, Context);
        _ ->
            false
    end.
    
is_ingroup(Id, Context) -> 
    case rid(Id, Context) of
        #rsc{id=RscId} ->
            zp_acl:rsc_ingroup(RscId, Context);
        _ ->
            false
    end.

is_me(Id, Context) -> 
    case rid(Id, Context) of
        #rsc{id=RscId} ->
            zp_acl:person(Context) == RscId;
        _ ->
            false
    end.

%% @todo Perform access control checks, return 'undefined' on an error or permission denial

% List of special properties to be redirected to functions
p(Id, media, Context) -> media(Id, Context);
p(Id, o, Context)  -> o(Id, Context);
p(Id, s, Context)  -> s(Id, Context);
p(Id, m, Context)  -> m(Id, Context);
p(Id, op, Context) -> op(Id, Context);
p(Id, sp, Context) -> sp(Id, Context);
p(Id, is_me, Context) -> is_me(Id, Context);
p(Id, is_visible, Context) -> is_visible(Id, Context);
p(Id, is_editable, Context) -> is_editable(Id, Context);
p(Id, is_ingroup, Context) -> is_ingroup(Id, Context);
p(Id, is_a, Context) -> [ {C,true} || C <- is_a_list(Id, Context) ];
p(Id, exists, Context) -> exists(Id, Context);
p(Id, page_url, Context) -> page_url(Id, Context);
p(Id, group, Context) -> 
    case p(Id, group_id, Context) of
        undefined -> undefined;
        GroupId -> m_group:get(GroupId, Context)
    end;
p(Id, category, Context) -> 
    m_category:get(p(Id, category_id, Context), Context);
    
% Check if the requested predicate is a readily available property or an edge
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
            % Unknown properties will be checked against the predicates, returns o(Predicate).
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
	#rsc_list{list=m_edge:objects(Id, Predicate, Context)};
o(undefined, _Predicate, _Context) ->
    #rsc_list{list=[]};
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
	#rsc_list{list=m_edge:subjects(Id, Predicate, Context)};
s(undefined, _Predicate, _Context) ->
    #rsc_list{list=[]};
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
rid(#rsc{} = Rsc, _Context) ->
    Rsc;
rid(Id, _Context) when is_integer(Id) ->
	#rsc{id=Id};
rid(#rsc_list{list=[R|_]}, _Context) ->
	R;
rid(#rsc_list{list=[]}, _Context) ->
	undefined;
rid([C|_] = UniqueName, Context) when is_list(UniqueName) and is_integer(C) ->
    case only_digits(UniqueName) of
        true -> #rsc{id=list_to_integer(UniqueName)};
        false -> rid_name(UniqueName, Context)
    end;
rid(UniqueName, Context) when is_binary(UniqueName) ->
    rid_name(binary_to_list(UniqueName), Context);
rid(undefined, _Context) -> 
	undefined;
rid(UniqueName, Context) when is_atom(UniqueName) -> 
    rid_name(atom_to_list(UniqueName), Context);
rid(<<>>, _Context) -> 
	undefined.


only_digits([]) -> 
    true;
only_digits([C|R]) when C >= $0 andalso C =< $9 ->
    only_digits(R);
only_digits(_) ->
    false.

%% @doc Return the id of the resource with a certain unique name.
%% rid_name(Name, Context) -> #rsc{} | undefined
rid_name(Name, Context) ->
    Lower = zp_utils:to_lower(Name),
    case zp_depcache:get({rsc_name, Lower}) of
        {ok, undefined} ->
            undefined;
        {ok, Id} ->
            Id;
        undefined ->
            Id = case zp_db:q1("select id from rsc where name = $1", [Lower], Context) of
                undefined -> undefined;
                Value -> #rsc{id=Value}
            end,
            zp_depcache:set({rsc_name, Lower}, Id, ?DAY, [Id]),
            Id
    end.

%% @doc Check if the resource is inside a category
is_a(Id, Cat, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
            RscCatId = p(Id, category_id, Context),
            case RscCatId of
                CatId ->
                    true;
                _ ->
                    Path = m_category:get_path(RscCatId, Context),
                    lists:any(fun(X) -> X == CatId end, Path)
            end;
        _ ->
            false
    end.

is_a_list(Id, Context) ->
    RscCatId = p(Id, category_id, Context),
    Path = m_category:get_path(RscCatId, Context),
    [ zp_convert:to_atom(m_category:id_to_name(C, Context)) || C <- Path ++ [RscCatId]].
    

page_url(Id, Context) ->
    case rid(Id, Context) of
        #rsc{id=RscId} ->
            CatId = p(Id, category_id, Context),
            Args = [{id,RscId},{slug, p(Id, slug, Context)}],
            case CatId of
                undefined ->
                    page_url_path([], Args, Context);
                _ ->
                    CatPath = lists:reverse(m_category:get_path(CatId, Context) ++ [CatId]),
                    page_url_path(CatPath, Args, Context)
            end;
        _ ->
            undefined
    end.

page_url_path([], Args, Context) ->
    zp_dispatcher:url_for(page, Args, Context);
page_url_path([CatId|Rest], Args, Context) ->
    Name = m_category:id_to_name(CatId, Context),
    case zp_dispatcher:url_for(Name, Args, Context) of
        undefined -> page_url_path(Rest, Args, Context);
        Url -> Url
    end.
