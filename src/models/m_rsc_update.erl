%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-24
%%
%% @doc Update routines for resources.  For use by the m_rsc module.

-module(m_rsc_update).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    insert/2,
    delete/2,
    update/3,
    
    delete_nocheck/2,
    props_filter/3,
    
    test/0
]).

-include_lib("zophrenic.hrl").


%% @doc Insert a new resource. Crashes when insertion is not allowed.
%% @spec insert(Props, Context) -> {ok, Id} | {error, Reason}
insert(Props, Context) ->
    PropsDefaults = zp_acl:add_defaults(Props, Context),
    update(insert_rsc, PropsDefaults, Context).


%% @doc Delete a resource
%% @spec delete(Props, Context) -> ok | {error, Reason}
delete(Id, Context) when is_integer(Id), Id /= 1 ->
    case zp_acl:rsc_editable(Id, Context) of
        true ->
            case m_rsc:is_a(Id, category, Context) of
                true ->
                    m_category:delete(Id, undefined, Context);
                false ->
                    delete_nocheck(Id, Context)
            end;
        false ->
            {error, eacces}
    end.


%% @doc Delete a resource, no check on rights etc is made. This is called by m_category:delete/3
%% @spec delete_nocheck(Id, Context) -> ok | {error, Reason}
delete_nocheck(Id, Context) ->
    Referrers = m_edge:subjects(Id, Context),
    CatList = m_rsc:is_a(Id, Context),

    F = fun(Ctx) ->
        zp_notifier:notify({rsc_delete, Id}, Ctx),
        zp_db:delete(rsc, Id, Ctx)
    end,

    case zp_db:transaction(F, Context) of
        {ok, _RowsDeleted} ->
            % After inserting a category we need to renumber the categories
            case lists:member(category, CatList) of
                true ->  m_category:renumber(Context);
                false -> nop
            end,
    
            % Flush all cached entries depending on this entry, one of its subjects or its categories.
            zp_depcache:flush(Id),
            [ zp_depcache:flush(SubjectId) || SubjectId <- Referrers ],
            [ zp_depcache:flush(Cat) || Cat <- CatList ],
    
            % Notify all modules that the rsc has been deleted
            zp_notifier:notify({rsc_update_done, delete, Id, CatList, []}, Context),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.
    

%% @doc Update a resource
%% @spec update(Id, Props, Context) -> {ok, Id} | {error, Reason}
update(Id, Props, Context) when is_integer(Id) orelse Id == insert_rsc ->
    case Id == insert_rsc orelse zp_acl:rsc_editable(Id, Context) of
        true ->
            TextProps = recombine_dates(Props),
            AtomProps = [ {zp_convert:to_atom(P), V} || {P, V} <- TextProps ],
            FilteredProps = props_filter(AtomProps, [], Context),
            EditableProps = props_filter_protected(FilteredProps),
            FilledProps = props_defaults(EditableProps, Context),
            SafeProps = zp_html:escape_props(FilledProps),
            case preflight_check(Id, SafeProps, Context) of
                ok ->
                    % This function will be executed in a transaction
                    TransactionF = fun(Ctx) ->
                        {RscId, UpdateProps, BeforeProps, BeforeCatList, RenumberCats} = case Id of
                            insert_rsc ->
                                CategoryId = proplists:get_value(category_id, SafeProps),
                                GroupId = proplists:get_value(group_id, SafeProps),
                                InsProps = [{category_id, CategoryId}, {group_id, GroupId}, {version, 0}],

                                % Allow the insertion props to be modified.
                                InsPropsN = zp_notifier:foldr({rsc_insert}, InsProps, Ctx),
                                
                                % Check if the user is allowed to add to the group of the new rsc, if so proceed
                                GroupIdN = proplists:get_value(group_id, InsPropsN),
                                case zp_acl:group_editable(GroupIdN, Ctx) of
                                    true ->
                                        {ok, InsId} = zp_db:insert(rsc, InsPropsN, Ctx),

                                        % Insert a category record for categories. Categories are so low level that we want
                                        % to make sure that all categories always have a category record attached.
                                        InsertCatList = [ CategoryId | m_category:get_path(CategoryId, Ctx) ],
                                        IsACat = case lists:member(m_category:name_to_id_check(category, Ctx), InsertCatList) of
                                            true ->
                                                1 = zp_db:q("insert into category (id, seq) values ($1, 1)", [InsId], Ctx),
                                                true;
                                            false ->
                                                false
                                        end,

                                        % Place the inserted properties over the update properties, replacing duplicates.
                                        SafePropsN = lists:foldl(
                                                                fun
                                                                    ({version, _}, Acc) -> Acc;
                                                                    ({P,V}, Acc) -> zp_utils:prop_replace(P, V, Acc) 
                                                                end,
                                                                SafeProps, 
                                                                InsPropsN),
                                        {InsId, SafePropsN, InsPropsN, [], IsACat};
                                    false ->
                                        throw({error, eacces})
                                end;
                            _ ->
                                {Id, SafeProps, m_rsc:get(Id, Ctx), m_rsc:is_a(Id, Ctx), false}
                        end,
                    
                        UpdateProps1 = [
                            {version, zp_db:q1("select version+1 from rsc where id = $1", [RscId], Ctx)},
                            {modifier_id, zp_acl:user(Ctx)},
                            {modified, erlang:universaltime()}
                            | UpdateProps
                        ],
                        
                        % Allow the update props to be modified.
                        UpdatePropsN = zp_notifier:foldr({rsc_update, RscId, BeforeProps}, UpdateProps1, Ctx),
                        UpdatePropsN1 = case proplists:get_value(category_id, UpdatePropsN) of
                            undefined ->
                                UpdatePropsN;
                            CatId ->
                                CatNr = zp_db:q1("select nr from category where id = $1", [CatId], Ctx),
                                [ {pivot_category_nr, CatNr} | UpdatePropsN]
                        end,
                        
                        case zp_db:update(rsc, RscId, UpdatePropsN1, Ctx) of
                            {ok, _RowsModified} -> {ok, RscId, UpdatePropsN, BeforeCatList, RenumberCats};
                            {error, Reason} -> {error, Reason}
                        end                        
                    end,
                    % End of transaction function
                    
                    case zp_db:transaction(TransactionF, Context) of
                        {ok, NewId, NewProps, OldCatList, RenumberCats} ->    
                            zp_depcache:flush(NewId),
                            case proplists:get_value(name, NewProps) of
                                undefined -> nop;
                                Name -> zp_depcache:flush({rsc_name, zp_convert:to_list(Name)})
                            end,

                            NewCatList = m_rsc:is_a(NewId, Context),
                            AllCatList = lists:usort(NewCatList ++ OldCatList),
                            
                            % After inserting a category we need to renumber the categories
                            case RenumberCats of
                                true ->  m_category:renumber(Context);
                                false -> nop
                            end,
                            
                            % Flush all cached content that is depending on one of the updated categories
                            [ zp_depcache:flush(Cat) || Cat <- AllCatList ],

                            % Notify that a new resource has been inserted, or that an existing one is updated
                            case Id of
                                insert_rsc -> zp_notifier:notify({rsc_update_done, update, NewId, OldCatList, NewCatList}, Context);
                                _ ->          zp_notifier:notify({rsc_update_done, insert, NewId, OldCatList, NewCatList}, Context)
                            end,
                            
                            % Return the updated or inserted id
                            {ok, NewId};
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, eacces}
    end.


%% @doc Check if all props are acceptable. Examples are unique name, uri etc.
%% @spec preflight_check(Id, Props, Context) -> ok | {error, Reason}
preflight_check(insert_rsc, Props, Context) ->
    preflight_check(-1, Props, Context);
preflight_check(_Id, [], _Context) ->
    ok;
preflight_check(Id, [{name, Name}|T], Context) ->
    case zp_db:q1("select count(*) from rsc where name = $1 and id <> $2", [Name, Id], Context) of
        0 ->  preflight_check(Id, T, Context);
        _N -> {error, duplicate_name}
    end;
preflight_check(Id, [{page_path, Path}|T], Context) ->
    case zp_db:q1("select count(*) from rsc where page_path = $1 and id <> $2", [Path, Id], Context) of
        0 ->  preflight_check(Id, T, Context);
        _N -> {error, duplicate_page_path}
    end;
preflight_check(Id, [{uri, Uri}|T], Context) ->
    case zp_db:q1("select count(*) from rsc where uri = $1 and id <> $2", [Uri, Id], Context) of
        0 ->  preflight_check(Id, T, Context);
        _N -> {error, duplicate_uri}
    end;
preflight_check(Id, [_H|T], Context) ->
    preflight_check(Id, T, Context).


%% @doc Remove protected properties and convert some other to the correct data type
%% @spec props_filter(Props1, Acc, Context) -> Props2
props_filter([], Acc, _Context) ->
    Acc;
props_filter([{uri, <<>>}|T], Acc, Context) ->
    props_filter(T, [{uri, undefined} | Acc], Context);
props_filter([{uri, ""}|T], Acc, Context) ->
    props_filter(T, [{uri, undefined} | Acc], Context);

props_filter([{name, <<>>}|T], Acc, Context) ->
    props_filter(T, [{name, undefined} | Acc], Context);
props_filter([{name, ""}|T], Acc, Context) ->
    props_filter(T, [{name, undefined} | Acc], Context);
props_filter([{name, Name}|T], Acc, Context) ->
    props_filter(T, [{name, zp_string:to_name(Name)} | Acc], Context);

props_filter([{page_path, <<>>}|T], Acc, Context) ->
    props_filter(T, [{page_path, undefined} | Acc], Context);
props_filter([{page_path, ""}|T], Acc, Context) ->
    props_filter(T, [{page_path, undefined} | Acc], Context);
props_filter([{page_path, Path}|T], Acc, Context) ->
    Tokens = string:tokens(zp_utils:to_list(Path), "/"),
    AsSlug = lists:map(fun(X) -> zp_string:to_slug(X) end, Tokens),
    case string:strip(string:join(AsSlug, "/"), both, $/) of
        [] -> props_filter(T, [{page_path, undefined} | Acc], Context);
        P  -> props_filter(T, [{page_path, P} | Acc], Context)
    end;

props_filter([{slug, Slug}|T], Acc, Context) ->
    props_filter(T, [{slug, zp_string:to_slug(Slug)} | Acc], Context);

props_filter([{is_published, P}|T], Acc, Context) ->
    props_filter(T, [{is_published, zp_convert:to_bool(P)} | Acc], Context);
props_filter([{is_authoritative, P}|T], Acc, Context) ->
    props_filter(T, [{is_authoritative, zp_convert:to_bool(P)} | Acc], Context);
props_filter([{is_featured, P}|T], Acc, Context) ->
    props_filter(T, [{is_featured, zp_convert:to_bool(P)} | Acc], Context);

props_filter([{visible_for, Vis}|T], Acc, Context) ->
    VisibleFor = zp_convert:to_integer(Vis),
    case VisibleFor of
        N when N==0; N==1; N==2 ->
            case N >= zp_acl:publish_level(Context) of
                true -> 
                    props_filter(T, [{visible_for, N} | Acc], Context);
                false ->
                    % Do not let the user upgrade visibility beyond his permissions
                    props_filter(T, Acc, Context)
            end;
        _ ->
            props_filter(T, Acc, Context)
    end;

props_filter([{group_id, GId}|T], Acc, Context) ->
    GroupId = zp_convert:to_integer(GId),
    case GroupId of
        undefined -> 
            props_filter(T, Acc, Context);
        N when N > 0 ->
            case zp_acl:has_role(admin, Context) of
                true ->
                    props_filter(T, [{group_id, GroupId}|Acc], Context);
                false ->
                    Gs = zp_acl:groups_member(Context),
                    case lists:member(GroupId, Gs) of
                        true -> props_filter(T, [{group_id, GroupId}|Acc], Context);
                        false -> throw({error, eacces})
                    end
            end
    end;
props_filter([{group, GroupName}|T], Acc, Context) ->
    props_filter([{group_id, m_group:name_to_id_check(GroupName, Context)} | T], Acc, Context);

props_filter([{category, CatName}|T], Acc, Context) ->
    props_filter([{category_id, m_category:name_to_id_check(CatName, Context)} | T], Acc, Context);

props_filter([{_Prop, _V}=H|T], Acc, Context) ->
    props_filter(T, [H|Acc], Context).


%% @doc Fill in some defaults for empty props.
%% @spec props_defaults(Props1, Context) -> Props2
props_defaults(Props, _Context) ->
    case proplists:get_value(slug, Props) of
        L when L == <<>>; L == [] ->
            case proplists:get_value(title, Props) of
                undefined ->
                    Props;
                Title ->
                    Text = ?TR(Title, en),
                    Slug = zp_string:to_slug(Text),
                    lists:keystore(slug, 1, Props, {slug, Slug})
            end;
        _ -> 
            Props
    end.


props_filter_protected(Props) ->
    lists:filter(fun({K,_V}) -> not protected(K) end, Props).

%% @doc Properties that can't be updated with m_rsc_update:update/3 or m_rsc_update:insert/2
protected(id) -> true;
protected(creator_id) -> true;
protected(modifier_id) -> true;
protected(created) -> true;
protected(modified) -> true;
protected(version) -> true;
protected(comment_by) -> true;
protected(comments) -> true;
protected(rating) -> true;
protected(rating_count) -> true;
protected(props) -> true;
protected(pivot_tsv) -> true;
protected(pivot_rtsv) -> true;
protected(pivot_category_nr) -> true;
protected(pivot_first_name) -> true;
protected(pivot_surname) -> true;
protected(pivot_gender) -> true;
protected(pivot_date_start) -> true;
protected(pivot_date_end) -> true;
protected(pivot_date_start_month_day) -> true;
protected(pivot_date_end_month_day) -> true;
protected(pivot_street) -> true;
protected(pivot_city) -> true;
protected(pivot_state) -> true;
protected(pivot_postcode) -> true;
protected(pivot_country) -> true;
protected(pivot_geocode) -> true;
protected(_) -> false.


recombine_dates(Props) ->
    Now = erlang:localtime(),
    {Dates, Props1} = recombine_dates(Props, [], []),
    {Dates1, DateGroups} = group_dates(Dates),
    {DateGroups1, DatesNull} = collect_empty_date_groups(DateGroups, [], []),
    {Dates2, DatesNull1} = collect_empty_dates(Dates1, [], DatesNull),
    Dates3 = [ {Name, date_from_default(Now, D)} || {Name, D} <- Dates2 ],
    DateGroups2 = [ {Name, dategroup_fill_parts(date_from_default(Now, S), E)} || {Name, {S,E}} <- DateGroups1 ],
    Dates4 = lists:foldl(fun({Name, {S, E}}, Acc) -> [{Name++"_start", zp_utils:to_utc(S)}, {Name++"_end", zp_utils:to_utc(E)} | Acc] end, Dates3, DateGroups2),
    Dates4 ++ DatesNull1 ++ Props1.


collect_empty_date_groups([], Acc, Null) ->
    {Acc, Null};
collect_empty_date_groups([{"publication", _} = R|T], Acc, Null) ->
    collect_empty_date_groups(T, [R|Acc], Null);
collect_empty_date_groups([{Name, {
                            {{undefined, undefined, undefined}, {undefined, undefined, undefined}}, 
                            {{undefined, undefined, undefined}, {undefined, undefined, undefined}}
                            }}|T], Acc, Null) ->
    collect_empty_date_groups(T, Acc, [{Name++"_start", undefined}, {Name++"_end", undefined} | Null]);
collect_empty_date_groups([H|T], Acc, Null) ->
    collect_empty_date_groups(T, [H|Acc], Null).



collect_empty_dates([], Acc, Null) ->
    {Acc, Null};
collect_empty_dates([{Name, {{undefined, undefined, undefined}, {undefined, undefined, undefined}}}|T], Acc, Null) ->
    collect_empty_dates(T, Acc, [{Name, undefined}|Null]);
collect_empty_dates([H|T], Acc, Null) ->
    collect_empty_dates(T, [H|Acc], Null).
    

    
recombine_dates([], Dates, Acc) ->
    {Dates, Acc};
recombine_dates([{"dt:"++K,V}|T], Dates, Acc) ->
    [Part, End, Name] = string:tokens(K, ":"),
    Dates1 = recombine_date(Part, End, Name, V, Dates),
    recombine_dates(T, Dates1, Acc);
recombine_dates([H|T], Dates, Acc) ->
    recombine_dates(T, Dates, [H|Acc]).

    recombine_date(Part, _End, Name, V, Dates) ->
        Date = case proplists:get_value(Name, Dates) of
            undefined -> 
                {{undefined, undefined, undefined}, {undefined, undefined, undefined}};
            D ->
                D
        end,
        Date1 = recombine_date_part(Date, Part, to_int(string:strip(V))),
        lists:keystore(Name, 1, Dates, {Name, Date1}).
    
    recombine_date_part({{_Y,M,D},{H,I,S}}, "y", V) -> {{V,M,D},{H,I,S}};
    recombine_date_part({{Y,_M,D},{H,I,S}}, "m", V) -> {{Y,V,D},{H,I,S}};
    recombine_date_part({{Y,M,_D},{H,I,S}}, "d", V) -> {{Y,M,V},{H,I,S}};
    recombine_date_part({{Y,M,D},{_H,I,S}}, "h", V) -> {{Y,M,D},{V,I,S}};
    recombine_date_part({{Y,M,D},{H,_I,S}}, "i", V) -> {{Y,M,D},{H,V,S}};
    recombine_date_part({{Y,M,D},{H,I,_S}}, "s", V) -> {{Y,M,D},{H,I,V}}.


group_dates(Dates) ->
    group_dates(Dates, [], []).

    group_dates([], Groups, Acc) ->
        {Acc, Groups};
    group_dates([{Name,D}|T], Groups, Acc) ->
        case lists:suffix("_start", Name) of
            true ->
                Base = lists:sublist(Name, length(Name) - 6),
                Range = case proplists:get_value(Base, Groups) of
                    {_Start, End} -> 
                        { D, End };
                    undefined ->
                        { D, {{undefined, undefined, undefined}, {undefined, undefined, undefined}} }
                end,
                Groups1 = lists:keystore(Base, 1, Groups, {Base, Range}),
                group_dates(T, Groups1, Acc);

            false ->
                case lists:suffix("_end", Name) of
                    true ->
                        Base = lists:sublist(Name, length(Name) - 4),
                        Range = case proplists:get_value(Base, Groups) of
                            {Start, _End} -> 
                                { Start, D };
                            undefined ->
                                { {{undefined, undefined, undefined}, {0, 0, 0}}, D }
                        end,
                        Groups1 = lists:keystore(Base, 1, Groups, {Base, Range}),
                        group_dates(T, Groups1, Acc);

                    false ->
                        group_dates(T, Groups, [T|Acc])
                end
        end.
    

dategroup_fill_parts( S, {{undefined,undefined,undefined},{undefined,undefined,undefined}} ) ->
    {S, {{9999,6,1},{0,0,0}}};
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{undefined,Me,De},{He,Ie,Se}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Me,De},{He,Ie,Se}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,undefined,De},{He,Ie,Se}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}} ,{{Ye,Ms,De},{He,Ie,Se}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,undefined},{He,Ie,Se}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,Ds},{He,Ie,Se}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{undefined,Ie,Se}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{23,Ie,Se}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,undefined,Se}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,59,Se}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,undefined}} ) ->
    dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,59}} );
dategroup_fill_parts( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,Se}} ) ->
    {{{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,Se}}}.


date_from_default( S, {{undefined,undefined,undefined},{undefined,undefined,undefined}} ) ->
    S;
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{undefined,Me,De},{He,Ie,Se}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ys,Me,De},{He,Ie,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,undefined,De},{He,Ie,Se}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Ms,De},{He,Ie,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,undefined},{He,Ie,Se}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,Ds},{He,Ie,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{undefined,Ie,Se}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{0,Ie,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,undefined,Se}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,0,Se}} );
date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,undefined}} ) ->
    date_from_default( {{Ys,Ms,Ds},{Hs,Is,Ss}}, {{Ye,Me,De},{He,Ie,0}} );
date_from_default( _S, {{Ye,Me,De},{He,Ie,Se}} ) ->
    {{Ye,Me,De},{He,Ie,Se}}.

to_int("") ->
    undefined;
to_int(A) -> 
    try
        list_to_integer(A)
    catch
        _:_ -> undefined
    end.


test() ->
    [{"publication_start",{{2009,7,9},{0,0,0}}},
          {"publication_end",{{9999,6,1},{0,0,0}}},
          {"plop","hello"}]
     = recombine_dates([
        {"dt:y:0:publication_start", "2009"},
        {"dt:m:0:publication_start", "7"},
        {"dt:d:0:publication_start", "9"},
        {"dt:y:1:publication_end", ""},
        {"dt:m:1:publication_end", ""},
        {"dt:d:1:publication_end", ""},
        {"plop", "hello"}
    ]),
    ok.

