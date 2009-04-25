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
    
    test/0
]).

-include_lib("zophrenic.hrl").


%% @doc Insert a new resource
%% @spec insert(Props, Context) -> {ok, Id}
insert(Props, Context) ->
    zp_db:insert(rsc, Props, Context).


%% @doc Delete a resource
%% @spec delete(Props, Context) -> void()
delete(Id, Context) when is_integer(Id) ->
    case zp_acl:rsc_editable(Id, Context) of
        true ->
            zp_db:delete(rsc, Id, Context),
            zp_depcache:flush(#rsc{id=Id}),
            ok;
        false ->
            {error, eacces}
    end.
    

%% @doc Update a predicate
%% @spec update(Props, Props, Context) -> void()
update(Id, Props, Context) when is_integer(Id) ->
    case zp_acl:rsc_editable(Id, Context) of
        true ->
            ?DEBUG(Props),
            TextProps = recombine_dates(Props),
            ?DEBUG(TextProps),
            AtomProps = [ {zp_convert:to_atom(P), V} || {P, V} <- TextProps ],
            ?DEBUG(AtomProps),
            FilteredProps = props_filter(Id, AtomProps, [], Context),
            ?DEBUG(FilteredProps),
            zp_db:update(rsc, Id, FilteredProps, Context),
            zp_depcache:flush(#rsc{id=Id}),
            ok;
        false ->
            {error, eacces}
    end.


props_filter(_Id, [], Acc, _Context) ->
    Acc;
props_filter(Id, [{is_published, P}|T], Acc, Context) ->
    props_filter(Id, T, [{is_published, zp_convert:to_bool(P)} | Acc], Context);
props_filter(Id, [{is_authoritative, P}|T], Acc, Context) ->
    props_filter(Id, T, [{is_authoritative, zp_convert:to_bool(P)} | Acc], Context);
props_filter(Id, [{is_featured, P}|T], Acc, Context) ->
    props_filter(Id, T, [{is_featured, zp_convert:to_bool(P)} | Acc], Context);
props_filter(Id, [{Prop, _V}=H|T], Acc, Context) ->
    case protected(Prop) of
        true ->
            props_filter(Id, T, Acc, Context);
        false ->
            props_filter(Id, T, [H|Acc], Context)
    end.


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
    Dates4 = lists:foldl(fun({Name, {S, E}}, Acc) -> [{Name++"_start", S}, {Name++"_end", E} | Acc] end, Dates3, DateGroups2),
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
    recombine_dates([
        {"dt:Y:0:publication_start", ""},
        {"dt:M:0:publication_start", ""},
        {"dt:D:0:publication_start", ""},
        {"dt:Y:1:publication_end", ""},
        {"dt:M:1:publication_end", ""},
        {"dt:D:1:publication_end", ""},
        {"plop", "hello"}
    ]).
