%%%-------------------------------------------------------------------
%%% Adapted and expanded for Zotonic by Marc Worrell <marc@worrell.nl>
%%%-------------------------------------------------------------------

-module(erlydtl_runtime).
-compile(export_all).

-include_lib("zotonic.hrl").

% Finde the value of a model value
find_value(<<>>, #m{}, _Context) ->
    undefined;
find_value(undefined, #m{}, _Context) ->
    undefined;
find_value(Key, #m{model=Model} = M, Context) ->
    Model:m_find_value(Key, M, Context);
    
% Index of list with an integer like "a[2]"
find_value(Key, L, _Context) when is_integer(Key), is_list(L) ->
    try
        lists:nth(Key, L)
    catch
        _:_ -> undefined
    end;

%% q and q_validated are indexed with strings, this because the indices are from
%% the query string and post. Wrap them in a 'q' tuple to force a subsequent lookup.
find_value(Key, q, Context) ->
    z_context:get_q(z_convert:to_list(Key), Context);
find_value(Key, q_validated, Context) ->
    z_context:get_q_validated(z_convert:to_list(Key), Context);
find_value(q, _Vars, _Context) ->
    q;
find_value(q_validated, _Vars, _Context) ->
    q_validated;

%% Assume a predicate/property lookup in a list of ids, map to lookup of first entry
find_value(Key, [N|_], Context) when is_atom(Key), is_integer(N) ->
	m_rsc:p(N, Key, Context);

%% Property of a resource, just assume an integer is a rsc id
find_value(Key, Id, Context) when is_integer(Id) ->
	m_rsc:p(Id, Key, Context);

%% Lookup current date/time, defaults to calendar:local_time/0
find_value(now, L, _Context) when is_list(L) ->
    case proplists:get_value(now, L) of
        undefined -> calendar:local_time();
        Now -> Now
    end;

%% Regular proplist lookup
find_value(Key, L, _Context) when is_list(L) ->
    proplists:get_value(Key, L);

%% Resource list handling, special lookups when skipping the index
find_value(Key, #rsc_list{list=L}, _Context) when is_integer(Key) ->
    try
        lists:nth(Key, L)
    catch
        _:_ -> undefined
    end;
find_value(Key, #rsc_list{list=[H|_T]}, Context) ->
	find_value(Key, H, Context);
find_value(_Key, #rsc_list{list=[]}, _Context) ->
	undefined;

% Index of tuple with an integer like "a[2]"
find_value(Key, T, _Context) when is_integer(Key) andalso is_tuple(T) ->
    case element(1,T) of
        dict ->
            case dict:find(Key, T) of
                {ok, Val} ->
                    Val;
                _ ->
                    undefined
            end;
        _ ->
            try
                element(Key, T)
            catch 
                _:_ -> undefined
            end
    end;

% Search results
find_value(Key, #search_result{} = S, _Context) when is_integer(Key) ->
    try
        lists:nth(Key, S#search_result.result)
    catch
        _:_ -> undefined
    end;
find_value(Key, #search_result{} = S, _Context) ->
    case Key of
        result -> S#search_result.result;
        all -> S#search_result.all;
        total -> S#search_result.total;
        page -> S#search_result.page;
        pages -> S#search_result.pages
    end;

%% Other cases: context, dict or parametrized module lookup.
find_value(Key, Tuple, Context) when is_tuple(Tuple) ->
    Module = element(1, Tuple),
    case Module of
        context -> 
            z_context:get_value(Key, Tuple);
        dict -> 
            case dict:find(Key, Tuple) of
                {ok, Val} -> Val;
                _ -> undefined
            end;
        Module ->
            Exports = Module:module_info(exports),
            case proplists:get_value(Key, Exports) of
                0 -> Tuple:Key();
                1 -> Tuple:Key(Context);
                _ ->
                    case proplists:get_value(get, Exports) of
                        1 -> Tuple:get(Key);
                        2 -> Tuple:get(Key, Context);
                        _ ->
                            undefined
                    end
            end
    end;

%% When the current value lookup is a function, the context can be passed to F
find_value(Key, F, Context) when is_function(F, 2) ->
	F(Key, Context);
find_value(Key, F, _Context) when is_function(F, 1) ->
	F(Key);

%% Any subvalue of a non-existant value is undefined
find_value(_Key, undefined, _Context) ->
	undefined;
find_value(_Key, <<>>, _Context) ->
	undefined.

%% This used to translate undefined into <<>>, this translation is now done by z_render:render/2
fetch_value(Key, Data, Context) ->
    find_value(Key, Data, Context).

are_equal(Arg1, Arg2) ->
    z_utils:are_equal(Arg1, Arg2).
    
is_false([]) ->
    true;
is_false(false) ->
    true;
is_false(undefined) ->
    true;
is_false(0) ->
    true;
is_false("0") ->
    true;
is_false(<<"0">>) ->
    true;
is_false(<<>>) ->
    true;
is_false({rsc_list, []}) ->
    true;
is_false(#m{value=V}) ->
    is_false(V);
is_false(#m_search_result{result=V}) ->
    is_false(V);
is_false(#search_result{result=[]}) ->
    true;
is_false(_) ->
    false.


init_counter_stats(List) ->
    init_counter_stats(List, undefined).

init_counter_stats(List, Parent) ->
    N = length(List),
    [{counter, 1}, 
        {counter0, 0}, 
        {revcounter, N}, 
        {revcounter0, N - 1}, 
        {first, true}, 
        {last, N =:= 1},
        {parentloop, Parent}].


to_list(#m{model=Model} = M, Context) -> Model:m_to_list(M, Context);
to_list(#rsc_list{list=L}, _Context) -> L;
to_list(#search_result{result=L}, _Context) -> L;
to_list(q, Context) -> z_context:get_q_all(Context);
to_list(q_validated, _Context) -> [];
to_list(L, _Context) when is_list(L) -> L;
to_list(T, _Context) when is_tuple(T) -> tuple_to_list(T);
to_list(N, _Context) -> z_convert:to_list(N).

to_value(#m{model=Model} = M, Context) ->
    Model:m_value(M, Context);
to_value(V, _Context) ->
    V.
    
increment_counter_stats([{counter, Counter}, {counter0, Counter0}, {revcounter, RevCounter},
         {revcounter0, RevCounter0}, {first, _}, {last, _}, {parentloop, Parent}]) ->
    [{counter, Counter + 1},
        {counter0, Counter0 + 1},
        {revcounter, RevCounter - 1},
        {revcounter0, RevCounter0 - 1},
        {first, false}, {last, RevCounter0 =:= 1},
        {parentloop, Parent}].


cycle(NamesTuple, Counters, Context) when is_tuple(NamesTuple) ->
    element(fetch_value(counter0, Counters, Context) rem size(NamesTuple) + 1, NamesTuple).


cache(MaxAge, Name, Args, Func, Context) ->
    DoCache = case z_convert:to_bool(proplists:get_value(if_anonymous, Args, false)) of
        true -> z_acl:user(Context) =:= undefined;
        false -> true
    end,
    case DoCache of
        false ->
            Func(Context);
        true ->
            Varies = lists:flatten(proplists:get_all_values(vary, Args)),
            Cat = proplists:get_all_values(cat, Args),
            Cat1 = lists:map(fun z_convert:to_atom/1, Cat),
            VisibleFor = z_acl:args_to_visible_for(Args),
            FuncContext = z_acl:set_visible_for(VisibleFor, Context),
            Key = {Name, Varies, z_acl:cache_key(FuncContext)},
            F = fun() ->
                Func(FuncContext)
            end,
            z_depcache:memo(F, Key, MaxAge, Varies ++ Cat1, FuncContext)
    end.
