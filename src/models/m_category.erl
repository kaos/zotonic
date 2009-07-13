%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-08
%%
%% @doc Model for categories.  Add, change and re-order categories.

-module(m_category).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    get/2,
    get_by_name/2,
    get_by_parent/2,
    get_root/1,
    get_range/2,
    get_range_by_name/2,
    get_path/2,
    get_page_count/2,
    delete/3,
    name_to_id/2,
    name_to_id_check/2,
    id_to_name/2,
    move_below/3,
    move_end/2,
    move_before/3,
    update_sequence/2,
    all_flat/1,
    tree/1,
    tree/2,
    tree_depth/2,
    tree_depth/3,
    renumber/1,
    enumerate/1
]).


-include_lib("zophrenic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(tree, #m{value=undefined}, Context) ->
    tree(Context);
m_find_value(tree1, #m{value=undefined}, Context) ->
    get_root(Context);
m_find_value(tree2, #m{value=undefined}, Context) ->
    tree_depth(2, Context);
m_find_value(all_flat, #m{value=undefined}, Context) ->
    all_flat(Context);

m_find_value(Index, #m{value=undefined} = M, Context) ->
    case name_to_id(Index, Context) of
        {ok, Id} -> M#m{value={cat, Id}};
        {error, _} -> undefined
    end;

m_find_value(tree, #m{value={cat, Id}}, Context) ->
    tree(Id, Context);
m_find_value(tree1, #m{value={cat, Id}}, Context) ->
    get_by_parent(Id, Context);
m_find_value(tree2, #m{value={cat, Id}}, Context) ->
    tree_depth(Id, 2, Context);
m_find_value(image, #m{value={cat, Id}}, Context) ->
    image(Id, Context);
m_find_value(Key, #m{value={cat, Id}}, Context) ->
    proplists:get_value(Key, get(Id, Context));
m_find_value(_Key, _Value, _Context) ->
    undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(#m{value=undefined}, Context) ->
    tree(Context);
m_to_list(#m{value={cat, Id}}, Context) ->
    get(Id, Context);
m_to_list(_, _Context) ->
    [].
    
%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, Context) ->
    tree(Context);
m_value(#m{value=#m{value={cat, Id}}}, Context) ->
    get(Id, Context).

get(Id, Context) ->
    F = fun() ->
        zp_db:assoc_props_row("
                select c.*, r.name
                from category c join rsc r on r.id = c.id
                where c.id = $1", [Id], Context)
    end,
    zp_depcache:memo(F, {category, Id}, ?WEEK, [category]).

get_by_name(Name, Context) ->
    F = fun() ->
        zp_db:assoc_props_row("
                select c.*, r.name 
                from category c join rsc r on r.id = c.id
                where r.name = $1", [Name], Context)
    end,
    zp_depcache:memo(F, {category_by_name, Name}, ?WEEK, [category]).

get_root(Context) ->
    F = fun() ->
        zp_db:assoc_props("
                select c.*, r.name 
                from category c join rsc r on c.id = r.id 
                where c.parent_id is null
                order by c.nr", Context)
    end,
    zp_depcache:memo(F, {category_root}, ?WEEK, [category]).

get_by_parent(Id, Context) ->
    F = fun() ->
        case Id of
            undefined ->
                get_root(Context);
            _ ->
                zp_db:assoc_props("
                    select c.*, r.name 
                    from category c join rsc r on r.id = c.id
                    where c.parent_id = $1 order by c.nr", [Id], Context)
        end
    end,
    zp_depcache:memo(F, {category_parent, Id}, ?WEEK, [category]).

get_range(Id, Context) ->
    F = fun() ->
        case zp_db:q("
                select lft, rght 
                from category 
                where id = $1", [Id], Context) of
            [Row] -> Row;
            _ -> {1, 0} % empty range
        end
    end,
    zp_depcache:memo(F, {category_range, Id}, ?WEEK, [category]).

get_range_by_name(Name, Context) ->
    F = fun() ->
        case zp_db:q("
                select c.lft, c.rght
                from category c join rsc r on r.id = c.id
                where r.name = $1", [Name], Context) of
            [Row] -> Row;
            _ -> {1, 0} % empty range
        end
    end,
    zp_depcache:memo(F, {category_range_name, Name}, ?WEEK, [category]).

get_page_count(Id, Context) ->
    zp_db:q1("select count(*) from rsc where category_id = $1", [Id], Context).
    
name_to_id(Name, _Context) when is_integer(Name) ->
    {ok, Name};
name_to_id(Name, Context) ->
    F = fun() ->
        case zp_db:q1("
                select r.id 
                from rsc r join category c on r.id = c.id 
                where r.name = $1", [Name], Context) of
            undefined -> {error, {enoent, category, Name}};
            Id -> {ok, Id}
        end
    end,
    F().
%    zp_depcache:memo(F, {category_name_to_id, Name}, ?WEEK, [category]).

name_to_id_check(Name, Context) ->
    {ok, Id} = name_to_id(Name, Context),
    Id.

id_to_name(Id, Context) ->
    F = fun() ->
        zp_db:q1("select r.name from rsc r join category c on r.id = c.id where r.id = $1", [Id], Context)
    end,
    zp_depcache:memo(F, {category_id_to_name, Id}, ?WEEK, [category]).


%% @doc Move the category below another category, placing it at the end of the children of that category.
%% @spec move_end(CatId::int(), NewParentId::int(), Context) -> ok | {error, Reason}
move_below(Id, ParentId, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            PathParentId = [ParentId | get_path(ParentId, Context)],
            case lists:member(Id, PathParentId) of
                false ->
                    F = fun(Ctx) ->
                        zp_db:q("update category set parent_id = $1, seq = 10000 where id = $2", [ParentId, Id], Context),
                        renumber(Ctx)
                    end,
                    zp_db:transaction(F, Context),
                    zp_depcache:flush(category);
                true ->
                    {error, cycle}
            end;
        false ->
            {error, eacces}
    end.

%% @doc Move the category to the end of all categories, making it a top category in the process
%% @spec move_end(CatId::int(), Context) -> ok | {error, Reason}
move_end(Id, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            F = fun(Ctx) ->
                zp_db:q("update category set parent_id = null, seq = 10000 where id = $1", [Id], Context),
                renumber(Ctx)
            end,
            zp_db:transaction(F, Context),
            zp_depcache:flush(category);
        false ->
            {error, eacces}
    end.

%% @doc Move a category in front of another category, resetting the parent of the moved category to
%% the parent of the other category.
%% @spec move_before(CatId::int(), BeforeCatId::int(), Context) -> ok | {error, Reason}
move_before(Id, BeforeId, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            F = fun(Ctx) ->
                    {ParentId, Seq} = zp_db:q_row("select parent_id, seq from category where id = $1", [BeforeId], Context),
                    PathParentId = [ParentId | get_path(ParentId, Context)],
                    case lists:member(Id, PathParentId) of
                        false ->
                            case ParentId of
                                undefined ->
                                    zp_db:q("update category set seq = seq+1 where parent_id is null and seq >= $1", [Seq], Context);
                                _ -> 
                                    zp_db:q("update category set seq = seq+1 where parent_id = $2 and seq >= $1", [Seq, ParentId], Context)
                            end,
                            zp_db:q("update category set parent_id = $1, seq = $2 where id = $3", [ParentId, Seq, Id], Context),
                            renumber(Ctx);
                        true -> 
                            {error, cycle}
                    end
            end,

            case zp_db:transaction(F, Context) of
                ok ->
                    zp_depcache:flush(category), 
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, eacces}
    end.


update_sequence(Ids, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            F = fun(Ctx) ->
                zp_db:update_sequence(category, Ids, Ctx),
                renumber(Ctx)
            end,
            zp_db:transaction(F, Context),
            zp_depcache:flush(category);
        false ->
            {error, eacces}
    end.


%% @doc Delete the category, move referring pages to another category. Fails when the transfer id is not a category.
%% @spec delete(Id:int(), TransferId::int(), Context) -> ok | {error, Reason}
delete(Id, TransferId, Context) ->
    % fail when deleting 'other' or 'category'
    case zp_db:q("select name from rsc where id = $1", [Id], Context) of
        N when N == <<"other">>; N == <<"category">> -> {error, is_system_category};
        _ ->
            case zp_acl:has_role(admin, Context) of
                true ->
                    F = fun(Ctx) ->
                        ToId = case TransferId of
                            undefined ->
                                case zp_db:q1("select parent_id from category where id = $1", [Id], Ctx) of
                                    undefined ->
                                        %% The removed category is a top-category, move all content to 'other'
                                        case zp_db:q1("
                                                select c.id 
                                                from rsc r join category c on c.id = r.id
                                                where r.name = 'other'", Context) of
                                            N when is_integer(N) -> N
                                        end;
                                    N ->
                                        N
                                end;
                            N when is_integer(N) ->
                                N = zp_db:q1("select id from category where id = $1", [TransferId], Ctx)
                        end,
                
                        _RscRows = zp_db:q("update rsc set category_id = $1 where category_id = $2", [ToId, Id], Ctx),
                        case Id of
                            undefined ->
                                zp_db:q("update category set parent_id = $1 where parent_id is null", [ToId], Ctx);
                            _ ->
                                zp_db:q("update category set parent_id = $1 where parent_id = $2", [ToId, Id], Ctx)
                        end,
                        ok = m_rsc_update:delete_nocheck(Id, Ctx),
                        ok = renumber(Ctx)
                    end,
                    case zp_db:transaction(F, Context) of
                        ok ->  zp_depcache:flush();
                        {error, Reason} -> {error, Reason}
                    end;
                false ->
                    {error, eacces}
            end
    end.


image(Id, Context) ->
    F = fun() ->
        #search_result{result=Result1} = zp_search:search({media_category_image, [{cat,Id}]}, Context),
        #search_result{result=Result2} = zp_search:search({media_category_depiction, [{cat,Id}]}, Context),
        Result1 ++ Result2
    end,
    Files = zp_depcache:memo(F, {category_image, Id}, ?DAY, [category]),
    case Files of
        [] -> undefined;
        _ -> lists:nth(zp_ids:number(length(Files)), Files)
    end.

    
%% @doc Return the path from a root to the category (excluding the category itself)
%% @spec path(Id, Context) -> [CatId]
get_path(undefined, _Context) ->
    [];
get_path(Id, Context) ->
    Cat = get(Id, Context),
    case proplists:get_value(path, Cat) of
        {ok, Path} -> Path;
        _ -> []
    end.


%% @doc Return a flattened representation of the complete category tree.  Can be used for overviews or select boxes.
%% The "meta" categories of predicate, category and group are suppressed.
all_flat(Context) ->
    F = fun() ->
        zp_db:q("select c.id, c.lvl, r.name, c.props from category c join rsc r on r.id = c.id order by c.nr", Context)
    end,
    All = zp_depcache:memo(F, {category_flat}, ?WEEK, [category]),
    [
        {Id, Lvl, string:copies("&nbsp;&nbsp;&nbsp;&nbsp;", Lvl-1), flat_title(Name, Props)} 
        || {Id, Lvl, Name, Props} <- All, Name /= <<"meta">>, Name /= <<"predicate">>, Name /= <<"category">>, Name /= <<"group">>
    ].
    
    flat_title(Name, Props) ->
        case proplists:get_value(title, Props) of
            undefined -> Name;
            Title -> Title
        end.
    
    

%% @doc Return the tree of all categories
%% @spec tree(Context) -> Tree
tree(Context) ->
    F = fun() ->
        CatTuples = zp_db:q("
                select c.id, c.parent_id, c.lvl, r.name, c.props 
                from category c join rsc r on r.id = c.id  
                order by c.nr", Context),
        build_tree(CatTuples, [])
    end,
    zp_depcache:memo(F, {category_tree}, ?WEEK, [category]).

%% @doc Return the tree of all categories till a certain depth
%% @spec tree_depth(Depth, Context) -> Tree
tree_depth(Depth, Context) ->
    F = fun() ->
        CatTuples = zp_db:q("
                select c.id, c.parent_id, c.lvl, r.name, c.props 
                from category c join rsc r on r.id = c.id 
                where c.lvl <= $1 
                order by c.nr", [Depth], Context),
        build_tree(CatTuples, [])
    end,
    zp_depcache:memo(F, {category_tree_depth, Depth}, ?WEEK, [category]).


%% @doc Return the tree of all categories below a category id
%% @spec tree(CatId, Context) -> TreeNode
tree(CatId, Context) ->
    F = fun() ->
        CatTuples = zp_db:q("
            select a.id, a.parent_id, a.lvl, r.name, a.props 
            from category a join rsc r on a.id = r.id, category p
            where p.id = $1
              and a.nr <= p.rght
              and a.nr >= p.lft
            order by a.nr", [CatId], Context),
        case build_tree(CatTuples, []) of 
            [TreeNode] -> TreeNode;
            [] -> []
        end
    end,
    zp_depcache:memo(F, {category_tree_cat, CatId}, ?WEEK, [category]).

%% @doc Return the tree of all categories below a category id till a certain depth
%% @spec tree_depth(CatId, Depth, Context) -> TreeNode
tree_depth(CatId, Depth, Context) ->
    F = fun() ->
        CatTuples = zp_db:q("
            select a.id, a.parent_id, a.lvl, r.name, a.props 
            from category a join rsc r on a.id = r.id, category p
            where p.id = $1
              and a.nr <= p.rght
              and a.nr >= p.lft
              and a.lvl <= p.lvl + $2
            order by a.nr", [CatId, Depth], Context),
        case build_tree(CatTuples, []) of 
            [TreeNode] -> TreeNode;
            [] -> []
        end
    end,
    zp_depcache:memo(F, {category_tree_cat_depth, CatId, Depth}, ?WEEK, [category]).


build_tree([], Acc) ->
    lists:reverse(Acc);
build_tree([{_Id, _Parent, _Lvl, _Name, _Props} = C|Rest], Acc) ->
    {C1, Rest1} = build_tree(C, [], Rest),
    build_tree(Rest1, [C1|Acc]).
    
build_tree({Id, _Parent, _Lvl, _Name, _Props} = P, Acc, [{_Id2, Parent2, _Lvl2, _Name2, _Props2} = C|Rest])
    when Id == Parent2 ->
    {C1, Rest1} = build_tree(C, [], Rest),
    build_tree(P, [C1|Acc], Rest1);
build_tree({Id, Parent, Lvl, Name, Props}, Acc, Rest) ->
    Props1 = case Props of
        <<>> -> [];
        _ -> Props
    end,
    {[{id,Id}, {parent,Parent}, {level,Lvl}, {children, {ok, lists:reverse(Acc)}}, {name, Name} | Props1], Rest}.



%% @doc Renumber all categories so that the left/right and level indices are correct.
%% @spec renumber(Context) -> ok
renumber(Context) ->
    ok = zp_db:transaction(fun renumber_transaction/1, Context),
    zp_depcache:flush(category),
    ok.

renumber_transaction(Context) ->
    CatTuples = zp_db:q("select id, parent_id, seq from category order by seq,id", Context),
    Enums = enumerate(CatTuples),
    [
        zp_db:update(category, CatId, [
            {nr, Nr},
            {seq, Nr},
            {lvl, Level},
            {lft, Left},
            {rght, Right},
            {path, {ok, Path}}
        ], Context)
        || {CatId, Nr, Level, Left, Right, Path} <- Enums
    ],
    ok.



%% @doc Take a category list and make it into a tree, recalculating the left/right and lvl nrs
%% @spec cat_enumerate(Cats) -> Sorts
%%  Cats = [Cat]
%%  Cat = {CatId, Parent, NodeSeq} 
%%  Sorts = [Sort]
%%  Sort = {CatId, Nr, Level, Left, Right, Path}
enumerate(Cats) ->
    % Fetch all the roots of our forest
    {Roots, Rest} = lists:partition(fun({_Id, Parent, _Seq}) -> Parent == undefined end, Cats),
    % Make the trees from the roots down
    Trees = [ make_tree(Root, Rest, 1, []) || Root <- Roots],
    % Flatten the trees, enumerating all nodes depth-first
    {Flatten,_Nr} = lists:foldl(fun(Tree, {Acc,Nr}) -> flatten_tree(Tree, Acc, Nr) end, {[],1}, Trees),
    Flatten.

make_tree({NodeId,_Parent,NodeSeq} = Node, Nodes, Level, Path) ->
    SubNodes = lists:filter(fun ({_,Parent,_}) -> Parent == NodeId end, Nodes),
    SubTrees = [ make_tree(SubNode, Nodes, Level+1, [NodeId|Path]) || SubNode <- SubNodes ],
    {Level, NodeSeq, Node, lists:keysort(2, SubTrees), lists:reverse(Path)}.
    
flatten_tree({Level, _NodeSeq, {NodeId,_Parent,_Seq}, SubTrees, Path}, NodesAcc, NodeNr) ->
    {NodesAcc1, NodeNr1} = lists:foldl(fun(Tree, {Acc,Nr}) -> flatten_tree(Tree, Acc, Nr) end, {NodesAcc,NodeNr+1}, SubTrees),
    {[ {NodeId, NodeNr, Level, NodeNr, NodeNr1-1, Path} | NodesAcc1], NodeNr1}.

