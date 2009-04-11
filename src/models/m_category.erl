%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-08
%%
%% @doc Model for categories.  Add, change and re-order categories.

-module(m_category).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    get/2,
    get_by_name/2,
    get_by_parent/2,
    get_range/2,
    get_range_by_name/2,
    path/2,
    name_to_id/2,
    update_parent/3,
    update_sequence/2,
    tree/1,
    tree/2,
    renumber/1,
    enumerate/1
]).


get(Id, Context) ->
    zp_db:assoc_props_row("select * from category where id = $1", [Id], Context).

get_by_name(Name, Context) ->
    zp_db:assoc_props_row("select * from category where name = $1", [Name], Context).

get_by_parent(Id, Context) ->
    zp_db:assoc_props("select * from category where parent_id = $1 order by seq", [Id], Context).

get_range(Id, Context) ->
    case zp_db:q("select lft, rght from category where id = $1", [Id], Context) of
        [Row] -> Row;
        _ -> {1, 0} % empty range
    end.

get_range_by_name(Name, Context) ->
    case zp_db:q("select lft, rght from category where name = $1", [Name], Context) of
        [Row] -> Row;
        _ -> {1, 0} % empty range
    end.

name_to_id(Name, Context) ->
    zp_db:q1("select id from category where name = $1", [Name], Context).


update_parent(Id, ParentId, Context) ->
    F = fun(Ctx) ->
        zp_db:q("update category set parent_id = $1 where id = $2", [ParentId, Id], Context),
        renumber(Ctx)
    end,
    zp_db:transaction(F, Context).


update_sequence(Ids, Context) ->
    F = fun(Ctx) ->
        zp_db:update_sequence(category, Ids, Ctx),
        renumber(Ctx)
    end,
    zp_db:transaction(F, Context).


%% @doc Return the path from a root to the category
%% @spec path(Id, Context) -> [CatId]
path(Id, Context) ->
    Cat = get(Id, Context),
    proplists:get_value(path, Cat).
        

%% @doc Return the tree of all categories
%% @spec tree(Context) -> Tree
tree(Context) ->
    CatTuples = zp_db:q("select id, parent_id, lvl, props from category order by nr", Context),
    build_tree(CatTuples, []).

%% @doc Return the tree of all categories below a category id
%% @spec tree(Context, CatId) -> TreeNode
tree(Context, CatId) ->
    CatTuples = zp_db:q("
        select a.id, a.parent_id, a.lvl, a.props 
        from category a, category r
        where r.id = $1
          and a.nr <= r.rght
          and a.nr >= r.lft
        order by a.nr", [CatId], Context),
    case build_tree(CatTuples, []) of 
        [TreeNode] -> TreeNode;
        [] -> []
    end.


build_tree([], Acc) ->
    lists:reverse(Acc);
build_tree([{_Id, _Parent, _Lvl, _Props} = C|Rest], Acc) ->
    {C1, Rest1} = build_tree(C, [], Rest),
    build_tree(Rest1, [C1|Acc]).
    
build_tree({Id, _Parent, _Lvl, _Props} = P, Acc, [{_Id2, Parent2, _Lvl2, _Props2} = C|Rest])
    when Id == Parent2 ->
    {C1, Rest1} = build_tree(C, [], Rest),
    build_tree(P, [C1|Acc], Rest1);
build_tree({Id, Parent, Lvl, Props}, Acc, Rest) ->
    {[{id,Id}, {parent,Parent}, {level,Lvl}, {p,Props}, {children,lists:reverse(Acc)}], Rest}.



%% @doc Renumber all categories so that the left/right and level indices are correct.
%% @spec renumber(Context) -> ok
renumber(Context) ->
    ok = zp_db:transaction(fun renumber_transaction/1, Context),
    ok.

renumber_transaction(Context) ->
    CatTuples = zp_db:q("select id, parent_id, seq from category", Context),
    Enums = enumerate(CatTuples),
     [
        zp_db:update(category, CatId, [
            {nr, Nr},
            {lvl, Level},
            {lft, Left},
            {rght, Right},
            {path, Path}
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
    {Level, NodeSeq, Node, lists:keysort(2, SubTrees), Path}.
    
flatten_tree({Level, _NodeSeq, {NodeId,_Parent,_Seq}, SubTrees, Path}, NodesAcc, NodeNr) ->
    {NodesAcc1, NodeNr1} = lists:foldl(fun(Tree, {Acc,Nr}) -> flatten_tree(Tree, Acc, Nr) end, {NodesAcc,NodeNr+1}, SubTrees),
    {[ {NodeId, NodeNr, Level, NodeNr, NodeNr1-1, Path} | NodesAcc1], NodeNr1}.

