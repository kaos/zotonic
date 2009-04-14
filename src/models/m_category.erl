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
    insert/2,
    name_to_id/2,
    name_to_id_check/2,
    id_to_name/2,
    update_parent/3,
    update_sequence/2,
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
    tree_depth(Context, 2);

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
        zp_db:assoc_props_row("select * from category where id = $1", [Id], Context)
    end,
    zp_depcache:memo(F, {category, Id}, ?WEEK, [category]).

get_by_name(Name, Context) ->
    F = fun() ->
        zp_db:assoc_props_row("select * from category where name = $1", [Name], Context)
    end,
    zp_depcache:memo(F, {category_by_name, Name}, ?WEEK, [category]).

get_root(Context) ->
    F = fun() ->
        zp_db:assoc_props("select * from category where parent_id is null order by nr", Context)
    end,
    zp_depcache:memo(F, {category_root}, ?WEEK, [category]).

get_by_parent(Id, Context) ->
    F = fun() ->
        zp_db:assoc_props("select * from category where parent_id = $1 order by nr", [Id], Context)
    end,
    zp_depcache:memo(F, {category_parent, Id}, ?WEEK, [category]).

get_range(Id, Context) ->
    F = fun() ->
        case zp_db:q("select lft, rght from category where id = $1", [Id], Context) of
            [Row] -> Row;
            _ -> {1, 0} % empty range
        end
    end,
    zp_depcache:memo(F, {category_range, Id}, ?WEEK, [category]).

get_range_by_name(Name, Context) ->
    F = fun() ->
        case zp_db:q("select lft, rght from category where name = $1", [Name], Context) of
            [Row] -> Row;
            _ -> {1, 0} % empty range
        end
    end,
    zp_depcache:memo(F, {category_range_name, Name}, ?WEEK, [category]).

name_to_id(Name, _Context) when is_integer(Name) ->
    {ok, Name};
name_to_id(Name, Context) ->
    F = fun() ->
        case zp_db:q1("select id from category where name = $1", [Name], Context) of
            undefined -> {error, {enoent, category, Name}};
            Id -> {ok, Id}
        end
    end,
    zp_depcache:memo(F, {category_name_to_id, Name}, ?WEEK, [category]).

name_to_id_check(Name, Context) ->
    {ok, Id} = name_to_id(Name, Context),
    Id.

id_to_name(Id, Context) ->
    F = fun() ->
        zp_db:q1("select name from category where id = $1", [Id], Context)
    end,
    zp_depcache:memo(F, {category_id_to_name, Id}, ?WEEK, [category]).


update_parent(Id, ParentId, Context) ->
    F = fun(Ctx) ->
        zp_db:q("update category set parent_id = $1 where id = $2", [ParentId, Id], Context),
        renumber(Ctx)
    end,
    zp_db:transaction(F, Context),
    zp_depcache:flush(category).


update_sequence(Ids, Context) ->
    F = fun(Ctx) ->
        zp_db:update_sequence(category, Ids, Ctx),
        renumber(Ctx)
    end,
    zp_db:transaction(F, Context),
    zp_depcache:flush(category).


insert(Props, Context) ->
    {ok, Id} = zp_db:insert(category, Props, Context),
    zp_depcache:flush(category),
    {ok, Id}.


image(Id, Context) ->
    F = fun() ->
        search:search_media({category_image, Id}, Context)
    end,
    Files = zp_depcache:memo(F, {category_image, Id}, ?DAY, [category]),
    case Files of
        [] -> undefined;
        _ -> lists:nth(zp_ids:number(length(Files)), Files)
    end.

    
%% @doc Return the path from a root to the category
%% @spec path(Id, Context) -> [CatId]
get_path(Id, Context) ->
    Cat = get(Id, Context),
    proplists:get_value(path, Cat).
        

%% @doc Return the tree of all categories
%% @spec tree(Context) -> Tree
tree(Context) ->
    F = fun() ->
        CatTuples = zp_db:q("select id, parent_id, lvl, name, props from category order by nr", Context),
        build_tree(CatTuples, [])
    end,
    zp_depcache:memo(F, {category_tree}, ?WEEK, [category]).

%% @doc Return the tree of all categories till a certain depth
%% @spec tree_depth(Depth, Context) -> Tree
tree_depth(Depth, Context) ->
    F = fun() ->
        CatTuples = zp_db:q("select id, parent_id, lvl, name, props from category where lvl <= $1 order by nr", [Depth], Context),
        build_tree(CatTuples, [])
    end,
    zp_depcache:memo(F, {category_tree_depth, Depth}, ?WEEK, [category]).


%% @doc Return the tree of all categories below a category id
%% @spec tree(CatId, Context) -> TreeNode
tree(CatId, Context) ->
    F = fun() ->
        CatTuples = zp_db:q("
            select a.id, a.parent_id, a.lvl, a.name, a.props 
            from category a, category r
            where r.id = $1
              and a.nr <= r.rght
              and a.nr >= r.lft
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
            select a.id, a.parent_id, a.lvl, a.name, a.props 
            from category a, category r
            where r.id = $1
              and a.nr <= r.rght
              and a.nr >= r.lft
              and a.lvl <= r.lvl + $2
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
    {[{id,Id}, {parent,Parent}, {level,Lvl}, {children,lists:reverse(Acc)}, {name, Name} | Props1], Rest}.



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
    {Level, NodeSeq, Node, lists:keysort(2, SubTrees), lists:reverse(Path)}.
    
flatten_tree({Level, _NodeSeq, {NodeId,_Parent,_Seq}, SubTrees, Path}, NodesAcc, NodeNr) ->
    {NodesAcc1, NodeNr1} = lists:foldl(fun(Tree, {Acc,Nr}) -> flatten_tree(Tree, Acc, Nr) end, {NodesAcc,NodeNr+1}, SubTrees),
    {[ {NodeId, NodeNr, Level, NodeNr, NodeNr1-1, Path} | NodesAcc1], NodeNr1}.

