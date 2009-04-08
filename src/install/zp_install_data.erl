%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-07
%%
%% @doc Initialize the database with start data.

-module(zp_install_data).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    install/1,
    enumerate_categories/1,
    install_category/1
]).

-include_lib("zophrenic.hrl").

%% @doc Insert boot data into the database.
%% @spec install(Connection) -> ok
install(C) ->
    ok = install_config(C),
    ok = install_group(C),
    ok = install_category(C),
    ok = install_rsc(C),
    ok = install_predicate(C),
    ok.


%% @doc Install all configuration parameters with default values
%% @spec install_config(Connection) -> ok
install_config(C) ->
    {ok, 1} = pgsql:equery(C, 
        "insert into config (module, key, value, props, modified) values ($1, $2, $3, $4, now())", 
        ["zophrenic", "version", ?ZOPHRENIC_VERSION, []]),
    ok.
    

%% @doc Install the default admin, editor, supervisor, community and public groups
install_group(C) ->
    % Install the group types
    GroupTypes = [
        [1, "GENERAL",    [{title, {trans, [{en, "Generic"},       {nl, "Generiek"}]}}]],
        [2, "WORKGROUP",  [{title, {trans, [{en, "Working Group"}, {nl, "Werkgroep"}]}}]],
        [3, "FAMILY",     [{title, {trans, [{en, "Family"},        {nl, "Familie"}]}}]]
    ],

    [ {ok,1} = pgsql:equery(C, "
            insert into grouptype (id, name, props) 
            values ($1, $2, $3)", R) || R <- GroupTypes],

    Groups = [
        %tp name                admin  edit   spvsr  cpub   ppub   props
        [1, 2, "ADMINS",           true,  true,  true,  true,  true,  [{title, {trans, [{en, "Administrators"},   {nl, "Beheerders"}]}}]],
        [2, 2, "EDITORS",          false, true,  false, true,  true,  [{title, {trans, [{en, "Editors"},          {nl, "Redacteurs"}]}}]],
        [3, 2, "COMMUNITYEDITORS", false, true,  false, true,  false, [{title, {trans, [{en, "Community Editors"},{nl, "Gemeenschap Redacteurs"}]}}]],
        [4, 2, "SUPERVISORS",      false, false, true,  false, false, [{title, {trans, [{en, "Supervisors"},      {nl, "Toezichthouders"}]}}]]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into \"group\" (id, grouptype_id, name, is_admin, is_editor, is_supervisor, is_community_publisher, is_public_publisher, props) 
            values ($1, $2, $3, $4, $5, $6, $7, $8, $9)", R) || R <- Groups],
    
    ok.

install_category(C) ->
    Cats = [
        {1, undefined, 1, [{title, {trans, [{en, "Uncategorized"},  {nl, "Geen categorie"}]}}] },
        {2, undefined, 1, [{title, {trans, [{en, "Person"},         {nl, "Persoon"}]}}] },
        {3, undefined, 1, [{title, {trans, [{en, "Artifact"},       {nl, "Artefact"}]}}] },
        {4, 3,         1, [{title, {trans, [{en, "Publication"},    {nl, "Publicatie"}]}}] },
        {5, 4,         2, [{title, {trans, [{en, "Review"},         {nl, "Beoordeling"}]}}] },
        {6, 4,         1, [{title, {trans, [{en, "Article"},        {nl, "Artikel"}]}}] },
        {7, 3,         1, [{title, {trans, [{en, "Product"},        {nl, "Product"}]}}] }
    ],
    [ {ok,1} = pgsql:equery(C, "
            insert into category (id, parent_id, seq, props)
            values ($1, $2, $3, $4)", R) || R <- Cats],
    ok = enumerate_categories(C),
    ok.
    

%% @doc Install some initial resources, most important is the system administrator
%% @todo Add the hostname to the uri
install_rsc(C) ->
    Rsc = [
        % id  uri         vsfr  grp  cat  name,     props
        [ 1,  "person/1", 0,    1,   2,   "ADMIN",  [{title,"Site Administrator"}] ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into rsc (id, uri, visible_for, group_id, category_id, unique_name, props)
            values ($1, $2, $3, $4, $5, $6, $7)
            ", R) || R <- Rsc ],
    {ok, _} = pgsql:squery(C, "update rsc set owner_id = 1, creator_id = 1, modifier_id = 1, is_published = true"),
    ok.


%% @doc Install some initial predicates, this list should be extended with common and useful predicates
%% @todo Extend and check this list.  Add allowed from/to categories.
install_predicate(C) ->
    Preds = [
        % id  name      uri                                                  rvrsd  props
        [ 1, "ABOUT",   "http://www.w3.org/1999/02/22-rdf-syntax-ns#about",  false, [{title, {trans, [{en,"About"},   {nl,"Over"}]}}]],
        [ 2, "AUTHOR",  "http://purl.org/dc/elements/1.1/creator",           true,  [{title, {trans, [{en,"Author"},  {nl,"Auteur"}]}}]],
        [ 3, "REVIEW",  "http://purl.org/stuff/rev#Review",                  true,  [{title, {trans, [{en,"Reviews"}, {nl,"Beoordeelt"}]}}]]
    ],

    [ {ok,1} = pgsql:equery(C, "
            insert into predicate (id, name, uri, reversed, props)
            values ($1, $2, $3, $4, $5)
            ", R) || R <- Preds],
    ok.





%% @doc Enumerate all categories so that their left, right, level en nr are set correctly
%% @type enumerate_categories(Connection) -> ok
enumerate_categories(C) ->
    {ok, _, CatTuples} = pgsql:equery(C, "select id, parent_id, seq from category"),
    Enums = cat_enumerate(CatTuples),
    % {CatId, Nr, Level, Left, Right}
    [
        {ok, _} = pgsql:equery(C, "update category set nr = $2, lvl = $3, lft = $4, rght = $5 where id = $1", Enum)
        || Enum <- Enums
    ],
    ok.

%% @doc Take a category list and make it into a tree, calculating the left/right and lvl nrs
%% @spec cat_enumerate(Cats) -> Sorts
%%  Cats = [Cat]
%%  Cat = {CatId, Parent, NodeSeq} 
%%  Sorts = [Sort]
%%  Sort = {CatId, Nr, Level, Left, Right}
cat_enumerate(Cats) ->
    % Fetch all the roots of our forest
    {Roots, Rest} = lists:partition(fun({_Id, Parent, _Seq}) -> Parent == undefined end, Cats),
    % Make the trees from the roots down
    Trees = [ make_tree(Root, Rest, 1) || Root <- Roots],
    % Flatten the trees, enumerating all nodes depth-first
    {Flatten,_Nr} = lists:foldl(fun(Tree, {Acc,Nr}) -> flatten_tree(Tree, Acc, Nr) end, {[],1}, Trees),
    Flatten.

make_tree({NodeId,_Parent,NodeSeq} = Node, Nodes, Level) ->
    SubNodes = lists:filter(fun ({_,Parent,_}) -> Parent == NodeId end, Nodes),
    SubTrees = [ make_tree(SubNode, Nodes, Level+1) || SubNode <- SubNodes ],
    {Level, NodeSeq, Node, lists:keysort(2, SubTrees)}.
    
flatten_tree({Level, _NodeSeq, {NodeId,_Parent,_Seq}, SubTrees}, NodesAcc, NodeNr) ->
    {NodesAcc1, NodeNr1} = lists:foldl(fun(Tree, {Acc,Nr}) -> flatten_tree(Tree, Acc, Nr) end, {NodesAcc,NodeNr+1}, SubTrees),
    {[ {NodeId, NodeNr, Level, NodeNr, NodeNr1-1} | NodesAcc1], NodeNr1}.

