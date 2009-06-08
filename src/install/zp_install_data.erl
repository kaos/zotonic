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
    install_category/1
]).

-include_lib("zophrenic.hrl").

%% @doc Insert boot data into the database.
%% @spec install(Connection) -> ok
install(C) ->
    ok = install_config(C),
    ok = install_modules(C),
    ok = install_group(C),
    ok = install_category(C),
    ok = install_rsc(C),
    ok = install_identity(C),
    ok = install_predicate(C),
    ok = install_edge(C),
    ok.


%% @doc Install all configuration parameters with default values
%% @spec install_config(Connection) -> ok
install_config(C) ->
    {ok, 1} = pgsql:equery(C, 
        "insert into config (module, key, value, props, modified) values ($1, $2, $3, $4, now())", 
        ["zophrenic", "version", ?ZOPHRENIC_VERSION, []]),
    {ok, 1} = pgsql:equery(C, 
        "insert into config (module, key, value, props, modified) values ($1, $2, $3, $4, now())", 
        ["i18n", "language", "nl", []]),
    pgsql:reset_id(C, "config"),
    ok.


install_modules(C) ->
    Modules = [
        "mod_test",
        "mod_module_indexer",
        "mod_module_admin"
        "mod_base"
    ],
    [
        {ok, 1} = pgsql:equery(C, "insert into modules (name, is_active) values ($1, true)", [M]) || M <- Modules
    ],
    ok.


%% @doc Install the default admin, editor, supervisor, community and public groups
install_group(C) ->
    % Install the group types
    GroupTypes = [
        [1, "general",    [{title, {trans, [{en, "Generic"},       {nl, "Generiek"}]}}]],
        [2, "workgroup",  [{title, {trans, [{en, "Working Group"}, {nl, "Werkgroep"}]}}]],
        [3, "family",     [{title, {trans, [{en, "Family"},        {nl, "Familie"}]}}]]
    ],

    [ {ok,1} = pgsql:equery(C, "
            insert into grouptype (id, name, props) 
            values ($1, $2, $3)", R) || R <- GroupTypes],
    pgsql:reset_id(C, "grouptype"),

    Groups = [
        %   tp name                admin  spvsr  cpub   ppub   props
        [1, 2, "admins",           true,  true,  true,  true,  [{title, {trans, [{en, "Administrators"},   {nl, "Beheerders"}]}}]],
        [2, 2, "editors",          false, false, true,  true,  [{title, {trans, [{en, "Editors"},          {nl, "Redacteuren"}]}}]],
        [3, 2, "communityeditors", false, false, true,  false, [{title, {trans, [{en, "Community Editors"},{nl, "Gemeenschap redacteuren"}]}}]],
        [4, 2, "supervisors",      false, true,  false, false, [{title, {trans, [{en, "Supervisors"},      {nl, "Toezichthouders"}]}}]],
        [5, 2, "content",          false, false, false, false, [{title, {trans, [{en, "Content"},          {nl, "Inhoud"}]}}]]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into \"group\" (id, grouptype_id, name, is_admin, is_supervisor, is_community_publisher, is_public_publisher, props) 
            values ($1, $2, $3, $4, $5, $6, $7, $8)", R) || R <- Groups],
    
    pgsql:reset_id(C, "group"),
    ok.

install_category(C) ->
    Cats = [
        {1, undefined, 1, other,       [{title, {trans, [{en, "Uncategorized"},  {nl, "Zonder categorie"}]}}] },
        {2, undefined, 1, person,      [{title, {trans, [{en, "Person"},         {nl, "Persoon"}]}}] },
        {3, undefined, 1, artifact,    [{title, {trans, [{en, "Artifact"},       {nl, "Artefact"}]}}] },
        {4, 3,         1, publication, [{title, {trans, [{en, "Publication"},    {nl, "Publicatie"}]}}] },
        {5, 4,         2, review,      [{title, {trans, [{en, "Review"},         {nl, "Beoordeling"}]}}] },
        {6, 4,         1, article,     [{title, {trans, [{en, "Article"},        {nl, "Artikel"}]}}] },
        {7, 3,         3, product,     [{title, {trans, [{en, "Product"},        {nl, "Product"}]}}] },
        {8, 3,         2, event,       [{title, {trans, [{en, "Event"},          {nl, "Evenement"}]}}] },
        {9, 6,         1, news,        [{title, {trans, [{en, "News"},           {nl, "Nieuws"}]}}] }
    ],
    [ {ok,1} = pgsql:equery(C, "
            insert into category (id, parent_id, seq, name, props)
            values ($1, $2, $3, $4, $5)", R) || R <- Cats],
    pgsql:reset_id(C, "category"),
    ok = enumerate_categories(C),
    ok.
    

%% @doc Install some initial resources, most important is the system administrator
%% @todo Add the hostname to the uri
install_rsc(C) ->
    Rsc = [
        % id  uri       vsfr  grp  cat  name,     props
        [ 1,  "/id/1",  0,    1,   2,   "admin",  [{title,"Site Administrator"}] ],
        [ 2,  "/id/2",  0,    1,   6,   "about",  [{title,"About Zophrenic"}, {body, "<p>Some nice text in the body.</p>"}] ],
        [ 3,  "/id/3",  0,    1,   9,   undefined,[{title,"Some News"}, {body, "<p>And the text of the news should be typed here.</p>"}] ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into rsc (id, uri, visible_for, group_id, category_id, name, props)
            values ($1, $2, $3, $4, $5, $6, $7)
            ", R) || R <- Rsc ],
    {ok, _} = pgsql:squery(C, "update rsc set creator_id = 1, modifier_id = 1, is_published = true"),
    pgsql:reset_id(C, "rsc"),
    
    % Connect person resources to the correct groups
    RscGroup = [
        % Id, Rsc  Grp  obsvr   leader
        [ 1,  1,   1,   false,  true ]
    ],

    [ {ok,1} = pgsql:equery(C, "
            insert into rsc_group (id, rsc_id, group_id, is_observer, is_leader)
            values ($1, $2, $3, $4, $5)
            ", R) || R <- RscGroup ],
    pgsql:reset_id(C, "rsc_group"),
    ok.


%% @doc Install the admin user as an user.  For now the hard coded password "admin"
install_identity(C) ->
    Hash = m_identity:hash("admin"),
    {ok, 1} = pgsql:equery(C, "
        insert into identity (rsc_id, type, key, is_unique, propb)
        values (1, 'username_pw', 'admin', true, $1)", [Hash]),
    ok.
    

%% @doc Install some initial predicates, this list should be extended with common and useful predicates
%% @todo Extend and check this list.  Add allowed from/to categories.
install_predicate(C) ->
    Preds = [
        % id  name      uri                                                  rvrsd  props
        [ 1, "about",   "http://www.w3.org/1999/02/22-rdf-syntax-ns#about",  false, [{title, {trans, [{en,"About"},   {nl,"Over"}]}}]],
        [ 2, "author",  "http://purl.org/dc/elements/1.1/creator",           true,  [{title, {trans, [{en,"Author"},  {nl,"Auteur"}]}}]],
        [ 3, "review",  "http://purl.org/stuff/rev#Review",                  true,  [{title, {trans, [{en,"Reviews"}, {nl,"Beoordeelt"}]}}]]
    ],

    [ {ok,1} = pgsql:equery(C, "
            insert into predicate (id, name, uri, reversed, props)
            values ($1, $2, $3, $4, $5)
            ", R) || R <- Preds],
    pgsql:reset_id(C, "predicate"),
    ok.



%% @doc Install example edges between the predefined content
install_edge(C) ->
    Edges = [
        %  subj  obj  pred  seq
        [  2,    1,   2,    1  ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into edge (subject_id, object_id, predicate_id, seq)
            values ($1, $2, $3, $4)
            ", R) || R <- Edges],
    pgsql:reset_id(C, "edge"),
    ok.



%% @doc Enumerate all categories so that their left, right, level en nr are set correctly
%% @type enumerate_categories(Connection) -> ok
enumerate_categories(C) ->
    {ok, _, CatTuples} = pgsql:equery(C, "select id, parent_id, seq from category"),
    Enums = m_category:enumerate(CatTuples),
    [
        {ok, _} = pgsql:equery(C, "update category set nr = $2, lvl = $3, lft = $4, rght = $5, props = $6 where id = $1", [CatId, Nr, Level, Left, Right, [{path,Path}]])
        || {CatId, Nr, Level, Left, Right, Path} <- Enums
    ],
    ok.
