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
        "mod_base",
        "mod_search",
        "mod_admin",
        "mod_admin_modules",
        "mod_admin_users",
        "mod_emailer",
        "mod_shop"
    ],
    [
        {ok, 1} = pgsql:equery(C, "insert into module (name, is_active) values ($1, true)", [M]) || M <- Modules
    ],
    ok.


%% @doc Install the default admin, editor, supervisor, community and public groups
%% The resources will be inserted later, which is possible because the fk check is deferred till commit time.
install_group(C) ->
    Groups = [
        %   rsc admin  spvsr  cpub   ppub
        [4,  true,  true,  true,  true  ],
        [5,  false, false, true,  true  ],
        [6,  false, false, true,  false ],
        [7,  false, true,  false, false ],
        [8,  false, false, false, false ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into \"group\" (id, is_admin, is_supervisor, is_community_publisher, is_public_publisher) 
            values ($1, $2, $3, $4, $5)", R) || R <- Groups],
    ok.

install_category(C) ->
    Cats = [
        {1, undefined, 1, other,       [{title, {trans, [{en, "Uncategorized"},  {nl, "Zonder categorie"}]}}] },
        {2, undefined, 1, person,      [{title, {trans, [{en, "Person"},         {nl, "Persoon"}]}}] },
        % http://purl.org/dc/dcmitype/PhysicalObject
        {3, undefined, 1, artifact,    [{title, {trans, [{en, "Artifact"},       {nl, "Artefact"}]}}] },
        % http://purl.org/dc/dcmitype/Text
        {4, undefined, 1, text,        [{title, {trans, [{en, "Text"},           {nl, "Tekst"}]}}] },
        {5, 4,         2, review,      [{title, {trans, [{en, "Review"},         {nl, "Beoordeling"}]}}] },
        {6, 4,         1, article,     [{title, {trans, [{en, "Article"},        {nl, "Artikel"}]}}] },

        {7, 3,         3, product,     [{title, {trans, [{en, "Product"},        {nl, "Product"}]}}] },

        % http://purl.org/dc/dcmitype/Event
        {8, undefined, 1, event,       [{title, {trans, [{en, "Event"},          {nl, "Evenement"}]}}] },

        {9, 6,         1, news,        [{title, {trans, [{en, "News"},           {nl, "Nieuws"}]}}] },

        {10,undefined, 1, media,       [{title, {trans, [{en, "Media"},          {nl, "Media"}]}}] }, 
        % http://purl.org/dc/dcmitype/Image
        % http://purl.org/dc/dcmitype/StillImage
        {11,10,        1, image,       [{title, {trans, [{en, "Image"},          {nl, "Media"}]}}] },
        % http://purl.org/dc/dcmitype/MovingImage
        {12,10,        2, video,       [{title, {trans, [{en, "Video"},          {nl, "Video"}]}}] },
        % http://purl.org/dc/dcmitype/Sound
        {13,10,        3, sound,       [{title, {trans, [{en, "Sound"},          {nl, "Sound"}]}}] },

        % http://purl.org/dc/dcmitype/Collection
        {14,undefined, 1, collection,  [{title, {trans, [{en, "Collection"},     {nl, "Collectie"}]}}] },

        % Meta categories for defining categories, predicates and groups.
        {15,undefined, 1, meta,        [{title, {trans, [{en, "Meta"},          {nl, "Meta"}]}}] },
        {16,15,        1, category,    [{title, {trans, [{en, "Category"},      {nl, "Categorie"}]}}] },
        {17,15,        1, predicate,   [{title, {trans, [{en, "Predicate"},     {nl, "Predikaat"}]}}] },
        {18,15,        1, usergroup,   [{title, {trans, [{en, "User Group"},    {nl, "Gebruikersgroep"}]}}] }
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
        % id  vsfr  grp  cat  name,     props
        [ 4,  0,    4,   18,  "admins", [{title,"Admins"}] ],
        [ 5,  0,    5,   18,  "editors", [{title,"Site Administrators"}] ],
        [ 6,  0,    6,   18,  "communityeditors", [{title,"Community Editors"}] ],
        [ 7,  0,    7,   18,  "supervisors", [{title,"Supervisors"}] ],
        [ 8,  0,    8,   18,  "content", [{title,"Content"}] ],

        [ 1,  0,    4,   2,   "admin",  [{title,"Site Administrator"}] ],
        [ 2,  0,    4,   6,   "about",  [{title,"About Zophrenic"}, {body, "<p>Some nice text in the body.</p>"}] ],
        [ 3,  0,    4,   9,   undefined,[{title,"Some News"}, {body, "<p>And the text of the news should be typed here.</p>"}] ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into rsc (id, visible_for, group_id, category_id, name, props)
            values ($1, $2, $3, $4, $5, $6)
            ", R) || R <- Rsc ],
    {ok, _} = pgsql:squery(C, "update rsc set creator_id = 1, modifier_id = 1, is_published = true"),
    pgsql:reset_id(C, "rsc"),
    
    % Connect person resources to the correct groups
    RscGroup = [
        % Id, Rsc  Grp  obsvr   leader
        [ 1,  1,   4,   false,  true ]
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
        [ 1, "about",   "http://www.w3.org/1999/02/22-rdf-syntax-ns#about",  false, [{title, {trans, [{en,"About"},    {nl,"Over"}]}}]],
        [ 2, "author",  "http://purl.org/dc/elements/1.1/creator",           true,  [{title, {trans, [{en,"Author"},   {nl,"Auteur"}]}}]],
        [ 3, "review",  "http://purl.org/stuff/rev#Review",                  true,  [{title, {trans, [{en,"Reviews"},  {nl,"Beoordeelt"}]}}]],
        [ 4, "relation","http://purl.org/dc/elements/1.1/relation",          false, [{title, {trans, [{en,"Relation"}, {nl,"Relatie"}]}}]],
        [ 5, "media",   "http://zophrenic.com/predicate/media",              false, [{title, {trans, [{en,"Media"},    {nl,"Media"}]}}]],
        [ 6, "depiction","http://xmlns.com/foaf/0.1/depiction",              false, [{title, {trans, [{en,"Depiction"},{nl,"Afbeelding"}]}}]]
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
