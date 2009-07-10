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
    ?DEBUG("Inserting config keys"),
    {ok, 1} = pgsql:equery(C, 
        "insert into config (module, key, value, props, modified) values ($1, $2, $3, $4, now())", 
        ["zophrenic", "version", ?ZOPHRENIC_VERSION, []]),
    {ok, 1} = pgsql:equery(C, 
        "insert into config (module, key, value, props, modified) values ($1, $2, $3, $4, now())", 
        ["i18n", "language", "nl", []]),
    pgsql:reset_id(C, "config"),
    ok.


install_modules(C) ->
    ?DEBUG("Inserting modules"),
    Modules = [
        "mod_test",
        "mod_base",
        "mod_search",
        "mod_admin",
        "mod_admin_modules",
        "mod_admin_identity",
        "mod_admin_category",
        "mod_admin_predicate",
        "mod_admin_group",
        "mod_emailer",

        % The example site
        "mod_site_simple"
    ],
    [
        {ok, 1} = pgsql:equery(C, "insert into module (name, is_active) values ($1, true)", [M]) || M <- Modules
    ],
    ok.


%% @doc Install the default admin, editor, supervisor, community and public groups
%% The resources will be inserted later, which is possible because the fk check is deferred till commit time.
install_group(C) ->
    ?DEBUG("Inserting groups"),
    Groups = [
        %   rsc admin  spvsr  cpub   ppub
        [204,  true,  true,  true,  true  ],
        [205,  false, false, true,  true  ],
        [206,  false, false, true,  false ],
        [207,  false, true,  false, false ],
        [208,  false, false, false, false ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into \"group\" (id, is_admin, is_supervisor, is_community_publisher, is_public_publisher) 
            values ($1, $2, $3, $4, $5)", R) || R <- Groups],
    ok.

install_category(C) ->
    ?DEBUG("Inserting categories"),
    %% The egg has to lay a fk-checked chicken here, so the insertion order is sensitive.

    %% 1. Insert the category "category" and "meta"
    {ok, 1} = pgsql:equery(C, "insert into category (id, parent_id, seq) values (116, null, 1)"),
    {ok, 1} = pgsql:equery(C, "insert into category (id, parent_id, seq) values (115, null, 1)"),
    {ok, 1} = pgsql:equery(C, "update category set parent_id = 115 where id = 116"),

    %% "http://purl.org/dc/terms/DCMIType" ?
    {ok, 1} = pgsql:equery(C, "
            insert into rsc (id, visible_for, group_id, category_id, name, uri, props)
            values (116, 0, 204, 116, 'category', $1, $2)
            ", [    undefined, 
                    [{title, {trans, [{en, <<"Category">>},      {nl, <<"Categorie">>}]}}] 
                ]),

    {ok, 1} = pgsql:equery(C, "
            insert into rsc (id, visible_for, group_id, category_id, name, uri, props)
            values (115, 0, 204, 116, 'meta', $1, $2)
            ", [    undefined, 
                    [{title, {trans, [{en, <<"Meta">>},      {nl, <<"Meta">>}]}}] 
                ]),
    
    %% Now that we have the category "category" we can insert all other categories.
    Cats = [
        % Meta categories for defining categories, predicates and groups.
        {117,115,        1, predicate,   undefined, [{title, {trans, [{en, <<"Predicate">>},     {nl, <<"Predikaat">>}]}}] },
        {118,115,        1, group,       undefined, [{title, {trans, [{en, <<"User Group">>},    {nl, <<"Gebruikersgroep">>}]}}] },

        %% Other categories
        {101,undefined,  1, other,       undefined, [{title, {trans, [{en, <<"Uncategorized">>}, {nl, <<"Zonder categorie">>}]}}] },
        {102,undefined,  1, person,      undefined, [{title, {trans, [{en, <<"Person">>}, {nl, <<"Persoon">>}]}}] },
        {103,undefined,  1, artifact,    "http://purl.org/dc/dcmitype/PhysicalObject",[{title, {trans, [{en, <<"Artifact">>}, {nl, <<"Artefact">>}]}}] },
        {104,undefined,  1, text,        "http://purl.org/dc/dcmitype/Text",          [{title, {trans, [{en, <<"Text">>}, {nl, <<"Tekst">>}]}}] },
        {105,104,        2, review,      undefined, [{title, {trans, [{en, <<"Review">>},  {nl, <<"Beoordeling">>}]}}] },
        {106,104,        1, article,     undefined, [{title, {trans, [{en, <<"Article">>}, {nl, <<"Artikel">>}]}}] },

        {107,103,        3, product,     undefined, [{title, {trans, [{en, <<"Product">>}, {nl, <<"Product">>}]}}] },
        {108, undefined, 1, event,       "http://purl.org/dc/dcmitype/Event",         [{title, {trans, [{en, <<"Event">>}, {nl, <<"Evenement">>}]}}] },

        {109,106,        1, news,        undefined,                                   [{title, {trans, [{en, <<"News">>}, {nl, <<"Nieuws">>}]}}] },

        {110,undefined,  1, media,       "http://purl.org/dc/dcmitype/Image",         [{title, {trans, [{en, <<"Media">>}, {nl, <<"Media">>}]}}] }, 
        {111,110,        1, image,       "http://purl.org/dc/dcmitype/StillImage",    [{title, {trans, [{en, <<"Image">>}, {nl, <<"Afbeelding">>}]}}] },
        {112,110,        2, video,       "http://purl.org/dc/dcmitype/MovingImage",   [{title, {trans, [{en, <<"Video">>}, {nl, <<"Video">>}]}}] },
        {113,110,        3, sound,       "http://purl.org/dc/dcmitype/Sound",         [{title, {trans, [{en, <<"Sound">>}, {nl, <<"Sound">>}]}}] },
        {114,undefined,  1, collection,  "http://purl.org/dc/dcmitype/Collection",    [{title, {trans, [{en, <<"Collection">>}, {nl, <<"Collectie">>}]}}] }
    ],

    InsertCat = fun({Id, ParentId, Seq, Name, Uri, Props}) ->
        {ok, 1} = pgsql:equery(C, "
                insert into rsc (id, visible_for, group_id, category_id, name, uri, props)
                values ($1, 0, 204, 116, $2, $3, $4)
                ", [ Id, Name, Uri, Props ]),
        {ok, 1} = pgsql:equery(C, "
                insert into category (id, parent_id, seq)
                values ($1, $2, $3)", [Id, ParentId, Seq])
    end,
    [ InsertCat(R) || R <- Cats ],
    pgsql:reset_id(C, "rsc"),
    ok = enumerate_categories(C),
    ok.
    

%% @doc Install some initial resources, most important is the system administrator
%% @todo Add the hostname to the uri
install_rsc(C) ->
    ?DEBUG("Inserting base resources (group, admin, etc.)"),
    Rsc = [
        % id  vsfr  grp  cat     name,        props
        [ 204,  0,    204,   118,  "g_admins",  [{title,<<"Administrators">>}] ],
        [ 205,  0,    205,   118,  "g_editors", [{title,<<"Site Editors">>}] ],
        [ 206,  0,    206,   118,  "g_communityeditors", [{title,<<"Community Editors">>}] ],
        [ 207,  0,    207,   118,  "g_supervisors", [{title,<<"Supervisors">>}] ],
        [ 208,  0,    208,   118,  "g_content", [{title,<<"Content">>}] ],

        [   1,  0,    204,   102,  "u_admin",   [{title,<<"Site Administrator">>}] ],
        [ 500,  0,    204,   106,  undefined,   [{title,<<"About Zophrenic">>}, {body, "<p>Some nice text in the body.</p>"}] ],
        [ 501,  0,    204,   109,  undefined,   [{title,<<"Some News">>}, {body, "<p>And the text of the news should be typed here.</p>"}] ]
    ],
    
    [ {ok,1} = pgsql:equery(C, "
            insert into rsc (id, visible_for, group_id, category_id, name, props)
            values ($1, $2, $3, $4, $5, $6)
            ", R) || R <- Rsc ],
    {ok, _} = pgsql:squery(C, "update rsc set creator_id = 1, modifier_id = 1, is_published = true"),
    pgsql:reset_id(C, "rsc"),
    
    % Connect person resources to the correct groups
    RscGroup = [
        % Id, Rsc  Grp    obsvr   leader
        [ 1,  1,   204,   false,  true ]
    ],

    [ {ok,1} = pgsql:equery(C, "
            insert into rsc_group (id, rsc_id, group_id, is_observer, is_leader)
            values ($1, $2, $3, $4, $5)
            ", R) || R <- RscGroup ],
    pgsql:reset_id(C, "rsc_group"),
    ok.


%% @doc Install the admin user as an user.  Uses the hard coded password "admin" when no password defined in the environment.
install_identity(C) ->
    ?DEBUG("Inserting username/password for the admin"),
    Password = case os:getenv("ZP_ADMINPASSWORD") of false -> "admin"; PW -> PW end,
    Hash = m_identity:hash(Password),
    {ok, 1} = pgsql:equery(C, "
        insert into identity (rsc_id, type, key, is_unique, propb)
        values (1, 'username_pw', 'admin', true, $1)", [Hash]),
    ok.
    

%% @doc Install some initial predicates, this list should be extended with common and useful predicates
%% @todo Extend and check this list.  Add allowed from/to categories.
install_predicate(C) ->
    ?DEBUG("Inserting predicates"),
    Preds = [
        % id    name      uri                                                   props
        [ 300, "p_about",   "http://www.w3.org/1999/02/22-rdf-syntax-ns#about",  [{reversed, false},{title, {trans, [{en,"About"},    {nl,"Over"}]}}]],
        [ 301, "p_author",  "http://purl.org/dc/elements/1.1/creator",           [{reversed, true}, {title, {trans, [{en,"Author"},   {nl,"Auteur"}]}}]],
        [ 302, "p_review",  "http://purl.org/stuff/rev#Review",                  [{reversed, true}, {title, {trans, [{en,"Reviews"},  {nl,"Beoordeelt"}]}}]],
        [ 303, "p_relation","http://purl.org/dc/elements/1.1/relation",          [{reversed, false},{title, {trans, [{en,"Relation"}, {nl,"Relatie"}]}}]],
        [ 304, "p_depiction","http://xmlns.com/foaf/0.1/depiction",              [{reversed, false},{title, {trans, [{en,"Depiction"},{nl,"Afbeelding"}]}}]]
    ],

    {ok, CatId}   = pgsql:squery1(C, "select id from rsc where name = 'predicate'"),
    {ok, GroupId} = pgsql:squery1(C, "select id from rsc where name = 'g_admins'"),
    
    [ {ok,1} = pgsql:equery(C, "
            insert into rsc (id, name, uri, props, group_id, category_id, is_published, creator_id, modifier_id)
            values ($1, $2, $3, $4, $5, $6, true, 1, 1)
            ", R ++ [GroupId,CatId]) || R <- Preds],
    pgsql:reset_id(C, "rsc"),
    ok.


%% @doc Install example edges between the predefined content
install_edge(C) ->
    ?DEBUG("Inserting sample edge"),
    Edges = [
        %  subj  obj  pred    seq
        [  501,  500,   301,    1  ]
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
    ?DEBUG("Sorting the category hierarchy"),
    {ok, _, CatTuples} = pgsql:equery(C, "select id, parent_id, seq from category"),
    Enums = m_category:enumerate(CatTuples),
    [
        {ok, _} = pgsql:equery(C, "update category set nr = $2, lvl = $3, lft = $4, rght = $5, props = $6 where id = $1", [CatId, Nr, Level, Left, Right, [{path,Path}]])
        || {CatId, Nr, Level, Left, Right, Path} <- Enums
    ],
    ok.
