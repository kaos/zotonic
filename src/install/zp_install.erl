%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-07
%%
%% @doc Install Zophrenic, loads the datamodel into the database
%% Assumes the database has already been created (which normally needs superuser permissions anyway)
%%
%% CREATE DATABASE zophrenic WITH OWNER = zophrenic ENCODING = 'UTF8';
%% CREATE LANGUAGE "plpgsql";

-module(zp_install).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    install/1
]).

%% @doc Install the database on the database connection supplied
%% @spec install(Database) -> ok
install(Database) ->
    {ok, C} = pgsql_pool:get_connection(Database),
    ok = pgsql:with_transaction(C, fun install_all/1),
    pgsql_pool:return_connection(Database, C),
    ok.


install_all(C) ->
    install_sql_list(C, model_pgsql()),
    zp_install_data:install(C),
    ok.

install_sql_list(C, Model) ->
    [ {ok, [], []} = pgsql:squery(C, Sql) || Sql <- Model ].
    

%% @doc Return a list containing the SQL statements to build the database model
model_pgsql() ->
    [
    
    % Table config
    % Holds all configuration keys
    "CREATE TABLE config
    (
        id serial NOT NULL,
        module character varying(80) NOT NULL DEFAULT 'zophrenic'::character varying,
        key character varying(80) NOT NULL DEFAULT ''::character varying,
        value character varying(1000) NOT NULL DEFAULT ''::character varying,
        props bytea,
        created timestamp with time zone NOT NULL DEFAULT now(),
        modified timestamp with time zone NOT NULL DEFAULT now(),
        
        CONSTRAINT config_pkey PRIMARY KEY (id),
        CONSTRAINT config_module_key_key UNIQUE (module, key)
    )",

    
    % Table module
    % Holds install state of all known modules

    "CREATE TABLE module
    (
        id serial NOT NULL,
        name character varying(80) NOT NULL DEFAULT ''::character varying,
        uri character varying(250) NOT NULL DEFAULT ''::character varying,
        is_active boolean NOT NULL DEFAULT false,
        created timestamp with time zone NOT NULL DEFAULT now(),
        modified timestamp with time zone NOT NULL DEFAULT now(),
        
        CONSTRAINT module_pkey PRIMARY KEY (id),
        CONSTRAINT module_name_key UNIQUE (name)
    )",

    
    % Table: rsc
    % Holds all resources (posts, persons etc.)
    % @todo Split the pivot part when we want to support MySQL (no fulltext in InnoDB...)

    "CREATE TABLE rsc
    (
        id serial NOT NULL,
        uri character varying(250),
        name character varying(80),
        is_authoritative boolean NOT NULL DEFAULT true,
        is_published boolean NOT NULL DEFAULT false,
        is_featured boolean NOT NULL DEFAULT false,
        publication_start timestamp with time zone NOT NULL DEFAULT now(),
        publication_end timestamp with time zone NOT NULL DEFAULT '9999-06-01 00:00:00'::timestamp with time zone,
        group_id int NOT NULL,
        creator_id int,
        modifier_id int,
        version int NOT NULL DEFAULT 1,
        category_id int NOT NULL DEFAULT 1,
        visible_for int NOT NULL DEFAULT 1, -- 0 = public, 1 = community, 2 = group
        comment_by int NOT NULL DEFAULT 3, -- 0 = public, 1 = community, 2 = group, 3 = nobody
        comments int NOT NULL default 0,
        rating int,
        rating_count int,
        slug character varying(80) NOT NULL DEFAULT ''::character varying,
        props bytea,
        created timestamp with time zone NOT NULL DEFAULT now(),
        modified timestamp with time zone NOT NULL DEFAULT now(),

        -- pivot fields for searching
        pivot_tsv tsvector,       -- texts 
        pivot_rtsv tsvector,      -- related ids (cat, prop, rsc)

    	pivot_first_name character varying(100),
    	pivot_surname character varying(100),
        pivot_gender character varying(1),
        
        pivot_date_start timestamp with time zone,
        pivot_date_end timestamp with time zone,
        pivot_date_start_month_day int,  -- used for birthdays
        pivot_date_end_month_day int,    -- used for decease dates
        
        pivot_street character varying(120),
        pivot_city character varying(100),
        pivot_state character varying(50),
        pivot_postcode character varying(30),
        pivot_country character varying(80),
        pivot_geocode character varying(20),

        CONSTRAINT resource_pkey PRIMARY KEY (id),
        CONSTRAINT rsc_uri_key UNIQUE (uri),
        CONSTRAINT name UNIQUE (name)
    )",
    "COMMENT ON COLUMN rsc.visible_for IS '0 = public, 1 = community, 2 = group'",

    "ALTER TABLE rsc ADD CONSTRAINT fk_rsc_creator_id FOREIGN KEY (creator_id)
      REFERENCES rsc (id)
      ON UPDATE CASCADE ON DELETE SET NULL",
    "ALTER TABLE rsc ADD CONSTRAINT fk_rsc_modifier_id FOREIGN KEY (modifier_id)
      REFERENCES rsc (id)
      ON UPDATE CASCADE ON DELETE SET NULL",
      
    "CREATE INDEX fki_rsc_creator_id ON rsc (creator_id)",
    "CREATE INDEX fki_rsc_modifier_id ON rsc (modifier_id)",
    "CREATE INDEX fki_rsc_created ON rsc (created)",
    "CREATE INDEX fki_rsc_modified ON rsc (modified)",

    "CREATE INDEX rsc_pivot_tsv_key ON rsc USING gin(pivot_tsv)",
    "CREATE INDEX rsc_pivot_rtsv_key ON rsc USING gin(pivot_rtsv)",

    "CREATE INDEX rsc_pivot_surname_key ON rsc (pivot_surname)",
    "CREATE INDEX rsc_pivot_first_name_key ON rsc (pivot_first_name)",
    "CREATE INDEX rsc_pivot_gender_key ON rsc (pivot_gender)",
    "CREATE INDEX rsc_pivot_date_start_key ON rsc (pivot_date_start)",
    "CREATE INDEX rsc_pivot_date_end_key ON rsc (pivot_date_end)",
    "CREATE INDEX rsc_pivot_date_start_month_day_key ON rsc (pivot_date_start_month_day)",
    "CREATE INDEX rsc_pivot_date_end_month_day_key ON rsc (pivot_date_end_month_day)",
    "CREATE INDEX rsc_pivot_city_street_key ON rsc (pivot_city, pivot_street)",
    "CREATE INDEX rsc_pivot_country_key ON rsc (pivot_country)",
    "CREATE INDEX rsc_pivot_postcode_key ON rsc (pivot_postcode)",
    "CREATE INDEX rsc_pivot_geocode_key ON rsc (pivot_geocode)",

    % Table: predicate
    % Holds all predicates used in the edge table
    % @todo Move this to the rsc table, add a category "predicate"

    "CREATE TABLE predicate
    (
      id serial NOT NULL,
      name character varying(20) NOT NULL,
      uri character varying(250) NOT NULL DEFAULT ''::character varying,
      reversed boolean NOT NULL default false,
      props bytea,

      CONSTRAINT predicate_pkey PRIMARY KEY (id),
      CONSTRAINT predicate_uri_key UNIQUE (uri),
      CONSTRAINT predicate_name_key UNIQUE (name)
    )",

    % Table: edge
    % All relations between resources, forming a directed graph

    "CREATE TABLE edge
    (
      id serial NOT NULL,      
      subject_id int NOT NULL,
      predicate_id int NOT NULL,
      object_id int NOT NULL,
      seq int NOT NULL DEFAULT 1000000,
      creator_id int,
      created timestamp with time zone NOT NULL DEFAULT now(),

      CONSTRAINT edge_pkey PRIMARY KEY (id),
      CONSTRAINT edge_ops_key UNIQUE (object_id, predicate_id, subject_id),
      CONSTRAINT edge_spo_key UNIQUE (subject_id, predicate_id, object_id),
      CONSTRAINT fk_edge_subject_id FOREIGN KEY (subject_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_edge_object_id FOREIGN KEY (object_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_edge_predicate_id FOREIGN KEY (predicate_id)
        REFERENCES predicate (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_edge_creator_id FOREIGN KEY (creator_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE SET NULL
    )",

    "CREATE INDEX fki_edge_subject_id ON edge (subject_id)",
    "CREATE INDEX fki_edge_predicate_id ON edge (predicate_id)",
    "CREATE INDEX fki_edge_object_id ON edge (object_id)",
    "CREATE INDEX fki_edge_creator_id ON edge (creator_id)",
    "CREATE INDEX edge_sp_seq_key ON edge (subject_id, predicate_id, seq)",


    % Table: grouptype
    % The possible types of a group, with multilingual titles
    % @todo Move this into the category system (or remove it for now)

    "CREATE TABLE grouptype
    (
      id serial NOT NULL,
      name character varying(80) NOT NULL,
      props bytea,
      CONSTRAINT grouptype_pkey PRIMARY KEY (id),
      CONSTRAINT name_key UNIQUE (name)
    )",

    % Table: group
    % A group is the form in which people work together on content. 
    % Every resource is part of exactly one group (one and only one).
    % @todo Connect "group" to a resource, make the resource part of the group.

    "CREATE TABLE \"group\"
    (
      id serial NOT NULL,
      grouptype_id int NOT NULL,
      name character varying(80) NOT NULL default ''::character varying,
      description character varying(2000) NOT NULL default ''::character varying,
      is_admin boolean NOT NULL DEFAULT false,
      is_supervisor boolean NOT NULL DEFAULT false,
      is_community_publisher boolean NOT NULL DEFAULT false,
      is_public_publisher boolean NOT NULL DEFAULT false,
      props bytea,
      CONSTRAINT group_pkey PRIMARY KEY (id),
      CONSTRAINT fk_group_grouptype_id FOREIGN KEY (grouptype_id)
        REFERENCES grouptype (id)
        ON UPDATE CASCADE ON DELETE SET NULL
    )",

    "CREATE INDEX fki_group_grouptype_id ON \"group\" (grouptype_id)",


    % Now that we have the group table we can add rsc/group foreign key
    "ALTER TABLE rsc ADD CONSTRAINT fk_rsc_group FOREIGN KEY (group_id)
      REFERENCES \"group\"(id)
      ON UPDATE CASCADE ON DELETE RESTRICT",
    
    "CREATE INDEX fki_rsc_group ON rsc (group_id)",

    % Table rsc_group
    % Members of a group, every user can be member of multiple groups
    % When is_observer is set then the group member can only view and comment on content

    "CREATE TABLE rsc_group
    (
      id serial NOT NULL,
      rsc_id int NOT NULL,
      group_id int NOT NULL,
      is_observer boolean NOT NULL DEFAULT false,
      is_leader boolean NOT NULL DEFAULT false,

      CONSTRAINT rsc_group_pkey PRIMARY KEY (id),
      CONSTRAINT rsc_group_rsc_id_group_id_key UNIQUE (rsc_id, group_id),
      CONSTRAINT fk_rsc_group_rsc_id FOREIGN KEY (rsc_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_rsc_group_group_id FOREIGN KEY (group_id)
        REFERENCES \"group\" (id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX fki_rsc_group_rsc_id ON rsc_group (rsc_id)",
    "CREATE INDEX fki_rsc_group_group_id ON rsc_group (group_id)",


    % Table medium
    % Holds all references to media, used in the context of resources
    % Every medium is a resource

    "CREATE TABLE medium
    (
      id serial NOT NULL,
      rsc_id int NOT NULL,
      filename character varying(400) NOT NULL,
      rootname character varying(100) NOT NULL,
      mime character varying(64) NOT NULL DEFAULT 'application/octet-stream'::character varying,
      width int NOT NULL DEFAULT 0,
      height int NOT NULL DEFAULT 0,
      orientation int NOT NULL DEFAULT 1,
      size int NOT NULL DEFAULT 0,
      props bytea,
      created timestamp with time zone NOT NULL DEFAULT now(),

      CONSTRAINT medium_pkey PRIMARY KEY (id),
      CONSTRAINT medium_rsc_id_key UNIQUE (rsc_id),
      CONSTRAINT medium_filename_key UNIQUE (filename),
      CONSTRAINT fk_medium_rsc_id FOREIGN KEY (rsc_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX medium_rootname_key ON medium (rootname)",


    % Table comment
    % Comments on resources
    % notify_id is typically set to the owner of the resource being commented on (at the time of the comment)

    "CREATE TABLE comment
    (
      id serial NOT NULL,
      rsc_id int NOT NULL,
      creator_id int,
      notify_id int,
      props bytea,
      ip_address character varying(40),
      rating int,
      created timestamp with time zone NOT NULL DEFAULT now(),
      
      CONSTRAINT comment_pkey PRIMARY KEY (id),
      CONSTRAINT fk_comment_rsc_id FOREIGN KEY (rsc_id)
        REFERENCES rsc(id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_comment_creator_id FOREIGN KEY (creator_id)
        REFERENCES rsc(id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_comment_notify_id FOREIGN KEY (notify_id)
        REFERENCES rsc(id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX fki_comment_rsc_id ON comment (rsc_id)",
    "CREATE INDEX fki_comment_creator_id ON comment (creator_id)",
    "CREATE INDEX fki_comment_notify_id ON comment (notify_id)",

    % Table category
    % nr, left and right are filled using a topological sort of the category tree
    % @todo Attach the categories to a rsc (remove name, props and medium_id; add rsc_id)
    % @todo Add a name field for naming different categorisation systems
    % A category hierarchy is derived from a resource with the category "category hierarchy"

    "CREATE TABLE category
    (
      id serial NOT NULL,
      parent_id int,
      name character varying(80),
      medium_id int,
      seq int NOT NULL DEFAULT 1000000,
      nr int NOT NULL DEFAULT 0,
      lvl int NOT NULL DEFAULT 0,
      lft int NOT NULL DEFAULT 0,
      rght int NOT NULL DEFAULT 0,
      props bytea,
      CONSTRAINT category_pkey PRIMARY KEY (id),
      CONSTRAINT category_name_key UNIQUE (name),
      CONSTRAINT fk_category_medium_id FOREIGN KEY (medium_id)
        REFERENCES medium(id)
        ON UPDATE CASCADE ON DELETE SET NULL
    )",

    "ALTER TABLE category ADD CONSTRAINT fk_category_parent_id FOREIGN KEY (parent_id)
      REFERENCES category (id)
      ON UPDATE CASCADE ON DELETE SET NULL",
    "CREATE INDEX fki_category_parent_id ON category(parent_id)",
    "CREATE INDEX fki_category_medium_id ON category(medium_id)",
    "CREATE INDEX category_nr_key ON category (nr)",

    % Table: predicate_category
    % Defines which categories are valid for a predicate as subject or object
    % @todo Attach this to rscs which are predicates

    "CREATE TABLE predicate_category
    (
      id serial NOT NULL,
      is_subject boolean NOT NULL DEFAULT true,
      predicate_id int NOT NULL,
      category_id int NOT NULL,

      CONSTRAINT predicate_category_pkey PRIMARY KEY (id),
      CONSTRAINT predicate_category_key UNIQUE (predicate_id, is_subject, category_id),
      CONSTRAINT fk_predicate_category_predicate_id FOREIGN KEY (predicate_id)
        REFERENCES predicate(id)
        ON UPDATE CASCADE
        ON DELETE CASCADE,
      CONSTRAINT fk_predicate_category_category_id FOREIGN KEY (category_id)
        REFERENCES category(id)
        ON UPDATE CASCADE
        ON DELETE CASCADE
    )",
    
    "CREATE INDEX fki_predicate_category_predicate_id ON predicate_category (predicate_id)",
    "CREATE INDEX fki_predicate_category_category_id ON predicate_category (category_id)",

    % Table visitor
    % Holds information of a visitor.  Can be shopping cart, click history etc

    "CREATE TABLE visitor
    (
      id bigserial NOT NULL,
      rsc_id int,
      props bytea,
      created timestamp with time zone NOT NULL DEFAULT now(),
      modified timestamp with time zone NOT NULL DEFAULT now(),
      CONSTRAINT visitor_pkey PRIMARY KEY (id),
      CONSTRAINT fk_visitor_rsc_id FOREIGN KEY (rsc_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX fki_visitor_rsc_id ON visitor (rsc_id)",

    % Table visitor_cookie
    % Cookies that couple an user agent to a known visitor

    "CREATE TABLE visitor_cookie
    (
      cookie character varying (64) NOT NULL,
      visitor_id bigint NOT NULL,
      autologon_expire timestamp with time zone NOT NULL DEFAULT '2000-01-01 00:00:00+01'::timestamp with time zone,
      created timestamp with time zone NOT NULL DEFAULT now(),
      CONSTRAINT visitor_cookie_pkey PRIMARY KEY (cookie),
      CONSTRAINT fk_visitor_id FOREIGN KEY (visitor_id)
        REFERENCES visitor (id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX fki_visitor_id ON visitor_cookie (visitor_id)",

    % Table rating
    % rating can be coupled with a comment, remove the comment and the rating gets adapted
    
    "
    CREATE TABLE rating
    (
        id serial NOT NULL,
        rsc_id int not null,
        visitor_id bigint not null,
        comment_id int,
        created timestamp with time zone NOT NULL DEFAULT now(),
        ip_address character varying(40),
        CONSTRAINT rating_pkey PRIMARY KEY (id),
        CONSTRAINT fk_rating_rsc_id FOREIGN KEY (rsc_id)
          REFERENCES rsc(id)
          ON UPDATE CASCADE ON DELETE CASCADE,
        CONSTRAINT fk_rating_comment_id FOREIGN KEY (comment_id)
          REFERENCES comment(id)
          ON UPDATE CASCADE ON DELETE CASCADE,
        CONSTRAINT fk_rating_visitor_id FOREIGN KEY (visitor_id)
          REFERENCES visitor(id)
          ON UPDATE CASCADE ON DELETE CASCADE
    )
    ",

    "CREATE INDEX fki_rating_rsc_id ON rating (rsc_id)",
    "CREATE INDEX fki_rating_comment_id ON rating (comment_id)",
    "CREATE INDEX fki_rating_visitor_id ON rating (visitor_id)",

    % Table identity
    % Identities of an user, used for authentication.  Examples are password, openid, msn, xmpp etc.

    "CREATE TABLE identity
    (
      id serial NOT NULL,
      rsc_id int NOT NULL,
      type character varying(32) NOT NULL DEFAULT ''::character varying,
      key character varying(200) NOT NULL DEFAULT ''::character varying,
      is_unique boolean,          -- set to true when the type/key should be unique
      propb bytea,
      prop1 character varying(200) NOT NULL DEFAULT ''::character varying,
      prop2 character varying(200) NOT NULL DEFAULT ''::character varying,
      prop3 character varying(200) NOT NULL DEFAULT ''::character varying,
      verified boolean NOT NULL DEFAULT false,
      created timestamp with time zone NOT NULL DEFAULT now(),
      modified timestamp with time zone NOT NULL DEFAULT now(),
      visited timestamp with time zone,

      CONSTRAINT auth_pkey PRIMARY KEY (id),
      CONSTRAINT pk_auth_rsc_id FOREIGN KEY (rsc_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT identity_type_key_unique UNIQUE (type, key, is_unique)
    )",

    "CREATE INDEX fki_identity_rsc_id ON identity (rsc_id)",
    "CREATE INDEX identity_type_key_key ON identity (type, key)",
    "CREATE INDEX identity_visited_key ON identity (visited)",
    "CREATE INDEX identity_created_key ON identity (created)",

    % Email send queue and log
    "CREATE TABLE emailq (
        id serial NOT NULL,
        status character varying(10) not null default 'new', -- new, sent, fail
        retry_on timestamp with time zone default (now() + '00:10:00'::interval),
        retry int not null default 0,
        sender character varying(100),
        recipient character varying(100),
        props bytea,
        sent timestamp with time zone,
        created timestamp with time zone not null default now(),
        CONSTRAINT email_pkey PRIMARY KEY (id)
    )",

    "CREATE INDEX email_recipient_key ON emailq (recipient)",
    "CREATE INDEX email_created_key ON emailq (created)",
    "CREATE INDEX email_status_retry_key ON emailq (status, retry_on)",


    % pivot queue for rsc, all things that are updated are queued here for later full text indexing
    "CREATE TABLE rsc_pivot_queue
    (
        rsc_id int NOT NULL,
        serial int NOT NULL DEFAULT 1,
        due timestamp NOT NULL,
        is_update boolean NOT NULL default true,
        
        CONSTRAINT rsc_pivot_queue_pkey PRIMARY KEY (rsc_id),
        CONSTRAINT fk_rsc_pivot_queue_rsc_id FOREIGN KEY (rsc_id)
          REFERENCES rsc(id)
          ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX fki_rsc_pivot_queue_rsc_id ON rsc_pivot_queue (rsc_id)",
    "CREATE INDEX fki_rsc_pivot_queue_due ON rsc_pivot_queue (is_update, due)",

    % Update/insert trigger on rsc to fill the update queue
    % The text indexing is delayed until the updates are stable
    "
    CREATE FUNCTION rsc_pivot_update() RETURNS trigger AS $$
    declare 
        duetime timestamp;
        do_queue boolean;
    begin
        if (tg_op = 'INSERT') then
            duetime := now() - interval '10 minute';
            do_queue := true;
        elseif (new.version <> old.version or new.modified <> old.modified) then
            duetime := now() + interval '10 minute';
            do_queue := true;
        else
            do_queue := false;
        end if;

        if (do_queue) then
            <<insert_update_queue>>
            loop
                update rsc_pivot_queue 
                set due = (case when duetime < due then duetime else due end),
                    serial = serial + 1
                where rsc_id = new.id;
            
                exit insert_update_queue when found;
            
                begin
                    insert into rsc_pivot_queue (rsc_id, due, is_update) values (new.id, duetime, tg_op = 'UPDATE');
                    exit insert_update_queue;
                exception
                    when unique_violation then
                        -- do nothing
                end;
            end loop insert_update_queue;
        end if;
        return null;
    end;
    $$ LANGUAGE plpgsql
    ",
    "
    CREATE TRIGGER rsc_update_queue_trigger AFTER INSERT OR UPDATE
    ON rsc FOR EACH ROW EXECUTE PROCEDURE rsc_pivot_update()
    ",

    % Queue for deleted medium files, periodically checked for deleting files that are not referenced anymore
    "CREATE TABLE medium_deleted
    (
        id serial NOT NULL,
        filename character varying (400) NOT NULL,
        deleted timestamp NOT NULL default now(),
        
        CONSTRAINT medium_deleted_pkey PRIMARY KEY (id)
    )",

    "CREATE INDEX medium_deleted_deleted_key ON medium_deleted (deleted)",

    % Update/insert trigger on medium to fill the deleted files queue
    "
    CREATE FUNCTION medium_delete() RETURNS trigger AS $$
    begin
        if (tg_op = 'DELETE' and old.filename <> '') then
            insert into medium_deleted (filename) values (old.filename);
        end if;
        return null;
    end;
    $$ LANGUAGE plpgsql
    ",
    "
    CREATE TRIGGER medium_deleted_trigger AFTER DELETE
    ON medium FOR EACH ROW EXECUTE PROCEDURE medium_delete()
    "

    ].


%    -- Fulltext index of products
%    -- TODO: Also mix in the shop product id, brand, group and properties
%    -- TODO: Use ispell for handling typos
%    CREATE INDEX shop_product_tsv ON shop_product USING gin(tsv);
%    CREATE FUNCTION shop_product_trigger() RETURNS trigger AS $$ 
%    begin
%      new.tsv := 
%        setweight(to_tsvector('pg_catalog.dutch', coalesce(new.title_nl,'')), 'A') || 
%        setweight(to_tsvector('pg_catalog.dutch', coalesce(new.desc_nl,'')),  'D') ||
%        setweight(to_tsvector('pg_catalog.english', coalesce(new.title_en,'')), 'A') || 
%        setweight(to_tsvector('pg_catalog.english', coalesce(new.desc_en,'')),  'D'); 
%      return new; 
%    end 
%    $$ LANGUAGE plpgsql; 
%    CREATE TRIGGER tsvectorupdate_shop_product BEFORE INSERT OR UPDATE 
%    ON shop_product FOR EACH ROW EXECUTE PROCEDURE shop_product_trigger();



