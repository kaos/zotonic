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
    
    % Table: rsc
    % Holds all resources (posts, persons etc.)

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

    "CREATE TABLE edge
    (
      id serial NOT NULL,      
      subject_id int NOT NULL,
      predicate_id int NOT NULL,
      object_id int NOT NULL,
      seq int NOT NULL DEFAULT 1000000,
      creator_id int NOT NULL,
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

    % Table media
    % Holds all references to files, used in the context of resources

    "CREATE TABLE media
    (
      id serial NOT NULL,
      name character varying(80),
      visible_for integer NOT NULL DEFAULT 1, -- 0 = public, 1 = community, 2 = group
      group_id int NOT NULL,
      created timestamp with time zone NOT NULL DEFAULT now(),
      creator_id int,
      filename character varying(400) NOT NULL,
      rootname character varying(100) NOT NULL,
      mime character varying(64) NOT NULL DEFAULT 'application/octet-stream'::character varying,
      size int NOT NULL DEFAULT 0,
      props bytea,
      CONSTRAINT media_pkey PRIMARY KEY (id),
      CONSTRAINT media_filename_key UNIQUE (filename),
      CONSTRAINT media_name_key UNIQUE (name),
      CONSTRAINT fk_media_group_id FOREIGN KEY (group_id)
        REFERENCES \"group\" (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_media_creator_id FOREIGN KEY (creator_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE SET NULL
    )",

    "CREATE INDEX fki_media_group_id ON media (group_id)",
    "CREATE INDEX fki_media_creator_id ON media (creator_id)",
    "CREATE INDEX fki_media_rootname ON media (rootname)",


    % Table rsc_media
    % All media used by a resource

    "CREATE TABLE rsc_media
    (
      id serial NOT NULL,
      rsc_id int NOT NULL,
      media_id int NOT NULL,
      seq int NOT NULL DEFAULT 1000000,

      CONSTRAINT rsc_media_pkey PRIMARY KEY (id),
      CONSTRAINT fk_rsc_media_rsc_id FOREIGN KEY (rsc_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_rsc_media_media_id FOREIGN KEY (media_id)
        REFERENCES media (id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX rsc_media_rsc_id_media_id_key ON rsc_media (rsc_id, media_id)",
    "CREATE INDEX fki_rsc_media_rsc_id ON rsc_media (rsc_id)",
    "CREATE INDEX fki_rsc_media_media_id ON rsc_media (media_id)",
    "CREATE INDEX rsc_media_seq_key ON rsc_media(rsc_id, seq)",

    % Table prop_group
    % Defines a group of features

    "CREATE TABLE feature_group
    (
        id serial NOT NULL,
        props bytea,
        CONSTRAINT feature_group_pkey PRIMARY KEY (id)
    )",

    % Table feature
    % Defines the name(s) of a feature.

    "CREATE TABLE feature
    (
      id serial NOT NULL,
      feature_group_id int NOT NULL,
      props bytea,
      CONSTRAINT feature_pkey PRIMARY KEY (id),
      CONSTRAINT fk_feature_feature_group_id FOREIGN KEY (feature_group_id)
        REFERENCES feature_group(id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX fki_feature_feature_group_id ON feature(feature_group_id)",

    % Table feature_value
    % Holds the different values a feature can have.

    "CREATE TABLE feature_value
    (
      id serial NOT NULL,
      feature_id int NOT NULL,
      props bytea,
      CONSTRAINT feature_value_pkey PRIMARY KEY (id),
      CONSTRAINT fk_feature_value_feature_id FOREIGN KEY (feature_id)
        REFERENCES feature(id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX fki_feature_value_prop_id ON feature_value(feature_id)",

    % Table rsc_feature
    % Holds a feature (w/ value) of a resource

    "CREATE TABLE rsc_feature
    (
      rsc_id int NOT NULL,
      feature_id int NOT NULL,
      feature_value_id int,
      props bytea,

      CONSTRAINT rsc_feature_pkey PRIMARY KEY (rsc_id, feature_id),
      CONSTRAINT fk_rsc_feature_rsc_id FOREIGN KEY (rsc_id)
        REFERENCES rsc (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_rsc_feature_feature_id FOREIGN KEY (feature_id)
        REFERENCES feature (id)
        ON UPDATE CASCADE ON DELETE CASCADE,
      CONSTRAINT fk_rsc_feature_feature_value_id FOREIGN KEY (feature_value_id)
        REFERENCES feature_value (id)
        ON UPDATE CASCADE ON DELETE CASCADE
    )",

    "CREATE INDEX fki_rsc_feature_rsc_id ON rsc_feature (rsc_id)",
    "CREATE INDEX fki_rsc_feature_feature_id ON rsc_feature (feature_id)",
    "CREATE INDEX fki_rsc_feature_feature_value_id ON rsc_feature (feature_value_id)",


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

    "CREATE TABLE category
    (
      id serial NOT NULL,
      parent_id int,
      name character varying(80),
      media_id int,
      seq int NOT NULL DEFAULT 1000000,
      nr int NOT NULL DEFAULT 0,
      lvl int NOT NULL DEFAULT 0,
      lft int NOT NULL DEFAULT 0,
      rght int NOT NULL DEFAULT 0,
      props bytea,
      CONSTRAINT category_pkey PRIMARY KEY (id),
      CONSTRAINT category_name_key UNIQUE (name),
      CONSTRAINT fk_category_media_id FOREIGN KEY (media_id)
        REFERENCES media(id)
        ON UPDATE CASCADE ON DELETE SET NULL
    )",

    "ALTER TABLE category ADD CONSTRAINT fk_category_parent_id FOREIGN KEY (parent_id)
      REFERENCES category (id)
      ON UPDATE CASCADE ON DELETE SET NULL",
    "CREATE INDEX fki_category_parent_id ON category(parent_id)",
    "CREATE INDEX fki_category_media_id ON category(media_id)",
    "CREATE INDEX category_nr_key ON category (nr)",

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
    % The text indexing is deleted until the updates are stable
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



