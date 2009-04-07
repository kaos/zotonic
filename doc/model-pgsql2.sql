-- Table: rsc

-- DROP TABLE rsc;

CREATE TABLE rsc
(
  id serial NOT NULL,
  uri character varying(250) NOT NULL DEFAULT ''::character varying,
  is_authoritative boolean NOT NULL DEFAULT true,
  is_published boolean NOT NULL DEFAULT false,
  publication_start timestamp with time zone NOT NULL DEFAULT now(),
  publication_end timestamp with time zone NOT NULL DEFAULT '9999-01-01 00:00:00+01'::timestamp with time zone,
  visible_for integer NOT NULL DEFAULT 1, -- 0 = public, 1 = community, 2 = group
  group_id int NOT NULL,
  created timestamp with time zone NOT NULL DEFAULT now(),
  modified timestamp with time zone NOT NULL DEFAULT now(),
  owner_id int,
  creator_id int,
  modifier_id int,
  category_id int NOT NULL DEFAULT 1,
  is_featured boolean NOT NULL DEFAULT false,
  slug character varying(80) NOT NULL DEFAULT ''::character varying,
  unique_name character varying(80),
  props bytea,
  CONSTRAINT resource_pkey PRIMARY KEY (id),
  CONSTRAINT rsc_uri_key UNIQUE (uri),
  CONSTRAINT unique_name_key UNIQUE (unique_name)
);

COMMENT ON COLUMN rsc.visible_for IS '0 = public, 1 = community, 2 = group';

ALTER TABLE rsc ADD CONSTRAINT fk_rsc_owner_id FOREIGN KEY (owner_id)
  REFERENCES rsc (id)
  ON UPDATE CASCADE ON DELETE SET NULL;
ALTER TABLE rsc ADD CONSTRAINT fk_rsc_creator_id FOREIGN KEY (creator_id)
  REFERENCES rsc (id)
  ON UPDATE CASCADE ON DELETE SET NULL;
ALTER TABLE rsc ADD CONSTRAINT fk_rsc_modifier_id FOREIGN KEY (modifier_id)
  REFERENCES rsc (id)
  ON UPDATE CASCADE ON DELETE SET NULL;
CREATE INDEX fki_rsc_owner_id ON rsc (owner_id);
CREATE INDEX fki_rsc_creator_id ON rsc (creator_id);
CREATE INDEX fki_rsc_modifier_id ON rsc (modifier_id);
CREATE INDEX fki_rsc_created ON rsc (created);
CREATE INDEX fki_rsc_modified ON rsc (modified);



-- Table: predicate

CREATE TABLE predicate
(
  id serial NOT NULL,
  name character varying(20) NOT NULL,
  uri character varying(250) NOT NULL DEFAULT ''::character varying,
  props bytea,
  
  CONSTRAINT predicate_pkey PRIMARY KEY (id),
  CONSTRAINT predicate_uri_key UNIQUE (uri),
  CONSTRAINT predicate_name_key UNIQUE (name)
);


-- Table: edge

CREATE TABLE edge
(
  subject_id int NOT NULL,
  predicate_id int NOT NULL,
  object_id int NOT NULL,
  seq int NOT NULL DEFAULT 1000000,
  
  CONSTRAINT edge_pkey PRIMARY KEY (object_id, predicate_id, subject_id),
  CONSTRAINT fk_edge_subject_id FOREIGN KEY (subject_id)
    REFERENCES rsc (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_edge_object_id FOREIGN KEY (object_id)
    REFERENCES rsc (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_edge_predicate_id FOREIGN KEY (predicate_id)
    REFERENCES predicate (id)
    ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX fki_edge_subject ON edge (subject_id);
CREATE INDEX fki_edge_predicate ON edge (predicate_id);
CREATE INDEX fki_edge_object ON edge (object_id);
CREATE INDEX edge_spo_key ON edge (subject_id, predicate_id, object_id);
CREATE INDEX edge_sp_seq_key ON edge (subject_id, predicate_id, seq);


-- Table: grouptype
-- The possible types of a group, with multilingual titles
CREATE TABLE grouptype
(
  id serial NOT NULL,
  props bytea,
  CONSTRAINT grouptype_pkey PRIMARY KEY (id)
);


-- Table: group
-- A group is the form in which people work together on content. 
-- Every resource is part of exactly one group (one and only one).

CREATE TABLE "group"
(
  id serial NOT NULL,
  grouptype_id int NOT NULL,
  name character varying(80) NOT NULL default ''::character varying,
  description character varying(2000) NOT NULL default ''::character varying,
  is_admin boolean NOT NULL DEFAULT false,
  is_editor boolean NOT NULL DEFAULT false,
  is_supervisor boolean NOT NULL DEFAULT false,
  is_community_publisher boolean NOT NULL DEFAULT false,
  is_public_publisher boolean NOT NULL DEFAULT false,
  props bytea,
  CONSTRAINT group_pkey PRIMARY KEY (id),
  CONSTRAINT fk_group_grouptype_id FOREIGN KEY (grouptype_id)
    REFERENCES grouptype (id)
    ON UPDATE CASCADE ON DELETE SET NULL
);

CREATE INDEX fki_group_grouptype_id ON "group" (grouptype_id);


-- Now that we have the group table we can add rsc/group foreign key
ALTER TABLE rsc ADD CONSTRAINT fk_rsc_group FOREIGN KEY (group_id)
  REFERENCES "group"(id)
  ON UPDATE CASCADE ON DELETE RESTRICT;
CREATE INDEX fki_rsc_group ON rsc (group_id);




-- Table rsc_group
-- Members of a group, every user can be member of multiple groups
-- When is_observer is set then the group member can only view and comment on content

CREATE TABLE rsc_group
(
  rsc_id int NOT NULL,
  group_id int NOT NULL,
  is_observer boolean NOT NULL DEFAULT false,
  is_leader boolean NOT NULL DEFAULT false,
  CONSTRAINT rsc_group_pkey PRIMARY KEY (rsc_id, group_id),
  CONSTRAINT fk_rsc_group_rsc_id FOREIGN KEY (rsc_id)
    REFERENCES rsc (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_rsc_group_group_id FOREIGN KEY (group_id)
    REFERENCES "group" (id)
    ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX fki_rsc_group_rsc_id ON rsc_group (rsc_id);
CREATE INDEX fki_rsc_group_group_id ON rsc_group (group_id);




-- Table media
-- Holds all references to files, used in the context of resources

CREATE TABLE media
(
  id serial NOT NULL,
  visible_for integer NOT NULL DEFAULT 1, -- 0 = public, 1 = community, 2 = group
  group_id int NOT NULL,
  created timestamp with time zone NOT NULL DEFAULT now(),
  creator_id int,
  filename character varying(255) NOT NULL,
  mime character varying(64) NOT NULL DEFAULT 'application/octet-stream'::character varying,
  size int NOT NULL DEFAULT 0,
  props bytea,
  CONSTRAINT media_pkey PRIMARY KEY (id),
  CONSTRAINT media_filename_key UNIQUE (filename),
  CONSTRAINT fk_media_group_id FOREIGN KEY (group_id)
    REFERENCES "group" (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_media_creator_id FOREIGN KEY (creator_id)
    REFERENCES rsc (id)
    ON UPDATE CASCADE ON DELETE SET NULL
);

CREATE INDEX fki_media_group_id ON media (group_id);
CREATE INDEX fki_media_creator_id ON media (creator_id);



-- Table rsc_media
-- All media used by a resource

CREATE TABLE rsc_media
(
  rsc_id int NOT NULL,
  media_id int NOT NULL,
  seq int NOT NULL DEFAULT 1000000,
  
  CONSTRAINT rsc_media_pkey PRIMARY KEY (rsc_id, media_id),
  CONSTRAINT fk_rsc_media_rsc_id FOREIGN KEY (rsc_id)
    REFERENCES rsc (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_rsc_media_media_id FOREIGN KEY (media_id)
    REFERENCES media (id)
    ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX fki_rsc_media_rsc_id ON rsc_media (rsc_id);
CREATE INDEX fki_rsc_media_media_id ON rsc_media (media_id);
CREATE INDEX rsc_media_seq_key ON rsc_media(rsc_id, seq);



-- Table prop_group
-- Defines a group of properties

CREATE TABLE prop_group
(
    id serial NOT NULL,
    props bytea,
    CONSTRAINT prop_group_pkey PRIMARY KEY (id)
);

-- Table prop
-- Defines the name(s) of a property.

CREATE TABLE prop
(
  id serial NOT NULL,
  prop_group_id int NOT NULL,
  props bytea,
  CONSTRAINT prop_pkey PRIMARY KEY (id),
  CONSTRAINT fk_prop_prop_group_id FOREIGN KEY (prop_group_id)
    REFERENCES prop_group(id)
    ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX fki_prop_prop_group_id ON prop(prop_group_id);


-- Table prop_value
-- Holds the different values a property can have.

CREATE TABLE prop_value
(
  id serial NOT NULL,
  prop_id int NOT NULL,
  props bytea,
  CONSTRAINT prop_value_pkey PRIMARY KEY (id),
  CONSTRAINT fk_prop_value_prop_id FOREIGN KEY (prop_id)
    REFERENCES prop(id)
    ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX fki_prop_value_prop_id ON prop_value(prop_id);


-- Table rsc_prop
-- Holds a property (w/ value) of a resource

CREATE TABLE rsc_prop
(
  rsc_id int NOT NULL,
  prop_id int NOT NULL,
  prop_value_id int,
  props bytea,
    
  CONSTRAINT rsc_prop_pkey PRIMARY KEY (rsc_id, prop_id),
  CONSTRAINT fk_rsc_prop_rsc_id FOREIGN KEY (rsc_id)
    REFERENCES rsc (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_rsc_prop_prop_id FOREIGN KEY (prop_id)
    REFERENCES prop (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_rsc_prop_prop_value_id FOREIGN KEY (prop_value_id)
    REFERENCES prop_value (id)
    ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX fki_rsc_prop_rsc_id ON rsc_prop (rsc_id);
CREATE INDEX fki_rsc_prop_prop_id ON rsc_prop (prop_id);
CREATE INDEX fki_rsc_prop_prop_value_id ON rsc_prop (prop_value_id);




-- Table comment
-- Comments on resources
-- notify_id is typically set to the owner of the resource being commented on (at the time of the comment)

CREATE TABLE comment
(
  id serial NOT NULL,
  rsc_id int NOT NULL,
  created timestamp with time zone NOT NULL DEFAULT now(),
  creator_id int,
  notify_id int,
  props bytea,
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
);

CREATE INDEX fki_comment_rsc_id ON comment (rsc_id);
CREATE INDEX fki_comment_creator_id ON comment (creator_id);
CREATE INDEX fki_comment_notify_id ON comment (notify_id);


-- Table category
-- nr, left and right are filled using a topological sort of the category tree

CREATE TABLE category
(
  id serial NOT NULL,
  parent_id int,
  seq int NOT NULL DEFAULT 1000000,
  nr int NOT NULL DEFAULT 0,
  lvl int NOT NULL DEFAULT 0,
  lft int NOT NULL DEFAULT 0,
  rght int NOT NULL DEFAULT 0,
  props bytea,
  CONSTRAINT category_pkey PRIMARY KEY (id)
);

ALTER TABLE category ADD CONSTRAINT fk_category_parent_id FOREIGN KEY (parent_id)
  REFERENCES category (id)
  ON UPDATE CASCADE ON DELETE SET NULL;
CREATE INDEX fki_category_parent_id ON category(parent_id);
CREATE INDEX category_nr_key ON category (nr);



-- Table visitor
-- Holds information of a visitor.  Can be shopping cart, click history etc

CREATE TABLE visitor
(
  id bigserial NOT NULL,
  rsc_id int,
  created timestamp with time zone NOT NULL DEFAULT now(),
  modified timestamp with time zone NOT NULL DEFAULT now(),
  props bytea,
  CONSTRAINT visitor_pkey PRIMARY KEY (id),
  CONSTRAINT fk_visitor_rsc_id FOREIGN KEY (rsc_id)
    REFERENCES rsc (id)
    ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX fki_visitor_rsc_id ON visitor (rsc_id);


-- Table visitor_cookie
-- Cookies that couple an user agent to a known visitor

CREATE TABLE visitor_cookie
(
  cookie character varying (64) NOT NULL,
  visitor_id int NOT NULL,
  created timestamp with time zone NOT NULL DEFAULT now(),
  autologon_expire timestamp with time zone NOT NULL DEFAULT '2000-01-01 00:00:00+01'::timestamp with time zone,
  CONSTRAINT visitor_cookie_pkey PRIMARY KEY (cookie),
  CONSTRAINT fk_visitor_id FOREIGN KEY (visitor_id)
    REFERENCES visitor (id)
    ON UPDATE CASCADE ON DELETE CASCADE
);

CREATE INDEX fki_visitor_id ON visitor_cookie (visitor_id);


-- Table identity
-- Identities of an user, used for authentication.  Examples are password, openid, msn, xmpp etc.

CREATE TABLE identity
(
  id serial NOT NULL,
  rsc_id int NOT NULL,
  type character varying(32) NOT NULL DEFAULT ''::character varying,
  prop1 character varying(200) NOT NULL DEFAULT ''::character varying,
  prop2 character varying(200) NOT NULL DEFAULT ''::character varying,
  prop3 character varying(200) NOT NULL DEFAULT ''::character varying,
  prop4 character varying(200) NOT NULL DEFAULT ''::character varying,
  verified boolean NOT NULL DEFAULT false,
  created timestamp with time zone NOT NULL DEFAULT now(),
  modified timestamp with time zone NOT NULL DEFAULT now(),
  visited timestamp with time zone,
  
  CONSTRAINT auth_pkey PRIMARY KEY (id),
  CONSTRAINT pk_auth_rsc_id FOREIGN KEY (rsc_id)
    REFERENCES rsc (id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT identity_prop_key UNIQUE (type, prop1, prop2, prop3, prop4)
);

CREATE INDEX fki_auth_rsc_id ON identity (rsc_id);
CREATE INDEX auth_visited_key ON identity (visited);
CREATE INDEX auth_created_key ON identity (created);




