-- TODO:
-- Media
-- Section (tree) (has showcases and section_page)
-- Place

-- For e-commerce:
--   Shopping Cart
--   Orders
--   SKUs
--   Article (item that can be sold)

-- For community:
--   Advertisement
--   Pin (aka comment)
--   Tag
--   Series (aka Sermon Series)

-- translations - how to make translations of content -> maybe arrays?
-- couple relational tables for Post
-- indices for tables inheriting resources


-- Table: resource

-- DROP TABLE resource;

CREATE TABLE resource
(
  id bigserial NOT NULL,
  created timestamp with time zone NOT NULL DEFAULT now(),
  modified timestamp with time zone NOT NULL DEFAULT now(),
  created_by bigint,
  modified_by bigint,
  slug character varying(80) NOT NULL DEFAULT ''::character varying,
  is_published boolean NOT NULL DEFAULT false,
  publication_start timestamp with time zone NOT NULL DEFAULT now(),
  publication_end timestamp with time zone NOT NULL DEFAULT '9999-01-01 00:00:00+01'::timestamp with time zone,
  visible_for int NOT NULL DEFAULT 1,
  assembly_id bigint,
  CONSTRAINT resource_pkey PRIMARY KEY (id)
)
WITH (OIDS=FALSE);
ALTER TABLE resource OWNER TO zophrenic;

COMMENT ON COLUMN resource.visible_for IS '0 = public, 1 = community, 2 = assembly';


-- Table: address

-- DROP TABLE address;

CREATE TABLE address
(
  address_id bigserial NOT NULL,
  street1 character varying(80) NOT NULL DEFAULT ''::character varying,
  street2 character varying(80) NOT NULL DEFAULT ''::character varying,
  city character varying(50) NOT NULL DEFAULT ''::character varying,
  state character varying(50) NOT NULL DEFAULT ''::character varying,
  postcode character varying(30) NOT NULL DEFAULT ''::character varying,
  country character varying(80) NOT NULL DEFAULT ''::character varying,
  geo_lat real,
  geo_long real,
  geocode character varying(20) NOT NULL DEFAULT ''::character varying,
  mail_is_special boolean NOT NULL DEFAULT false,
  m_street1 character varying(80) NOT NULL DEFAULT ''::character varying,
  m_street2 character varying(80) NOT NULL DEFAULT ''::character varying,
  m_city character varying(50) NOT NULL DEFAULT ''::character varying,
  m_state character varying(50) NOT NULL DEFAULT ''::character varying,
  m_postcode character varying(30) NOT NULL DEFAULT ''::character varying,
  m_country character varying(80) NOT NULL DEFAULT ''::character varying,
  m_addressee character varying(80) NOT NULL DEFAULT ''::character varying,
  CONSTRAINT address_pkey PRIMARY KEY (address_id)
)
WITH (OIDS=FALSE);
ALTER TABLE address OWNER TO zophrenic;




-- Table: family

-- DROP TABLE family;


CREATE TABLE family
(
  id serial NOT NULL,
  name character varying(100) NOT NULL DEFAULT ''::character varying,
  emergency_phone_1 character varying(20) NOT NULL DEFAULT ''::character varying,
  emergency_phone_2 character varying(20) NOT NULL DEFAULT ''::character varying,

  CONSTRAINT family_pkey PRIMARY KEY (id),
)
WITH (OIDS=FALSE);
ALTER TABLE address OWNER TO zophrenic;

CREATE INDEX family_name ON family USING btree (name);



-- Table: person

-- DROP TABLE person;

CREATE TABLE person
(
  anonymous boolean NOT NULL DEFAULT true,
  visited timestamp with time zone,

  family int,
  is_family_contact boolean NOT NULL DEFAULT false,
  is_family_child boolean NOT NULL DEFAULT false,

  nick character varying(50),
  prefix character varying(20) NOT NULL DEFAULT ''::character varying,
  first_name character varying(100) NOT NULL DEFAULT ''::character varying,
  given_names character varying(150) NOT NULL DEFAULT ''::character varying,
  middle_name character varying(100) NOT NULL DEFAULT ''::character varying,
  surname_prefix character varying(20) NOT NULL DEFAULT ''::character varying,
  surname character varying(100) NOT NULL DEFAULT ''::character varying,
  suffix character varying(20) NOT NULL DEFAULT ''::character varying,
  gender character(1) NOT NULL DEFAULT '?'::character,

  is_organization boolean NOT NULL DEFAULT false,
  organization character varying(100) NOT NULL DEFAULT ''::character varying,
  job_title character varying(100) NOT NULL DEFAULT ''::character varying,
  department character varying(100) NOT NULL DEFAULT ''::character varying,

  birth_date date,
  birth_year integer,
  birth_month integer,
  birth_day integer,

  decease_date date,

  email character varying(80) NOT NULL DEFAULT ''::character varying,
  url character varying(250) NOT NULL DEFAULT ''::character varying,
  openid character varying(250),
  "password" character varying(80),
  phone character varying(20) NOT NULL DEFAULT ''::character varying,
  phone_alt character varying(20) NOT NULL DEFAULT ''::character varying,
  phone_emergency character varying(20) NOT NULL DEFAULT ''::character varying,

  is_admin boolean NOT NULL DEFAULT false,
  is_editor boolean NOT NULL DEFAULT false,
  is_supervisor boolean NOT NULL DEFAULT false,
  is_staff boolean NOT NULL DEFAULT false,
  is_member boolean NOT NULL DEFAULT false,

  about character varying(50000) NOT NULL DEFAULT ''::character varying,
  allergies character varying(50000) NOT NULL DEFAULT ''::character varying,
  notes character varying(50000) NOT NULL DEFAULT ''::character varying,

  mailing boolean NOT NULL DEFAULT true,

  CONSTRAINT person_pkey PRIMARY KEY (id),
  CONSTRAINT person_nick_key UNIQUE (nick),
  CONSTRAINT person_openid_key UNIQUE (openid)
  CONSTRAINT fk_person_created_by FOREIGN KEY (created_by)
    REFERENCES person (id) MATCH SIMPLE
    ON UPDATE CASCADE ON DELETE SET NULL,
  CONSTRAINT fk_person_modified_by FOREIGN KEY (modified_by)
    REFERENCES person (id) MATCH SIMPLE
    ON UPDATE CASCADE ON DELETE SET NULL,
)
INHERITS (resource, address)
WITH (OIDS=FALSE);
ALTER TABLE person OWNER TO zophrenic;

CREATE INDEX fki_person_created_by ON person USING btree (created_by);
CREATE INDEX fki_person_modified_by ON person USING btree (modified_by);
CREATE INDEX person_birth_date ON person USING btree (birth_date);
CREATE INDEX person_birth_day ON person USING btree (birth_month, birth_day);
CREATE INDEX person_decease_date ON person USING btree (decease_date);
CREATE INDEX person_name ON person USING btree (surname, first_name);
CREATE INDEX person_password ON person USING btree (password);
CREATE INDEX person_visited ON person USING btree (visited);



-- Table: person_cookie

-- DROP TABLE person_cookie;

CREATE TABLE person_cookie
(
  id bigserial NOT NULL,
  person_id bigint NOT NULL,
  cookie character varying(32) NOT NULL,
  created timestamp with time zone NOT NULL DEFAULT now(),
  autologon_expire timestamp with time zone NOT NULL DEFAULT '2000-01-01 00:00:00+01'::timestamp with time zone,
  CONSTRAINT person_cookie_pkey PRIMARY KEY (id),
  CONSTRAINT fk_person_cookie_person_id FOREIGN KEY (person_id)
      REFERENCES person (id) MATCH SIMPLE
      ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT person_cookie_cookie_key UNIQUE (cookie)
)
WITH (OIDS=FALSE);
ALTER TABLE person_cookie OWNER TO zophrenic;

CREATE INDEX fki_person_cookie_person_id ON person_cookie USING hash (person_id);



-- Table: family_person_emergency

-- DROP TABLE family_person_emergency;

CREATE TABLE family_person_emergency
(
  family_id int NOT NULL,
  person_id bigint NOT NULL,
  CONSTRAINT family_person_emergency_pkey PRIMARY KEY (family_id, person_id),
  CONSTRAINT family_person_emergency_person_id_fkey FOREIGN KEY (person_id)
    REFERENCES person (id) MATCH SIMPLE
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT family_person_emergency_family_id_fkey FOREIGN KEY (family_id)
    REFERENCES family (id) MATCH SIMPLE
    ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (OIDS=FALSE);
ALTER TABLE family_person_emergency OWNER TO zophrenic;

CREATE INDEX family_person_emergency_person_id ON family_person_emergency USING hash (person_id);
CREATE INDEX family_person_emergency_family_id ON family_person_emergency USING hash (family_id);




-- Table: assembly

-- DROP TABLE assembly;

CREATE TABLE assembly
(
  id serial NOT NULL,
  name character varying(80) NOT NULL default ''::character varying,
  description character varying(2000) NOT NULL default ''::character varying,
  leader_id bigint,
  CONSTRAINT assembly_pkey PRIMARY KEY (id),
  CONSTRAINT fk_assembly_leader_id FOREIGN KEY (leader_id)
    REFERENCES person (id) MATCH SIMPLE
    ON UPDATE CASCADE ON DELETE SET NULL,
)
WITH (OIDS=FALSE);
ALTER TABLE assembly OWNER TO zophrenic;

CREATE INDEX fki_assembly_leader_id ON assembly USING hash (leader_id);


-- Table: assembly_person

-- DROP TABLE assembly_person;

CREATE TABLE assembly_person
(
  assembly_id int NOT NULL,
  person_id bigint NOT NULL,
  CONSTRAINT assembly_person_pkey PRIMARY KEY (family_id, person_id),
  CONSTRAINT fk_assembly_person_person_id FOREIGN KEY (person_id)
    REFERENCES person (id) MATCH SIMPLE
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_assembly_person_assembly_id FOREIGN KEY (assembly_id)
    REFERENCES assembly (id) MATCH SIMPLE
    ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (OIDS=FALSE);
ALTER TABLE assembly_person OWNER TO zophrenic;

CREATE INDEX fki_assembly_person_person_id ON assembly_person USING hash (person_id);
CREATE INDEX fki_assembly_person_assembly_id ON assembly_person USING hash (assembly_id);


-- Table: blog

-- DROP TABLE blog;

CREATE TABLE blog
(
  name character varying(100),
  CONSTRAINT blog_pkey PRIMARY KEY (id),
)
INHERITS(resource);
ALTER TABLE blog OWNER TO zophrenic;

-- Add the normal set of resource indices

CREATE INDEX blog_name ON blog USING btree (name);




-- Table: post_type

-- DROP TABLE post_type;

-- Types are: post, event, link, sermon etc.

CREATE TABLE post_type
(
    name character varying(20) NOT NULL,
    CONSTRAINT post_type_pkey PRIMARY KEY (name)
);
ALTER TABLE post_type OWNER TO zophrenic;



-- Table post

-- DROP TABLE post;

CREATE TABLE post
(
  post_type character varying(20) NOT NULL,

  -- Content
  title character varying(100) NOT NULL default ''::character varying,
  abstract character varying(200) NOT NULL default ''::character varying,
  introduction character varying(500) NOT NULL default ''::character varying,
  body character varying(10000) NOT NULL default ''::character varying,
  url character varying(250) NOT NULL default ''::character varying,
  
  -- VEvent
  date_start timestamp with time zone,
  date_end timestamp with time zone,
  need_registration boolean NOT NULL default false,
  
  -- Relation
  speaker_id bigint,
  blog_id bigint,
  
  -- serie (bigint)

  -- people (person)
  -- participants (person)
  -- related (post)
  -- media (media)  
  -- place (place)
  -- ?collection

  CONSTRAINT post_pkey PRIMARY KEY(id),
  CONSTRAINT fk_post_post_type FOREIGN KEY (post_type)
    REFERENCES post_type (name) MATCH SIMPLE
    ON UPDATE CASCADE ON DELETE CASCADE
)
INHERITS(resource);
ALTER TABLE post OWNER TO zophrenic;


-- Add the normal set of resource indices

CREATE INDEX fki_post_post_type ON post USING hash (post_type);
CREATE INDEX post_type_created ON post USING btree (post_type, created);


