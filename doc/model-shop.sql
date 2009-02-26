-- Product groups, with a maximum nesting of 2 (so no sub-sub-groups)
CREATE TABLE shop_group
(
  id serial NOT NULL,
  title_nl character varying(20) NOT NULL DEFAULT ''::character varying,
  descr_nl character varying(500) NOT NULL DEFAULT ''::character varying,
  title_en character varying(20) NOT NULL DEFAULT ''::character varying,
  descr_en character varying(500) NOT NULL DEFAULT ''::character varying,
  shop_group_id integer,
  sort_order integer NOT NULL,
  
  CONSTRAINT shop_group_pkey PRIMARY KEY (id)
)
WITH (OIDS=FALSE);

ALTER TABLE shop_group ADD CONSTRAINT shop_group_shop_group_id 
  FOREIGN KEY (shop_group_id) REFERENCES shop_group
  ON DELETE RESTRICT ON UPDATE CASCADE; 


-- Properties can be added to products.  They define things like colour, wheel size, etc.
CREATE TABLE shop_property
(
  id serial NOT NULL,
  title_nl character varying(20) NOT NULL DEFAULT ''::character varying,
  descr_nl character varying(200) NOT NULL DEFAULT ''::character varying,
  title_en character varying(20) NOT NULL DEFAULT ''::character varying,
  descr_en character varying(200) NOT NULL DEFAULT ''::character varying,
  sort_order int NOT NULL DEFAULT 999,

  CONSTRAINT shop_property_pkey PRIMARY KEY (id)
)
WITH (OIDS=FALSE);

CREATE INDEX shop_property_sort_order ON shop_property USING btree (sort_order);

-- Property value holds the different possible values for a property. 
-- Think of 'red', 'blue' for colour etc. 
CREATE TABLE shop_property_value
(
  id serial NOT NULL,
  shop_property_id int NOT NULL,
  title_nl character varying(20) NOT NULL DEFAULT ''::character varying,
  title_en character varying(20) NOT NULL DEFAULT ''::character varying,
  sort_order int NOT NULL DEFAULT 999,

  CONSTRAINT shop_property_value_pkey PRIMARY KEY (id),
  CONSTRAINT fk_shop_property_value_shop_property_id FOREIGN KEY (shop_property_id)
    REFERENCES shop_property(id)
    ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (OIDS=FALSE);

CREATE INDEX fki_shop_property_value_shop_property_id ON shop_property_value(shop_property_id);
CREATE INDEX shop_property_value_sort_order ON shop_property_value USING btree (shop_property_id,sort_order);


-- All brands, just the name, an image and the uri
CREATE TABLE shop_brand
(
  id serial NOT NULL,
  name character varying(50) NOT NULL DEFAULT ''::character varying,
  url character varying(200) NOT NULL DEFAULT ''::character varying,
  image_file character varying(100) NOT NULL DEFAULT ''::character varying,

  CONSTRAINT shop_brand_pkey PRIMARY KEY (id)
)
WITH (OIDS=FALSE);


-- Products.  Everything that is on sale.
CREATE TABLE shop_product
(
  id serial NOT NULL,
  title_nl character varying(80) NOT NULL DEFAULT ''::character varying,
  descr_nl character varying(2000) NOT NULL DEFAULT ''::character varying,
  title_en character varying(80) NOT NULL DEFAULT ''::character varying,
  descr_en character varying(2000) NOT NULL DEFAULT ''::character varying,
  tsv tsvector,
  stock integer NOT NULL DEFAULT 0,  -- Sum stock minus sum order lines for the last hour (or so)
  shop_brand_id integer,
  shop_group_id1 integer,
  shop_group_id2 integer,
  image_file character varying(100) NOT NULL DEFAULT ''::character varying,
  
  -- Show this product on the home or category pages (otherwise random products)
  featured_home boolean NOT NULL DEFAULT false,
  featured_group1 boolean NOT NULL DEFAULT false,
  featured_group2 boolean NOT NULL DEFAULT false,

  -- The two properties that distinguish the skus of this product
  shop_property_id1 integer,
  shop_property_id2 integer,
  
  created timestamp with time zone NOT NULL DEFAULT now(),
  modified timestamp with time zone NOT NULL DEFAULT now(),
  
  CONSTRAINT shop_product_pkey PRIMARY KEY (id),
  CONSTRAINT fk_shop_product_shop_brand_id FOREIGN KEY (shop_brand_id)
    REFERENCES shop_brand (id)
    ON UPDATE CASCADE ON DELETE SET NULL,
  CONSTRAINT fk_shop_product_shop_group_id1 FOREIGN KEY (shop_group_id1)
    REFERENCES shop_group (id)
    ON UPDATE CASCADE ON DELETE SET NULL,
  CONSTRAINT fk_shop_product_shop_group_id2 FOREIGN KEY (shop_group_id2)
    REFERENCES shop_group (id)
    ON UPDATE CASCADE ON DELETE SET NULL,
  CONSTRAINT fk_shop_product_shop_property_id1 FOREIGN KEY (shop_property_id1)
    REFERENCES shop_property (id)
    ON UPDATE CASCADE ON DELETE SET NULL,
  CONSTRAINT fk_shop_product_shop_property_id2 FOREIGN KEY (shop_property_id2)
    REFERENCES shop_property (id)
    ON UPDATE CASCADE ON DELETE SET NULL
)
WITH (OIDS=FALSE);

CREATE INDEX fki_shop_product_shop_brand_id ON shop_product (shop_brand_id);
CREATE INDEX fki_shop_product_shop_group_id1 ON shop_product (shop_group_id1);
CREATE INDEX fki_shop_product_shop_group_id2 ON shop_product (shop_group_id2);
CREATE INDEX fki_shop_product_shop_property_id1 ON shop_product (shop_property_id1);
CREATE INDEX fki_shop_product_shop_property_id2 ON shop_product (shop_property_id2);
CREATE INDEX fki_shop_product_featured_home ON shop_product (featured_home);
CREATE INDEX fki_shop_product_featured_group1 ON shop_product (shop_group_id1, featured_group1);
CREATE INDEX fki_shop_product_featured_group2 ON shop_product (shop_group_id1, shop_group_id2, featured_group2);

-- Fulltext index of products
-- TODO: Also mix in the shop product id, brand, group and properties
-- TODO: Use ispell for handling typos
CREATE INDEX shop_product_tsv ON shop_product USING gin(tsv);
CREATE FUNCTION shop_product_trigger() RETURNS trigger AS $$ 
begin
  new.tsv := 
    setweight(to_tsvector('pg_catalog.dutch', coalesce(new.title_nl,'')), 'A') || 
    setweight(to_tsvector('pg_catalog.dutch', coalesce(new.desc_nl,'')),  'D') ||
    setweight(to_tsvector('pg_catalog.english', coalesce(new.title_en,'')), 'A') || 
    setweight(to_tsvector('pg_catalog.english', coalesce(new.desc_en,'')),  'D'); 
  return new; 
end 
$$ LANGUAGE plpgsql; 
CREATE TRIGGER tsvectorupdate_shop_product BEFORE INSERT OR UPDATE 
ON shop_product FOR EACH ROW EXECUTE PROCEDURE shop_product_trigger();



-- All properties of a product
CREATE TABLE shop_product_property
(
  shop_product_id integer NOT NULL,
  shop_property_value_id integer NOT NULL,
  CONSTRAINT shop_product_property_pkey PRIMARY KEY (shop_product_id, shop_property_value_id),
  CONSTRAINT fk_shop_product_property_shop_product_id FOREIGN KEY (shop_product_id)
    REFERENCES shop_product(id)
    ON UPDATE CASCADE ON DELETE CASCADE,
  CONSTRAINT fk_shop_product_property_shop_property_value_id FOREIGN KEY (shop_property_value_id)
    REFERENCES shop_property_value(id)
    ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (OIDS=FALSE);

CREATE INDEX fki_shop_product_property_shop_product_id ON shop_product_property (shop_product_id);
CREATE INDEX fki_shop_product_property_shop_property_value_id ON shop_product_property (shop_property_value_id);


-- All Stock Keeping Units, one product can have one or more skus.  
-- Skus can be distinguished on two properties, eg. clothing on size (S,M,L,XL)
CREATE TABLE shop_sku
(
  id serial NOT NULL,

  -- The product and the properties distinguishing this sku within the product
  shop_product_id integer,
  shop_property_value_id1 integer,
  shop_property_value_id2 integer,

  available boolean NOT NULL DEFAULT TRUE,
  imported timestamp with time zone NOT NULL DEFAULT now(),
  created timestamp with time zone NOT NULL DEFAULT now(),
  modified timestamp with time zone NOT NULL DEFAULT now(),

  -- With VMSII we have to calculate the VAT percentage from the prices...
  vat float NOT NULL DEFAULT 19.0,
  
  -- Imported from VMSII (VendIT)
  article_nr character varying(50) NOT NULL,
  upc character varying(20) NOT NULL DEFAULT ''::character varying,
  description1 character varying(200) NOT NULL DEFAULT ''::character varying,
  description2 character varying(200) NOT NULL DEFAULT ''::character varying,
  stock integer NOT NULL DEFAULT 0,
  price_incl integer NOT NULL DEFAULT 0,
  price_excl integer NOT NULL DEFAULT 0,
  special_price_incl integer NOT NULL DEFAULT 0,
  special_start date NOT NULL DEFAULT '2000-01-01'::date,
  special_end date NOT NULL DEFAULT '2000-01-01'::date,
  removal_tax integer NOT NULL DEFAULT 0,
  brand character varying(50) NOT NULL DEFAULT ''::character varying,
  image_file character varying(100) NOT NULL DEFAULT ''::character varying,
  product_group character varying(200) NOT NULL DEFAULT ''::character varying,
  extra1 character varying(200) NOT NULL DEFAULT ''::character varying,
  extra2 character varying(200) NOT NULL DEFAULT ''::character varying,
  extra3 character varying(200) NOT NULL DEFAULT ''::character varying,
  extra4 character varying(200) NOT NULL DEFAULT ''::character varying,
  extra5 character varying(200) NOT NULL DEFAULT ''::character varying,
  extra6 character varying(200) NOT NULL DEFAULT ''::character varying,
  CONSTRAINT shop_sku_pkey PRIMARY KEY (id),
  CONSTRAINT shop_sku_article_nr_key UNIQUE (article_nr),
  CONSTRAINT fk_shop_sku_shop_product_id FOREIGN KEY (shop_product_id)
    REFERENCES shop_product(id)
    ON UPDATE CASCADE ON DELETE SET NULL,
  CONSTRAINT fk_shop_sku_shop_property_value_id1 FOREIGN KEY (shop_property_value_id1)
    REFERENCES shop_property_value(id)
    ON UPDATE CASCADE ON DELETE SET NULL,
  CONSTRAINT fk_shop_sku_shop_property_value_id2 FOREIGN KEY (shop_property_value_id2)
    REFERENCES shop_property_value(id)
    ON UPDATE CASCADE ON DELETE SET NULL
)
WITH (OIDS=FALSE);

CREATE INDEX fki_shop_sku_shop_product_id on shop_sku (shop_product_id);
CREATE INDEX fki_shop_sku_shop_property_value_id1 on shop_sku (shop_property_value_id1);
CREATE INDEX fki_shop_sku_shop_property_value_id2 on shop_sku (shop_property_value_id2);
CREATE INDEX shop_sku_article_nr ON shop_sku (article_nr);
CREATE INDEX shop_sku_imported ON shop_sku (imported);



-- An order by a person, an order has multiple order lines
CREATE TABLE shop_order
(
  id serial NOT NULL,
  status character varying(20) NOT NULL DEFAULT '',
  person_id bigint,
  created timestamp with time zone NOT NULL DEFAULT now(),
  modified timestamp with time zone NOT NULL DEFAULT now(),
  CONSTRAINT shop_order_pkey PRIMARY KEY (id),
  CONSTRAINT fk_shop_order_person_id FOREIGN KEY (person_id)
    REFERENCES person(id)
    ON UPDATE CASCADE ON DELETE RESTRICT
)
WITH (OIDS=FALSE);

CREATE INDEX fki_shop_order_person_id ON shop_order (person_id);


-- Order lines, also used to calculate the amount of reserved skus
CREATE TABLE shop_order_line
(
  id serial NOT NULL,
  status character varying(20) NOT NULL DEFAULT '',
  shop_order_id integer NOT NULL,
  shop_sku_id integer NOT NULL,
  quantity integer NOT NULL DEFAULT 1,
  price_total_incl integer NOT NULL DEFAULT 0,
  price_total_excl integer NOT NULL DEFAULT 0,
  created timestamp with time zone NOT NULL DEFAULT now(),
  CONSTRAINT shop_order_line_pkey PRIMARY KEY (id),
  CONSTRAINT fk_shop_order_line_shop_sku_id FOREIGN KEY (shop_sku_id)
    REFERENCES shop_sku (id)
    ON UPDATE CASCADE ON DELETE RESTRICT,
  CONSTRAINT fk_shop_order_line_shop_order_id FOREIGN KEY (shop_order_id)
    REFERENCES shop_order (id)
    ON UPDATE CASCADE ON DELETE CASCADE
)
WITH (OIDS=FALSE);

CREATE INDEX fki_shop_order_line_shop_sku_id ON shop_order_line(shop_sku_id);
CREATE INDEX fki_shop_order_line_shop_order_id ON shop_order_line(shop_order_id);

-- Shopping cart, a product in a cart of a person
CREATE TABLE shop_cart
(
  id serial NOT NULL,
  person_id bigint NOT NULL,
  quantity integer NOT NULL DEFAULT 1,
  shop_product_id int NOT NULL,
  shop_property_id1 integer,
  shop_property_id2 integer,
  created timestamp with time zone NOT NULL DEFAULT now(),
  CONSTRAINT shop_cart_pkey PRIMARY KEY (id),
  CONSTRAINT fk_shop_cart_person_id FOREIGN KEY (person_id)
    REFERENCES person(id)
    ON UPDATE CASCADE ON DELETE RESTRICT
  CONSTRAINT fk_shop_cart_shop_product_id FOREIGN KEY (shop_product_id)
    REFERENCES shop_product(id)
    ON UPDATE CASCADE ON DELETE RESTRICT
  CONSTRAINT fk_shop_cart_shop_property_value_id1 FOREIGN KEY (shop_property_value_id1)
    REFERENCES shop_property_value(id)
    ON UPDATE CASCADE ON DELETE SET NULL,
  CONSTRAINT fk_shop_cart_shop_property_value_id2 FOREIGN KEY (shop_property_value_id2)
    REFERENCES shop_property_value(id)
    ON UPDATE CASCADE ON DELETE SET NULL
)
WITH (OIDS=FALSE);

CREATE INDEX fki_shop_cart_shop_product_id ON shop_cart(shop_product_id);
CREATE INDEX fki_shop_cart_person_id ON shop_cart(person_id);
CREATE INDEX fki_shop_cart_shop_property_value_id1 on shop_cart(shop_property_value_id1);
CREATE INDEX fki_shop_cart_shop_property_value_id2 on shop_cart(shop_property_value_id2);
CREATE INDEX shop_cart_index ON shop_cart(person_id,created);


-- Simple log, so that we can see what happened when
CREATE TABLE shop_log
(
  id bigserial NOT NULL,
  message character varying(2000) NOT NULL DEFAULT ''::character varying,
  "date" timestamp with time zone NOT NULL DEFAULT now(),
  ip inet,
  CONSTRAINT shop_log_pkey PRIMARY KEY (id)  
)
WITH (OIDS=FALSE);
