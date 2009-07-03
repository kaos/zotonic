%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-11
%%
%% @doc Install data and tables for the shop functionality.
%% @todo  order_log, order_status, order_line_status

-module(shop_install_data).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    install/1
]).

-include_lib("zophrenic.hrl").

install(Context) ->
    zp_acl:sudo(fun(Ctx) -> install1(Ctx) end, Context).

install1(Context) ->
    F = fun(Ctx) ->
        case m_predicate:name_to_id(brand, Ctx) of
            {ok, _Id} -> ok;
            _ ->  {ok, _Id} = m_predicate:insert("Brand", Ctx)
        end,
        ok = install_tables(Ctx),
        ok = install_order_status(Ctx),
        ok = install_cat(Ctx),
        ok = install_rsc(Ctx),
        ok = install_sku(Ctx),
        shop_adyen:install(Ctx)
    end,

    ok = zp_db:transaction(F, Context),
    zp_depcache:flush(),
    ok.


install_tables(Context) ->
    [ [] = zp_db:q(Sql, Context) || Sql <- tables_sql() ],
    ok.


tables_sql() ->
    [
    % All Stock Keeping Units, one product can have one or more skus.  
    % Skus are attached to a product and have optional varieties
    "
    CREATE TABLE shop_sku
    (
      id serial NOT NULL,

      -- The product and the properties distinguishing this sku within the product
      rsc_id integer,
      medium_id integer,
      variant character varying(100) not null default '',
      props bytea,
      stock_avail integer not null default 0,       -- Imported stock, minus sold and reserved items
      is_special boolean not null default false,
      price_actual integer not null default 0,      -- For later use, when sorting on price

      available boolean NOT NULL DEFAULT TRUE,
      imported timestamp with time zone NOT NULL DEFAULT now(),
      created timestamp with time zone NOT NULL DEFAULT now(),
      modified timestamp with time zone NOT NULL DEFAULT now(),
      
      -- Field for full text search
      tsv tsvector,
      
      -- Imported from VMSII (VendIT)
      article_nr character varying(50) NOT NULL,
      upc character varying(20) NOT NULL DEFAULT ''::character varying,
      description1 character varying(200) NOT NULL DEFAULT ''::character varying,
      description2 character varying(200) NOT NULL DEFAULT ''::character varying,
      stock int not null default 0,
      price_incl integer NOT NULL DEFAULT 0,
      price_excl integer NOT NULL DEFAULT 0,
      special_price_incl integer NOT NULL DEFAULT 0,
      special_price_excl integer NOT NULL DEFAULT 0,
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

      -- 0 = 0%, 1 = 6%, 2 = 19%
      vat_code character(1) default '2'::character,

      CONSTRAINT shop_sku_pkey PRIMARY KEY (id),
      CONSTRAINT shop_sku_article_nr_key UNIQUE (article_nr),
      CONSTRAINT fk_shop_sku_rsc_id FOREIGN KEY (rsc_id)
        REFERENCES rsc(id)
        ON UPDATE CASCADE ON DELETE SET NULL,
      CONSTRAINT fk_shop_sku_medium_id FOREIGN KEY (medium_id)
        REFERENCES medium(id)
        ON UPDATE CASCADE ON DELETE SET NULL
    )",

    "CREATE INDEX fki_shop_sku_rsc_id ON shop_sku (rsc_id)",
    "CREATE INDEX fki_shop_sku_medium_id ON shop_sku (medium_id)",
    "CREATE INDEX shop_sku_rsc_id_variant ON shop_sku (rsc_id, variant)",
    "CREATE INDEX shop_sku_imported ON shop_sku (imported)",

    "CREATE INDEX shop_sku_tsv_tsv ON shop_sku USING gin(tsv)",
    "CREATE FUNCTION shop_sku_trigger() RETURNS trigger AS $$ 
    begin
      new.tsv := to_tsvector(new.article_nr) ||
                 to_tsvector(new.upc) ||
                 to_tsvector(new.description1) ||
                 to_tsvector(new.description2) ||
                 to_tsvector(new.brand) ||
                 to_tsvector(new.product_group) ||
                 to_tsvector(new.extra1) ||
                 to_tsvector(new.extra2) ||
                 to_tsvector(new.extra3) ||
                 to_tsvector(new.extra4) ||
                 to_tsvector(new.extra5) ||
                 to_tsvector(new.extra6) ||
                 to_tsvector(new.variant);
      return new; 
    end 
    $$ LANGUAGE plpgsql",
    
    "CREATE TRIGGER tsvectorupdate_shop_sku BEFORE INSERT OR UPDATE 
    ON shop_sku FOR EACH ROW EXECUTE PROCEDURE shop_sku_trigger()",

    % Valid status for the shop_order
    % new, payment_requested, payment_authorized, payment_refused, payment_pending, payment_error, processed, canceled
    "
    CREATE TABLE shop_order_status
    (
        status character varying(20) not null,
        CONSTRAINT shop_order_status_pkey PRIMARY KEY(status)
    )
    ",
    
    % An order by a person, an order has multiple order lines
    "
    CREATE TABLE shop_order
    (
      id serial NOT NULL,
      name character varying(32),
      visitor_id bigint,

      status character varying(20) NOT NULL DEFAULT '',
      status_modified timestamp with time zone NOT NULL DEFAULT now(),
      expires timestamp with time zone,
      
      total_price_incl integer NOT NULL DEFAULT 0,
      total_price_excl integer NOT NULL DEFAULT 0,
      payment_method character varying(20),
      paid boolean not null default false,
      
      first_name character varying(50) not null default '',
      lastname_prefix character varying(20) not null default '',
      lastname character varying(100) not null default '',
      email character varying(100) not null default '',
      phone character varying(40) not null default '',
      
      attn character varying(100) not null default '',
      street character varying(100) not null default '',
      postcode character varying(20) not null default '',
      city character varying(100) not null default '',
      state character varying(100) not null default '',
      country character varying(50) not null default '',

      delivery_attn character varying(100) not null default'',
      delivery_street character varying(100) not null default '',
      delivery_postcode character varying(20) not null default '',
      delivery_city character varying(100) not null default '',
      delivery_state character varying(100) not null default '',
      delivery_country character varying(50) not null default '',
      
      created timestamp with time zone NOT NULL DEFAULT now(),
      modified timestamp with time zone NOT NULL DEFAULT now(),
      CONSTRAINT shop_order_pkey PRIMARY KEY (id),
      CONSTRAINT shop_order_name_key UNIQUE(name),
      CONSTRAINT fk_shop_order_visitor_id FOREIGN KEY (visitor_id)
        REFERENCES visitor(id)
        ON UPDATE CASCADE ON DELETE RESTRICT,   
      CONSTRAINT fk_shop_order_status FOREIGN KEY (status)
        REFERENCES shop_order_status(status)
        ON UPDATE CASCADE ON DELETE RESTRICT        
    )
    ",

    "CREATE INDEX shop_order_visitor_id ON shop_order (visitor_id)",
    "CREATE INDEX shop_order_created_key ON shop_order (created)",
    "CREATE INDEX shop_order_status_key ON shop_order (status, status_modified)",


    % Order lines, also used to calculate the amount of reserved skus
    "
    CREATE TABLE shop_order_line
    (
      id serial NOT NULL,
      shop_order_id integer NOT NULL,
      shop_sku_id integer NOT NULL,
      quantity integer NOT NULL,
      allocated integer NOT NULL,
      backorder integer NOT NULL,
      price_incl integer NOT NULL DEFAULT 0,
      price_excl integer NOT NULL DEFAULT 0,
      created timestamp with time zone NOT NULL DEFAULT now(),
      modified timestamp with time zone NOT NULL DEFAULT now(),
      CONSTRAINT shop_order_line_pkey PRIMARY KEY (id),
      CONSTRAINT fk_shop_order_line_shop_sku_id FOREIGN KEY (shop_sku_id)
        REFERENCES shop_sku (id)
        ON UPDATE CASCADE ON DELETE RESTRICT,
      CONSTRAINT fk_shop_order_line_shop_order_id FOREIGN KEY (shop_order_id)
        REFERENCES shop_order (id)
        ON UPDATE CASCADE ON DELETE RESTRICT
    )
    ",

    "CREATE INDEX fki_shop_order_line_shop_sku_id ON shop_order_line(shop_sku_id)",
    "CREATE INDEX fki_shop_order_line_shop_order_id ON shop_order_line(shop_order_id)",
    
    % Logging table for notification received from the payment service provider Adyen
    "
    CREATE TABLE shop_adyen_log
    (
      id serial NOT NULL,
      handled boolean not null default false,
      shop_order_id integer,
      live boolean not null,
      order_status_old character varying(20) not null default '',
      order_status_new character varying(20) not null default '',
      
      event_code character varying(50) not null default '',
      psp_reference character varying(50) not null default '',
      original_reference character varying(50) not null default '',
      merchant_account_code character varying(50) not null default '',
      event_date character varying(50) not null default '',
      success boolean not null,
      payment_method character varying(50) not null default '',
      operations character varying(100) not null default '',
      reason character varying(100) not null default '',
      currency character varying(10) not null default '',
      value integer,
      request bytea,
      created timestamp with time zone NOT NULL DEFAULT now(),
      CONSTRAINT shop_adyen_log_pkey PRIMARY KEY (id)
    )
    ",

    "CREATE INDEX shop_adyen_log_created_key ON shop_adyen_log(created)",
    "CREATE INDEX shop_adyen_log_psp_reference_key ON shop_adyen_log(psp_reference, event_code)",
    "CREATE INDEX shop_adyen_log_order_id_key ON shop_adyen_log(shop_order_id, created)",
    "CREATE INDEX shop_adyen_log_handled_created_key ON shop_adyen_log(handled, created)",

    % Statistics about which products are sold together
    % Every row is twice present, to facilitate easy querying
    "
    CREATE TABLE shop_combo
    (
        a_shop_sku_id int not null,
        b_shop_sku_id int not null,
        count int not null default 1,
        CONSTRAINT shop_combo_pkey PRIMARY KEY (a_shop_sku_id, b_shop_sku_id),
        CONSTRAINT fk_shop_combo_a_shop_sku_id FOREIGN KEY (a_shop_sku_id)
            REFERENCES shop_sku(id)
            ON UPDATE CASCADE ON DELETE CASCADE,
        CONSTRAINT fk_shop_combo_b_shop_sku_id FOREIGN KEY (b_shop_sku_id)
            REFERENCES shop_sku(id)
            ON UPDATE CASCADE ON DELETE CASCADE
    )
    ",
    
    "CREATE INDEX fki_shop_combo_a_shop_sku_id ON shop_combo(a_shop_sku_id)",
    "CREATE INDEX fki_shop_combo_b_shop_sku_id ON shop_combo(b_shop_sku_id)",
    "CREATE INDEX shop_combo_a_count_key ON shop_combo(a_shop_sku_id, count)"

    ].



install_cat(Context) ->
    Cats = [
        [ {name, bikes},         {title, "Fietsen"}, {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"}, {body, "<p>Verhaaltje waarom je fietsen bij ons niet online kan kopen enzo...</p>"}],
        [ {name, 'clothing-shoes'}, {title, "Kleding en schoenen"}, {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, accessories},   {title, "Accessoires"}, {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit>"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, parts},         {title, "Onderdelen"}, {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],

        [ {name, bags},          {title, "Tassen"}, {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, glasses},       {title, "Brillen"}, {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, batteries},     {title, "Batterijen"}, {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, bikecomputers}, {title, "Fietscomputers"}, {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, montagestands}, {title, "Montage standaarden"}, {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],

        [ {name, brand}, {title, "Brand"} ]
    ],

    Parents = [
        {product, bikes},
        {product, 'clothing-shoes'},
        {product, accessories},
        {product, parts},

        {accessories, bags},
        {accessories, glasses},
        {accessories, batteries},
        {accessories, bikecomputers},
        {accessories, montagestands}
    ],
    
    [ {ok, _} = m_category:insert(Cat, Context) || Cat <- Cats ],
    [ m_category:update_parent(m_category:name_to_id_check(B,Context), m_category:name_to_id_check(A,Context), Context) || {A,B} <- Parents],
    m_category:renumber(Context),
    ok.



install_rsc(Context) ->
    Rsc = [
        [
            {title, "Ortlieb Ultimate 5 Classic"},
            {slug, "ortlieb-ultimate-5-classic"},
            {category_id, m_category:name_to_id_check(bags, Context)},
            {product_nr, 735},
            {name, "product_735"},
            {intro, "Spatwaterdichte stuurtas met verstevigde klepsluiting."},
            {body, {trans, [{nl, "
<p>Spatwaterdichte stuurtas met verstevigde klepsluiting. Afsluitbare montage aan het stuur (buisdiameter maximaal 31,8 mm). Landkaart, zonnebril, papieren of energierepen, aan het stuur heeft u alles binnen handbereik. De Ultimate5 modellen zijn van duurzaam weefsel. </p>
<p>Makkelijk te bevestigen aan het stuur en ook te gebruiken als schoudertas. Aan de fiets zijn ze tegen diefstal beveiligd. Een verlengadapter voor sterk gebogen sturen wordt meegeleverd (extra accessoire).</p>
<p>Afmetingen (HxBxD): 24×25×18 cm. Inhoud: 7 liter. Gewicht: 650gr.</p>"}
            ]}}
        ],
        [
            {title, "Ortlieb Mud Racer XS"},
            {slug, "ortlieb-mud-racer-xs"},
            {category_id, m_category:name_to_id_check(bags, Context)},
            {product_nr, 1271},
            {name, "product_1271"},
            {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"},
            {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"}
        ],
        [
            {title, "Rudy Project Jekyll"},
            {slug, "rudy-project-jekyll"},
            {category_id, m_category:name_to_id_check(glasses, Context)},
            {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"},
            {product_nr, 712},
            {name, "product_712"}
        ],
        [
            {title, "Duracell AA Plus"},
            {slug, "duracell-aa-plus"},
            {category_id, m_category:name_to_id_check(batteries, Context)},
			{intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"},
            {product_nr, 1610},
            {name, "product_1610"}
        ],
        [
            {title, "Cateye Strada (Draadloos)"},
            {slug, "cateye-strada-draadloos"},
            {category_id, m_category:name_to_id_check(bikecomputers, Context)},
            {product_nr, 1591},
            {name, "product_1591"},
            {intro, "Cateye Strada is de perfecte fietscomputer."},
            {body, "<p>Cateye fietscomputers vind je op flink wat tweewielers terug en dat is geen toeval. Ze zijn bekend om hun gebruiksgemak en dat telt voor een fietscomputer. 

            <p>Benelux-distributeur Juncker stelde ons de nieuwe Strada Wireless ter beschikking om onze trainingskilometers om te zetten in digitale informatie.</p>
            <p>Via een handige, herbruikbare plastic ring die met een stelwieltje om het stuur of de stuurpen wordt geklemd, zet je de Strada Wireless waar je hem het liefste hebt. In ons geval op de stuurpen, want zo blijft de volledige bovenkant van het stuur vrij om over de kasseien te dokkeren. </p>
            <p>De cijfers op het scherm zijn niet kolossaal groot, maar wel voldoende duidelijk.</p>
            <p>Enig minpuntje aan de Strada Wireless is dat voor de instelling van de klok (wat we dankzij de zomer- en wintertijd 2 keer per jaar mogen doen) alle data opnieuw moeten worden ingevoerd.</p>"}
        ],
        [
            {title, "Tacx Cycle Motion Stand"},
            {slug, "tacx-cycle-motion-stand"},
            {category_id, m_category:name_to_id_check(montagestands, Context)},
            {product_nr, 1636},
            {name, "product_1636"},
            {intro, "Ideale montagestandaard voor zware werkzaamheden."},
            {body, "De Cycle Motion Stand is stabiele standaard voor de wat zwaardere werkzaamheden zoals het verwijderen van crankstel of pedalen. De fiets wordt bevestigd aan de voor- of achtervork. De vorkhouders zijn zowel in hoogte als
            in lengte verschuifbaar waardoor ATB's, race- en hybride fietsen met een wielmaat van 24 tot 28 inch kunnen worden geplaatst. Uitgevoerd met een verplaatsbaar montageblad. Optie: Exact wielrichter."}
        ],
        
        % Some brands
        [
            {title, "Tacx"},
            {category_id, m_category:name_to_id_check(brand, Context)},
            {name, "tacx"},
            {body, "<p>Tacx is Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"}
        ],
        [
            {title, "Cateye"},
            {category_id, m_category:name_to_id_check(brand, Context)},
            {name, "cateye"},
            {body, "<p>Cateye is Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"}
        ],
        [
            {title, "Rudy"},
            {category_id, m_category:name_to_id_check(brand, Context)},
            {name, "rudy"},
            {body, "<p>Rudy is Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"}
        ],
        [
            {title, "Duracell"},
            {category_id, m_category:name_to_id_check(brand, Context)},
            {name, "duracell"},
            {body, "<p>Duracell is Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"}
        ],
        [
            {title, "Ortlieb"},
            {category_id, m_category:name_to_id_check(brand, Context)},
            {name, "ortlieb"},
            {body, "<p>Ortlieb is Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"}
        ]
    ],
    
    BrandEdges = [
        {"product_735", "ortlieb"},
        {"product_1271", "ortlieb"},
        {"product_712", "rudy"},
        {"product_1610", "duracell"},
        {"product_1591", "cateye"},
        {"product_1636", "tacx"}
    ],

    Rets = [ m_rsc:insert([{is_published, true}, {visible_for, 0}, {group, admins} | R], Context) || R <- Rsc ],
    IdRsc = lists:zip(Rets, Rsc),
    M = fun({{ok,Id}, R}) ->
        case proplists:get_value(product_nr, R) of
            undefined -> ok;
            ProdNr ->
                File = filename:join([code:lib_dir(zophrenic, priv), "sites", "default", "files", "archive",  integer_to_list(ProdNr) ++ ".jpg"]),
                {ok, FileRscId} = m_media:insert_file(File, [{visible_for, 0}, {group, admins}, {creator_id, 1}], Context),
                m_edge:insert(Id, depiction, FileRscId, Context)
        end
    end,
    [ M(IR) || IR <- IdRsc],

    BrandPred = zp_db:q1("select id from rsc where name = 'brand'", Context),
    [ m_edge:insert(m_rsc:name_to_id_check(S, Context), 
                    BrandPred, 
                    m_rsc:name_to_id_check(O, Context), 
                    Context) || {S,O} <- BrandEdges ],
    ok.




install_sku(Context) ->
    Skus = [
        [
            {rsc_id, m_rsc:name_to_id_check("product-735", Context)},
            {stock_avail, 5},
            {article_nr, "prod0735"},
            {description1, "ULTIMATE5 CLASSIC"},
            {stock, 5},
            {price_incl, 7495},
            {price_excl, 6298}
        ],
        [
            {rsc_id, m_rsc:name_to_id_check("product-1271", Context)},
            {stock_avail, 4},
            {article_nr, "prod1271p"},
            {description1, "Ortlieb Mud Racer XS"},
            {variant, "xspink"},
            {title, "Pink Flower Edition"},
            {stock, 4},
            {price_incl, 2495},
            {price_excl, 2097}
        ],
        [
            {rsc_id, m_rsc:name_to_id_check("product-1271", Context)},
            {stock_avail, 4},
            {article_nr, "prod1271b"},
            {description1, "Ortlieb Mud Racer XS"},
            {title, "Deep Night Black"},
            {variant, "xsblack"},
            {stock, 4},
            {price_incl, 2295},
            {price_excl, 1929}
        ],
        [
            {rsc_id, m_rsc:name_to_id_check("product-712", Context)},
            {stock_avail, 4},
            {article_nr, "prod0712"},
            {description1, "Rudy Project Jekyll"},
            {stock, 4},
            {price_incl, 9990},
            {price_excl, 8395}
        ],
        [
            {rsc_id, m_rsc:name_to_id_check("product-1610", Context)},
            {stock_avail, 100},
            {article_nr, "prod1610"},
            {description1, "Duracell AA Plus"},
            {stock, 100},
            {price_incl, 695},
            {price_excl, 584}
        ],
        [
            {rsc_id, m_rsc:name_to_id_check("product-1591", Context)},
            {stock_avail, 10},
            {article_nr, "prod1591"},
            {description1, "Cateye Strada (Draadloos)"},
            {stock, 10},
            {price_incl, 8995},
            {price_excl, 7559},
            {special_price_incl, 6995},
            {special_price_excl, 5878},
            {special_start, {2009,4,1}},
            {special_end, {2009,6,1}}
        ],
        [
            {rsc_id, m_rsc:name_to_id_check("product-1636", Context)},
            {stock_avail, 100},
            {article_nr, "prod1636"},
            {description1, "Tacx Cycle Motion Stand"},
            {stock, 100},
            {price_incl, 8995},
            {price_excl, 7559}
        ]
    ],
    
    [ {ok, _} = zp_db:insert(shop_sku, Sku, Context) || Sku <- Skus ],
    ok.


install_order_status(Context) ->
    zp_db:q("
        insert into shop_order_status (status) values
            ('new'),
            ('payment_requested'),
            ('payment_authorized'),
            ('payment_refused'),
            ('payment_pending'),
            ('payment_error'),
            ('processed'),
            ('canceled')
        ", Context),
    ok.
