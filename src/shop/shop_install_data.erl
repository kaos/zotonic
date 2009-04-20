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
    F = fun(Ctx) ->
        ok = install_tables(Ctx),
        ok = install_order_status(Ctx),
        ok = install_cat(Ctx),
        ok = install_pred(Ctx),
        ok = install_rsc(Ctx),
        ok = install_sku(Ctx)
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

      variant character varying(80) not null default '',

      stock_avail int not null default 0,
      is_special boolean not null default false,
      
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
        ON UPDATE CASCADE ON DELETE SET NULL
    )",

    "CREATE INDEX fki_shop_sku_rsc_id ON shop_sku (rsc_id)",
    "CREATE INDEX shop_sku_rsc_id_variant ON shop_sku (rsc_id, variant)",
    "CREATE INDEX shop_sku_article_nr ON shop_sku (article_nr)",
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
    % checkout, canceled, timeout, mailed, delivered
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
      
      delivery_costs integer NOT NULL DEFAULT 0,
      total_price integer NOT NULL DEFAULT 0,
      payment_method character varying(20),

      first_name character varying(50) not null default '',
      lastname_prefix character varying(20) not null default '',
      lastname character varying(100) not null default '',
      email character varying(100) not null default '',
      phone character varying(40) not null default '',
      
      street character varying(100) not null default '',
      postcode character varying(20) not null default '',
      city character varying(100) not null default '',
      state character varying(100) not null default '',
      country character varying(50) not null default '',

      has_delivery_address boolean not null default false,
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
      CONSTRAINT fk_shop_order_person_id FOREIGN KEY (visitor_id)
        REFERENCES visitor(id)
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
    
    % Logging table for orders
    "
    CREATE TABLE shop_log
    (
      id serial NOT NULL,
      shop_order_id integer,
      visitor_id bigint,
      user_id int,
      action character varying(20) not null default ''::character varying,
      message character varying(500) not null default ''::character varying,
      ip_address character varying(40),
      user_agent character varying(128),
      created timestamp with time zone NOT NULL DEFAULT now(),
      CONSTRAINT shop_log_pkey PRIMARY KEY (id)
    )
    ",

    "CREATE INDEX shop_log_created_key ON shop_log(created)",
    "CREATE INDEX shop_log_order_id_key ON shop_log(shop_order_id, created)",
    "CREATE INDEX shop_log_visitor_id_key ON shop_log(visitor_id, created)",

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


install_order_status(Context) ->
    % checkout, canceled, timeout, mailed, delivered
    Status = [
        [{status, "checkout"}],
        [{status, "canceled"}],
        [{status, "timeout"}],
        [{status, "mailed"}],
        [{status, "delivered"}]
    ],
    [ {ok, _} = zp_db:insert(shop_order_status, S, Context) || S <- Status ],
    ok.


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


install_pred(Context) ->
    Preds = [
        [ {name, brand}, {title, "Brand"}, {uri, "http://zophrenic.com/predicate/brand"} ]
    ],
    
    [ {ok, _} = m_predicate:insert(P, Context) || P <- Preds ],
    ok.


install_rsc(Context) ->
    Rsc = [
        [
            {title, "Ortlieb Ultimate 5 Classic"},
            {slug, "ortlieb-ultimate-5-classic"},
            {price, 7495},
            {category_id, m_category:name_to_id_check(bags, Context)},
            {product_nr, 735},
            {name, "product-735"},
            {intro, "De ULTIMATE5 modellen zijn van bijzonder duurzaam weefsel en wegen praktisch niets."},
            {body, {trans, [{nl, "
<p>Landkaart, zonnebril, papieren of energierepen - aan het stuur heeft u alles binnen handbereik. De ULTIMATE5 modellen zijn van bijzonder duurzaam weefsel en wegen praktisch niets. Ze zijn even snel aangebracht als weer verwijderd en bewijzen hun diensten ook graag als schoudertas. Aan de fiets zijn ze betrouwbaar tegen diefstal beveiligd dankzij de afsluitbare montage aan het stuur. Een verlengadapter voor sterk gebogen sturen wordt meegeleverd (extra accessoire).</p>
<p>Altijd op koers blijven! Op de dekselsluiting van de ULTIMATE5 Plus en Classic kan een kaarttas of een GPS-hoesje worden aangebracht, beide zijn als toebehoren verkrijgbaar. De ORTLIEB stuurtassen zijn uiterst vormbestendig en duurzaam met verstevigd deksel en binnenversteviging. In maat M en L verkrijgbaar fascineren ze met hun veelzijdige functionaliteit zoals het geïntegreerde vakje voor waardevolle spullen. Met 3M-Scotchlite reflecterend materiaal zorgen de beide multitalenten ervoor, dat ze ook in het donker worden gezien. Ze zijn tot 3 kg belastbaar en voor sturen tot 31,8 mm diameter.</p>

<h3>ULTIMATE5 CLASSIC</h3>
<ul>
<li>Weefsel PD620/PS490</li>
<li>Uitneembare tussenschotten als toebehoren verkrijgbaar</li>
</ul>"}
            ]}}
        ],
        [
            {title, "Ortlieb Mud Racer XS"},
            {slug, "ortlieb-mud-racer-xs"},
            {price, 2295},
            {category_id, m_category:name_to_id_check(bags, Context)},
            {product_nr, 1271},
            {name, "product-1271"},
            {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"},
            {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"}
        ],
        [
            {title, "Rudy Project Jekyll"},
            {slug, "rudy-project-jekyll"},
            {price, 9990},
            {category_id, m_category:name_to_id_check(glasses, Context)},
            {intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"},
            {product_nr, 712},
            {name, "product-712"}
        ],
        [
            {title, "Duracell AA Plus"},
            {slug, "duracell-aa-plus"},
            {price, 695},
            {category_id, m_category:name_to_id_check(batteries, Context)},
			{intro, "Lorem ipsum dolor sit amet, consectetur adipisicing elit"},
            {product_nr, 1610},
            {name, "product-1610"}
        ],
        [
            {title, "Cateye Strada (Draadloos)"},
            {slug, "cateye-strada-draadloos"},
            {price, 8995},
            {special_price, 6995},
            {special_start, {2009,4,1}},
            {special_end, {2009,6,1}},
            {category_id, m_category:name_to_id_check(bikecomputers, Context)},
            {product_nr, 1591},
            {name, "product-1591"},
            {intro, "Strak, ligt gewicht en elegant, Cateye perfectioneert het design van een fietscomputer met de Strada."},
            {body, "<p>Strak, ligt gewicht en elegant, Cateye perfectioneert het design van een fietscomputer met de Strada. Laat het slanke design u niet misleiden, de Strada heeft het grootste display en lettertype uit zijn klasse en alle functies die u nodig heeft. De Strada monteert handig en snel op praktische iedere fiets.</p>
            <p>De Strada is draadloos, met draad of met cadans meter verkrijgbaar.</p>

            <h3>Functies</h3>

            <ul>
            <li>Actuele snelheid</li>
            <li>Maximum snelheid</li>
            <li>Gemiddelde snelheid</li>
            <li>Odometer (Totaal afstand)</li>
            <li>Dag afstand teller</li>
            <li>Tijd over dag afstand</li>
            <li>Dag afstand teller 2</li>
            <li>Klok</li>
            </ul>"}
        ],
        [
            {title, "Tacx Cycle Motion Stand"},
            {slug, "tacx-cycle-motion-stand"},
            {price, 9500},
            {category_id, m_category:name_to_id_check(montagestands, Context)},
            {product_nr, 1636},
            {name, "product-1636"},
            {intro, "Ideale montagestandaard voor de zwaardere werkzaamheden."},
            {body, "<p>Ideale montagestandaard voor de zwaardere werkzaamheden. Uitgevoerd met verplaatsbaar montageblad. De fiets wordt gefixeerd aan de voor- of achtervork en staat stevig op een kunststof bracketsteun. De vorkhouders zijn zowel in hoogte als in lengte verschuifbaar. Geschikt voor racefietsen en mountainbikes met een wieldiameter van 24 t/m 28 inch.</p>"}
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
        {"product-735", "ortlieb"},
        {"product-1271", "ortlieb"},
        {"product-712", "rudy"},
        {"product-1610", "duracell"},
        {"product-1591", "cateye"},
        {"product-1636", "tacx"}
    ],
    
    Rets = [ zp_db:insert(rsc, [{is_published, true}, {visible_for, 0}, {group_id, 1}, {modifier_id, 1}, {creator_id, 1} | R], Context) || R <- Rsc ],
    IdRsc = lists:zip(Rets, Rsc),
    M = fun({{ok,Id}, R}) ->
        case proplists:get_value(product_nr, R) of
            undefined -> ok;
            ProdNr ->
                File = "priv/files/archive/" ++ integer_to_list(ProdNr) ++ ".jpg",
                m_media:insert_file_rsc(File, Id, [], Context)
        end
    end,
    [ M(IR) || IR <- IdRsc],

    BrandPred = m_predicate:name_to_id_check("brand", Context),
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
            {article_nr, "prod1271"},
            {description1, "Ortlieb Mud Racer XS"},
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
            {stock_avail, 100},
            {article_nr, "prod1591"},
            {description1, "Cateye Strada (Draadloos)"},
            {stock, 100},
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

