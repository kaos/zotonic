%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-11
%%
%% @doc 

-module(shop_install_data).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    install/1,
    install_cat/1,
    install_rsc/1
]).

-include_lib("zophrenic.hrl").

install(Context) ->
    install_cat(Context),
    install_rsc(Context),
    ok.

install_cat(Context) ->
    Cats = [
        [ {name, bikes},         {title, "Fietsen"}, {body, "<p>Verhaaltje waarom je fietsen bij ons niet online kan kopen enzo...</p>"} ],
        [ {name, 'clothing-shoes'}, {title, "Kleding en schoenen"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, accessories},   {title, "Accessoires"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, parts},         {title, "Onderdelen"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],

        [ {name, bags},          {title, "Tassen"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, glasses},       {title, "Brillen"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, batteries},     {title, "Batterijen"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, bikecomputers}, {title, "Fietscomputers"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ],
        [ {name, montagestands}, {title, "Montage standaarden"}, {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"} ]
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
    
    F = fun(Ctx) ->
        [ ok = m_category:insert(Cat, Ctx) || Cat <- Cats ],
        [ m_category:update_parent(m_category:name_to_id(B,Ctx), m_category:name_to_id(A,Ctx), Ctx) || {A,B} <- Parents],
        m_category:renumber(Ctx)
    end,
    zp_db:transaction(F, Context).


install_rsc(Context) ->
    Rsc = [
        [
            {title, "Ortlieb Ultimate 5 Classic"},
            {slug, "ortlieb-ultimate-5-classic"},
            {price, 74.95},
            {category_id, m_category:name_to_id(bags, Context)},
            {group_id, 1},
            {product_nr, 735},
            {body, {trans, [{nl, "
<p>Landkaart, zonnebril, papieren of energierepen - aan het stuur heeft u alles binnen handbereik. De ULTIMATE5 modellen zijn van bijzonder duurzaam weefsel en wegen praktisch niets. Ze zijn even snel aangebracht als weer verwijderd en bewijzen hun diensten ook graag als schoudertas. Aan de fiets zijn ze betrouwbaar tegen diefstal beveiligd dankzij de afsluitbare montage aan het stuur. Een verlengadapter voor sterk gebogen sturen wordt meegeleverd (extra accessoire).</p>
<p>Altijd op koers blijven! Op de dekselsluiting van de ULTIMATE5 Plus en Classic kan een kaarttas of een GPS-hoesje worden aangebracht, beide zijn als toebehoren verkrijgbaar. De ORTLIEB stuurtassen zijn uiterst vormbestendig en duurzaam met verstevigd deksel en binnenversteviging. In maat M en L verkrijgbaar fascineren ze met hun veelzijdige functionaliteit zoals het geïntegreerde vakje voor waardevolle spullen. Met 3M-Scotchlite reflecterend materiaal zorgen de beide multitalenten ervoor, dat ze ook in het donker worden gezien. Ze zijn tot 3 kg belastbaar en voor sturen tot 31,8 mm diameter.</p>

<h3>ULTIMATE5 CLASSIC</h2>
<ul>
<li>Weefsel PD620/PS490</li>
<li>Uitneembare tussenschotten als toebehoren verkrijgbaar</li>
</ul>"}
            ]}}
        ],
        [
            {title, "Ortlieb Mud Racer XS"},
            {slug, "ortlieb-mud-racer-xs"},
            {price, 22.95},
            {category_id, m_category:name_to_id(bags, Context)},
            {group_id, 1},
            {product_nr, 1271},
            {intro, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit</p>"},
            {body, "<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"}
        ],
        [
            {title, "Rudy Project Jekyll"},
            {slug, "rudy-project-jekyll"},
            {price, 99.90},
            {category_id, m_category:name_to_id(glasses, Context)},
            {group_id, 1},
            {product_nr, 712}
        ],
        [
            {title, "Duracell AA Plus"},
            {slug, "duracell-aa-plus"},
            {price, 6.95},
            {category_id, m_category:name_to_id(batteries, Context)},
            {group_id, 1},
            {product_nr, 1610}
        ],
        [
            {title, "Cateye Strada (Draadloos)"},
            {slug, "cateye-strada-draadloos"},
            {price, 89.95},
            {category_id, m_category:name_to_id(bikecomputers, Context)},
            {group_id, 1},
            {product_nr, 1591},
            {body, "<p>Strak, ligt gewicht en elegant, Cateye perfectioneert het design van een fietscomputer met de Strada. Laat het slanke design u niet misleiden, de Strada heeft het grootste display en lettertype uit zijn klasse en alle functies die u nodig heeft. De Strada monteert handig en snel op praktische iedere fiets.</p>
            <p>De Strada is draadloos, met draad of met cadans meter verkrijgbaar.</p>

            <h3>Functies</h2>

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
            {price, 95.00},
            {category_id, m_category:name_to_id(montagestands, Context)},
            {group_id, 1},
            {product_nr, 1636},
            {body, "<p>Ideale montagestandaard voor de zwaardere werkzaamheden. Uitgevoerd met verplaatsbaar montageblad. De fiets wordt gefixeerd aan de voor- of achtervork en staat stevig op een kunststof bracketsteun. De vorkhouders zijn zowel in hoogte als in lengte verschuifbaar. Geschikt voor racefietsen en mountainbikes met een wieldiameter van 24 t/m 28 inch.</p>"}
        ]
    ],
    
    F = fun(Ctx) ->
        Rets = [ zp_db:insert(rsc, [{is_published, true}, {owner_id, 1}, {modifier_id, 1}, {creator_id, 1} | R], Ctx) || R <- Rsc ],
        IdRsc = lists:zip(Rets, Rsc),
        M = fun({{ok,Id}, R}) ->
            ProdNr = proplists:get_value(product_nr, R),
            File = "priv/files/archive/" ++ integer_to_list(ProdNr) ++ ".jpg",
            m_media:insert_file_rsc(File, Id, [], Ctx)
        end,
        [ M(IR) || IR <- IdRsc],
        ok
    end,
    
    ok = zp_db:transaction(F, Context).
    
    
    

