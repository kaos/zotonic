%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-04
%%
%% @doc Dummy data for developing the resource database layer

-module(dummy_data).
-author("Marc Worrell <marc@worrell.nl").

-include_lib("zophrenic.hrl").

%% interface functions
-export([
    rsc/1,
    media/1,
    o/1,
    s/1
]).


rsc(1) ->
    [
    {id, 1},
    {title, <<"Administrator">>}
    ];

rsc(2) ->
    [
    {id, 2},
    {uri, "/product/2/shimano-dura-ace-chain-cn7900"},
    {slug, "shimano-dura-ace-chain-cn7900"},
    {is_authoritative, true},
    {is_published, true},
    {visible_for, 0},
    {publication_start, {{2009,1,1},{0,0,0}}},
    {publication_end, {{5000,1,1},{0,0,0}}},
    
    {title, {trans, [
        {nl,<<"Shimano Dura-Ace Ketting CN7900">>},
        {en,<<"Shimano Dura-Ace Chain CN7900">>}
        ]}},
    {body, {trans, [
        {nl, <<"De nieuwe DURA-ACE. Een geperfectioneerde legende. De ideale balans tussen lichtgewicht, stijfheid en prestatie. Eindelijk verkrijgbaar om het professionele fietsen naar een nóg hoger niveau te tillen.">>},
        {en, <<"The DURA-ACE 7900 series sets a new benchmark in top-level road racing components. Shimano has combined all its engineering resources with the latest technology to realize an increasing level of perfection that will">>}
        ]}},
    
    {price, 49},
    {stock, 10}
    ];
    
rsc(3) ->
    [
    {id, 3},
    {title, <<"Shimano">>}
    ];
rsc(_) -> [].


o(2) ->
    [
        {brand, [#rsc{id=3}]}
    ];
o(_) -> [].

s(3) ->
    [
        {brand, [#rsc{id=2}]}
    ];
s(_) -> [].


media(1) ->
    [];
    
media(2) ->
    [
        [
            {filename, "handgrepen1.jpg"}
        ],

        [
            {filename, "handgrepen2.jpg"}
        ]
    ].
