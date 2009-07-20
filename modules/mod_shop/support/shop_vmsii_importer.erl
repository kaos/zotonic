%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Handle the upload of VendIT VMSII ARTIKEL.TXT and VOORRAAD.TXT files.
%% These files contain the complete dump of all SKUs from the administration system.
%% They are loaded into the sku table, non mentioned skus are set to unavailable.

-module(shop_vmsii_importer).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    test/0, 
    test1/0
]).

-include_lib("zotonic.hrl").

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the dropbox server
start_link() -> 
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(_Args) ->
    z_notifier:observe(dropbox_file, self()),
    {ok, []}.

%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.

%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling dropbox notifications
handle_info({dropbox_file, File}, State) ->
    Basename = string:to_lower(filename:basename(File)),
    case Basename of
        "artikel.txt" ->  
            do_import_skus(File),
            file:delete(File);
        "voorraad.txt" -> 
            do_import_stock(File),
            file:delete(File);
        _ -> ok
    end,
    {noreply, State};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


%% The standard fields in the export file are:
%% ARTIKELNUMMER^ 
%% OMSCHRIJVING1^ 
%% OMSCHRIJVING2^ 
%% VERKOOPIN^ 
%% VERKOOPEX^ 
%% AKTIEPRYS^ 
%% GELDIGVAN^ 
%% GELDIGTOT^ 
%% MERK^ 
%% BESTANDSNAAM_PLAATJE^ 
%% PRODUCTGROEP^ 
%% EXTRA INFO REGEL1^  
%% EXTRA INFO REGEL 2^  
%% EXTRA INFO REGEL 3^  
%% EXTRA INFO REGEL 4^  
%% EXTRA INFO REGEL 5^  
%% EXTRA INFO REGEL 6^ 
%% VERWIJDERINGSBIJDRAGE


%% @spec do_import_skus(File) -> void()
%% @doc Import the file with all skus, the separator is here a $^ (don't ask me why :-s )
do_import_skus(File) ->
    case readfile(File, $^) of
        {ok, Bin} ->
            List = binary_to_list(Bin),
            CSV  = split_lines_fields(List, $^),
            Rows = sku_list_to_dbrows(CSV),
            Rows;
        {error, Reason} ->
            {error, Reason}
    end.


%% @spec do_import_stock(File) -> void()
%% @doc Import the file with all sku stock and prices, the separator is here a $; (don't ask me why :-s )
do_import_stock(File) ->
    case readfile(File, $;) of
        {ok, Bin} ->
            List = binary_to_list(Bin),
            CSV  = split_lines_fields(List, $;),
            Rows = stock_list_to_dbrows(CSV),
            Rows;
        {error, Reason} ->
            {error, Reason}
    end.


%% @doc Transform the sku list to database rows [(field, value)]
sku_list_to_dbrows(Skus) ->
    % Database fields for the rows
    DB = [
        article_nr,
        description1,
        description2,
        price_incl,
        price_excl,
        special_price_incl,
        special_start,
        special_end,
        brand,
        image_file,
        product_group,
        extra1,
        extra2,
        extra3,
        extra4,
        extra5,
        extra6,
        removal_tax
    ],
    Types = [
        binary,
        binary, %description1
        binary, %description2
        price,
        price,
        price,
        date,
        date,
        binary, % brand
        binary, % image_file
        binary, % product_group
        binary, %extra1
        binary, %extra2
        binary, %extra3
        binary, %extra4
        binary, %extra5
        binary, %extra6
        price
    ],
    % Normalize all fields, create records
    fields_to_types(Skus, DB, Types).


%% @doc Transform the stock list to database rows [(field, value)]
stock_list_to_dbrows([First|Rest]=Stock) ->
    % Check the first line, it might contain all field names
    {DB, Types,Stock1} = case First of
                            [[C|_]|_] when C >= $A andalso C =< $Z ->
                                Cols  = lists:map(fun string:to_lower/1, First),
                                {D,T} = stock_header(Cols, {[],[]}),
                                {D,T,Rest};
                            _ -> 
                                % ARTNR;VOORRAAD;VERKOOPIN;ACTIEPRIJS;ACTIEVAN;ACTIETOT
                                D = [
                                    article_nr,
                                    stock,
                                    price_incl,
                                    special_price_incl,
                                    special_start,
                                    special_end
                                ],
                                T = [
                                    binary,
                                    integer,
                                    price,
                                    price,
                                    date,
                                    date
                                ],
                                {D,T,Stock}
                          end,
    fields_to_types(Stock1, DB, Types).

    
%% @doc Map the received header to type/field lists
stock_header([], {DAcc,TAcc}) ->
    {lists:reverse(DAcc), lists:reverse(TAcc)}; 
stock_header(["artnr"|Rest], {DAcc,TAcc}) ->
    stock_header(Rest, {[article_nr|DAcc],[binary|TAcc]}); 
stock_header(["voorraad"|Rest], {DAcc,TAcc}) ->
    stock_header(Rest, {[stock|DAcc],[integer|TAcc]}); 
stock_header(["verkoopin"|Rest], {DAcc,TAcc}) ->
    stock_header(Rest, {[price_incl|DAcc],[price|TAcc]}); 
stock_header(["verkoopex"|Rest], {DAcc,TAcc}) ->
    stock_header(Rest, {[price_excl|DAcc],[price|TAcc]}); 
stock_header(["actieprijs"|Rest], {DAcc,TAcc}) ->
    stock_header(Rest, {[special_price_incl|DAcc],[price|TAcc]}); 
stock_header(["actievan"|Rest], {DAcc,TAcc}) ->
    stock_header(Rest, {[special_start|DAcc],[date|TAcc]}); 
stock_header(["actietot"|Rest], {DAcc,TAcc}) ->
    stock_header(Rest, {[special_end|DAcc],[date|TAcc]}).

    
%% @spec fields_to_types(List, DBFields, Types) -> List
%% @doc Transform all data to the correct type as mentioned in the list
fields_to_types(Skus, DB, Types) ->
    fields_to_types(Skus, DB, Types, []).

fields_to_types([], _DB, _Types, Acc) ->
    lists:reverse(Acc);
fields_to_types([[]|Rest], DB, Types, Acc) ->
    fields_to_types(Rest, DB, Types, Acc);
fields_to_types([Row|Rest], DB, Types, Acc) ->
    Row1 = lists:zipwith3(fun type_transform/3, Row, DB, Types),
    fields_to_types(Rest, DB, Types, [Row1|Acc]).


type_transform(V, F, binary) -> 
    {F,list_to_binary(string:strip(V))};
type_transform(V, F, integer) ->
    case string:strip(V) of
        [] -> {F, 0};
        Value -> {F, list_to_integer(Value)}
    end;
type_transform(V, F, price) ->
    case string:strip(V) of
        [] -> {F, 0};
        Value -> {F, round(list_to_float(Value) * 100)}
    end;
type_transform(V, F, date) ->
    % An empty date is represented by the string ".  ."
    V1 = string:strip(V),
    V2 = string:strip(V1, both, $.),
    case string:strip(V2) of
        [] -> {F, {2000,1,1}};
        Value -> 
            % "DD.MM.YY" -> {Y,M,D}
            [D,M,Y] = string:tokens(Value, "."),
            Year  = list_to_integer(Y),
            Year1 = if Year >= 2000 -> Year; true -> Year+2000 end, 
            {F, {Year1, list_to_integer(M), list_to_integer(D)}}
    end.


%% @spec readfile(File, Separator) -> {ok, list()} | {error, Reason}
%% @doc Read a vmsii file and split it into lists of lists
readfile(File, Sep) ->
    case file:read_file(File) of
        {ok, Bin} ->
            List = binary_to_list(Bin),
            {ok, split_lines_fields(List, Sep)}; 
        {error, Reason} ->
            {error, Reason}
    end.
    

%% @doc Split the input stream into a list of fields, line seperator is newline, field separator is $^
split_lines_fields(List, Sep) ->
    split_lines_fields(List, Sep, [], [], []).

%% @doc Split the input stream into a list of fields, line seperator is newline, field separator is $^
split_lines_fields([], _Sep, [], [], Acc) -> 
    lists:reverse(Acc);
split_lines_fields([], _Sep, Field, Line, Acc) ->
    Line2 = lists:reverse([Field|Line]),
    Acc1  = acc_non_empty(Line2,Acc),
    lists:reverse(Acc1);

% CRLF -> map to LF
split_lines_fields([$\r,$\n|Rest], Sep, Field, Line, Acc) ->
    split_lines_fields([$\n|Rest], Sep, Field, Line, Acc);

% End of line, accumulate the fields till now
split_lines_fields([$\n|Rest], Sep, Field, Line, Acc) ->
    Line1 = [lists:reverse(Field)|Line],
    Line2 = lists:reverse(Line1),
    split_lines_fields(Rest, Sep, [], [], [Line2|Acc]);

% End of field, accumulate field into the line accumulator
split_lines_fields([C|Rest], Sep, Field, Line, Acc) when C == Sep ->
    split_lines_fields(Rest, Sep, [], [lists:reverse(Field)|Line], Acc);

% Accumulate other characters into the field
split_lines_fields([C|Rest], Sep, Field, Line, Acc) ->
    split_lines_fields(Rest, Sep, [C|Field], Line, Acc).


acc_non_empty([],List) -> List;
acc_non_empty(F, List) -> [F|List].



test() ->
    Data = <<"1710.0028^Loekie Flyer Rn M16^Zomer Blauw / Kids Blauw^149.00^125.21^125.00^01.01.08^01.02.08^Loekie^1710-0028.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0029^Loekie Flyer Rn M18^Zomer Blauw / Kids Blauw^159.00^133.61^129.00^01.05.08^01.07.08^Loekie^1710-0029.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0030^Loekie Flyer Rn M20^Fel Roze / Vlinder Roze^189.00^158.82^170.00^21.10.08^29.10.08^Loekie^1710-0030.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0031^Loekie Flyer Rn M22^Fel Roze / Vlinder Roze^199.00^167.23^0.00^.  .^.  .^Loekie^1710-0031.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0032^Bigbike Flyer Rn M24^Lila Paars / Zacht Paars^219.00^184.03^0.00^.  .^.  .^BigBike^1710-0032.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0033^Loekie Streetwise Rn J12.5^Muis Grijs / Mais Geel^149.00^125.21^0.00^.  .^.  .^Loekie^1710-0033.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0034^Loekie Streetwise Rn J16^Muis Grijs / Appelgroen^189.00^158.82^0.00^.  .^.  .^Loekie^1710-0034.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0035^Loekie Streetwise Rn J20^Muis Grijs / Fel Blauw^249.00^209.24^0.00^.  .^.  .^Loekie^1710-0035.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0037^Loekie X-Plorer Rn J16^Inkt Blauw / Kiezel Grijs^179.00^150.42^0.00^.  .^.  .^Loekie^1710-0037.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0038^Loekie X-Plorer Rn J20^Diep Rood / Kiezel Grijs^239.00^200.84^0.00^.  .^.  .^Loekie^1710-0038.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0039^Bigbike X-Plorer Rn J24^Heftig Blauw / Alu Grijs^269.00^226.05^0.00^.  .^.  .^BigBike^1710-0039.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0040^Bigbike X-Plorer N3 J24^Heftig Blauw / Alu Grijs^309.00^259.66^0.00^.  .^.  .^BigBike^1710-0040.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0041^Loekie Urban Rn J16^Space Zwart / Ferrari Rood^179.00^150.42^0.00^.  .^.  .^Loekie^1710-0041.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0042^Loekie Urban Rn J20^Space Zwart / Gletsjer Blauw^249.00^209.24^0.00^.  .^.  .^Loekie^1710-0042.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0043^Loekie Urban Rn J22^Space Zwart / Blauw^259.00^217.65^0.00^.  .^.  .^Loekie^1710-0043.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0044^Bigbike Urban Rn J24^Nacht Zwart / Kermit Groen^269.00^226.05^0.00^.  .^.  .^BigBike^1710-0044.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0045^Bigbike Urban3 J24^Nacht Zwart / Kermit Groen^309.00^259.66^0.00^.  .^.  .^BigBike^1710-0045.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0046^Loekie Flyer Rn J12.5^Blauw Groen / Zwembad Blauw^99.00^83.19^0.00^.  .^.  .^Loekie^1710-0046.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00
1710.0047^Loekie Flyer Rn J16^Koraal Rood / Warm Oranje^149.00^125.21^0.00^.  .^.  .^Loekie^1710-0047.jpg^Kinderfietsen*4-6 jaar^geen product informatie...^^^^^^0.00">>,
    List = binary_to_list(Data),
    CSV  = split_lines_fields(List, $^),
    sku_list_to_dbrows(CSV).

test1() ->
    Data = <<"ARTNR;VOORRAAD;VERKOOPIN;ACTIEPRIJS;ACTIEVAN;ACTIETOT
10014035;    0;8.95;0.00;;
10014258;    0;5.49;0.00;;
10014530;    0;12.95;0.00;;
10014531;    0;6.95;0.00;;
10014532;    0;12.95;0.00;;
10020700;    1;1499.00;0.00;;
43751654;   79;39.95;0.00;01.01.2001;01.01.2001
">>,
    List = binary_to_list(Data),
    CSV  = split_lines_fields(List, $;),
    stock_list_to_dbrows(CSV).
