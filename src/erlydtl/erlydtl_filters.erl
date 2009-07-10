%%%-------------------------------------------------------------------
%%% File:      erlydtl_filters.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc 
%%% Template filters
%%% @end  
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2007 Roberto Saccon, Evan Miller
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%% @since 2007-11-11 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl_filters).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

-compile(export_all).

-include_lib("zophrenic.hrl").

-define(NO_ENCODE(C), ((C >= $a andalso C =< $z) orelse
                                  (C >= $A andalso C =< $Z) orelse
                                  (C >= $0 andalso C =< $9) orelse
                                  (C =:= $. orelse C =:= $- 
                                  orelse C =:= $~ orelse C =:= $_))).

opttrans({trans, _}=Trans, Language) ->
	zp_trans:trans(Trans, Language);
opttrans(V, _Language) ->
	V.

add(undefined, _Number) ->
    undefined;
add(Input, Number) when is_binary(Input) ->
    list_to_binary(add(binary_to_list(Input), Number));
add(Input, Number) when is_list(Input) ->
    integer_to_list(add(list_to_integer(Input), Number));
add(Input, Number) when is_integer(Input) ->
    Input + zp_convert:to_integer(Number).


append(Input, undefined) ->
    Input;
append(undefined, Append) ->
    Append;
append(Input, Append) ->
    zp_convert:to_list(Input) ++ zp_convert:to_list(Append).


insert(Input, Insert) ->
    append(Insert, Input).


lt(undefined, _Number) ->
    undefined;
lt(Input, Number) ->
    try
        zp_convert:to_integer(Input) < zp_convert:to_integer(Number)
    catch
        _:_ -> undefined
    end.

le(undefined, _Number) ->
    undefined;
le(Input, Number) ->
    try
        zp_convert:to_integer(Input) =< zp_convert:to_integer(Number)
    catch
        _:_ -> undefined
    end.

eq(undefined, _Number) ->
    undefined;
eq(Input, Number) ->
    try
        zp_convert:to_integer(Input) == zp_convert:to_integer(Number)
    catch
        _:_ -> undefined
    end.


ne(undefined, _Number) ->
    undefined;
ne(Input, Number) ->
    try
        zp_convert:to_integer(Input) /= zp_convert:to_integer(Number)
    catch
        _:_ -> undefined
    end.


capfirst(undefined) ->
    undefined;
capfirst([H|T]) when H >= $a andalso H =< $z ->
    [H + $A - $a | T];
capfirst(<<Byte:8/integer, Binary/binary>>) when Byte >= $a andalso Byte =< $z ->
    [<<(Byte + $A - $a)>>, Binary].

center(undefined, _Number) ->
    undefined;
center(Input, Number) when is_binary(Input) ->
    list_to_binary(center(binary_to_list(Input), Number));
center(Input, Number) when is_list(Input) ->
    string:centre(Input, zp_convert:to_integer(Number)).


date(undefined, _FormatStr) ->
    undefined;
date(Input, FormatStr) when is_binary(FormatStr) ->
    date(Input, binary_to_list(FormatStr));
date(Input, FormatStr) when is_binary(Input) ->
    list_to_binary(date(binary_to_list(Input), FormatStr));
date({{_,_,_} = Date,{_,_,_} = Time}, FormatStr) ->
     erlydtl_dateformat:format({Date, Time}, FormatStr);
date({_,_,_} = Date, FormatStr) ->
    erlydtl_dateformat:format(Date, FormatStr);
date(Input, _FormatStr) when is_list(Input) ->
    io:format("Unexpected date parameter : ~p~n", [Input]),
    "".

escapejs(undefined) ->
    <<>>;
escapejs(Input) when is_binary(Input) ->
    escapejs(Input, 0);
escapejs(Input) when is_list(Input) ->
    escapejs(Input, []).

first(undefined) ->
    undefined;
first([First|_Rest]) ->
    [First];
first(<<First, _/binary>>) ->
    <<First>>;
first(_Other) ->
    <<>>.

fix_ampersands(undefined) ->
    undefined;
fix_ampersands(Input) when is_binary(Input) ->
    fix_ampersands(Input, 0);
fix_ampersands(Input) when is_list(Input) ->
    fix_ampersands(Input, []).

force_escape(undefined) -> 
    <<>>;
force_escape(Input) when is_list(Input) ->
    escape(Input, []);
force_escape(Input) when is_binary(Input) ->
    escape(Input, 0);
force_escape(Input) when is_integer(Input) ->
    integer_to_list(Input);
force_escape({{Y,M,D}, {_H,_I,_S}} = Input) when is_integer(Y) andalso is_integer(M) andalso is_integer(D) ->
    date(Input, "Y-m-d H:i:s");
force_escape({Y,M,D} = Input) when is_integer(Y) andalso is_integer(M) andalso is_integer(D) ->
    date(Input, "Y-m-d");
force_escape(true) ->
    yesno(true);
force_escape(false) ->
    yesno(false).
    

format_integer(Input) when is_integer(Input) ->
    integer_to_list(Input);
format_integer(Input) ->
    Input.

format_number(Input) when is_integer(Input) ->
    integer_to_list(Input);
format_number(Input) when is_float(Input) ->
    io_lib:format("~p", [Input]);
format_number(Input) when is_function(Input, 0) ->
    format_number(Input());
format_number(Input) ->
    Input.

format_price(Input) when is_integer(Input) ->
    case Input rem 100 of
        0 -> 
            integer_to_list(Input div 100);
        Cents when Cents < 10 -> 
            [integer_to_list(Input div 100), $,, $0, Cents + $0 ];
        Cents -> 
            [integer_to_list(Input div 100), $,, integer_to_list(Cents) ]
    end;
format_price(Input) when is_float(Input) ->
    format_price(round(Input * 100));
format_price(Input) when is_function(Input, 0) ->
    format_price(Input());
format_price(Input) when is_list(Input) ->
    case string:to_integer(Input) of
        {error, _} -> Input;
        {N, _Rest} -> format_price(N)
    end;
format_price(undefined) ->
    "-".


default(Input, Default) -> 
    case erlydtl_runtime:is_false(Input) of
        true -> Default;
        false -> Input
    end.


default_if_none(Input, Default) -> 
    default_if_undefined(Input, Default).
default_if_undefined(Input, Default) -> 
    case Input of
        undefined -> Default;
        _ -> Input
    end.


is_defined(undefined) ->
    false;
is_defined(_V) ->
    true.

is_undefined(V) ->
    not(is_defined(V)).


striptags(undefined) ->
    undefined;
striptags(In) when is_integer(In) ->
    In;
striptags(In) when is_float(In) ->
    In;
striptags(In) ->
    zp_html:strip(In).
    

% Translate atoms and numbers to strings
% Leave tuples as tuples.
stringify(undefined) ->
    <<>>;
stringify(In) when is_atom(In) ->
    atom_to_list(In);
stringify(In) when is_integer(In) ->
    integer_to_list(In);
stringify(In) when is_float(In) ->
    mochinum:digits(In);
stringify(In) ->
    In.

slugify(undefined) ->
    undefined;
slugify(Input) ->
    zp_string:to_slug(Input).


join(Input, Separator) when is_binary(Input) ->
    join(binary_to_list(Input), Separator);
join(Input, Separator) when is_list(Input) ->
    string:join(Input, zp_binary:to_list(Separator));
join(Input, _) ->
    Input.

last(undefined) ->
    undefined;
last(Input) when is_binary(Input) ->
    case size(Input) of
        0 -> Input;
        N ->
            Offset = N - 1,
            <<_:Offset/binary, Byte/binary>> = Input,
            Byte
    end;
last(Input) when is_list(Input) ->
    [lists:last(Input)];
last(Input) ->
    Input.

length(undefined) ->
    undefined;
length([]) -> 
    "0";
length(<<>>) -> 
    "0";
length(Input) when is_list(Input) ->
    integer_to_list(erlang:length(Input));
length(Input) when is_binary(Input) ->
    integer_to_list(size(Input));
length(_Input) ->
    "1".

length_is(undefined, _Number) ->
    undefined;
length_is(Input, Number) when is_list(Input), is_integer(Number) ->
    length_is(Input, integer_to_list(Number));
length_is(Input, Number) when is_list(Input), is_list(Number) ->
    ?MODULE:length(Input) =:= Number;
length_is(_Input, Number) ->
    1 =:= zp_convert:to_integer(Number).


linebreaksbr(undefined) ->
    undefined;
linebreaksbr(Input) when is_binary(Input) ->
    linebreaksbr(Input, 0);
linebreaksbr(Input) ->
    linebreaksbr(Input, []).


ljust(undefined, _Number) -> 
    undefined;
ljust(Input, Number) when is_binary(Input) ->
    list_to_binary(ljust(binary_to_list(Input), Number));
ljust(Input, Number) when is_list(Input) ->
    string:left(Input, zp_convert:to_integer(Number));
ljust(Input, _Number) -> 
    Input.


lower(undefined) ->
    undefined;
lower(Input) when is_list(Input) or is_binary(Input) ->
    zp_string:to_lower(Input);
lower(Input) ->
    Input.


member(_S, undefined) ->
    false;
member(S, [H|_] = L) when is_list(S) andalso is_binary(H) ->
    lists:member(list_to_binary(S), L);
member(S, [H|_] = L) when is_list(S) andalso is_integer(H) ->
    try
        lists:member(list_to_integer(S), L)
    catch
        _:_ -> false
    end;
member(S, L) when is_list(L) ->
    lists:member(S, L);
member(_S, _L) ->
    undefined.


rjust(undefined, _Number) -> 
    undefined;
rjust(Input, Number) when is_binary(Input) ->
    list_to_binary(rjust(binary_to_list(Input), Number));
rjust(Input, Number) when is_list(Input) ->
    string:right(Input, zp_convert:to_integer(Number));
rjust(Input, _Number) -> 
    Input.

upper(undefined) ->
    undefined;
upper(Input) when is_list(Input) or is_binary(Input) ->
    zp_string:to_upper(Input);
upper(Input) ->
    Input.

urlencode(undefined) ->
    undefined;
urlencode(Input) when is_binary(Input) ->
    urlencode(Input, 0);
urlencode(Input) when is_list(Input) ->
    urlencode(Input, []);
urlencode(_Input) ->
    <<>>.


yesno(B) ->
    case erlydtl_runtime:is_false(B) of
        true -> "no";
        false -> "yes"
    end.
yesno(undefined, Values) ->
    case string:tokens(zp_convert:to_list(Values), ",") of
        [_Yes, _No, Maybe] -> Maybe;
        [_Yes, No] -> No
    end;
yesno(B, Values) ->
    case erlydtl_runtime:is_false(B) of
        true ->
            [_Yes,No|_Rest] = string:tokens(zp_convert:to_list(Values), ","),
            No;
        false -> 
            [Yes|_Rest] = string:tokens(zp_convert:to_list(Values), ","),
            Yes
    end.


% internal

escape(Binary, Index) when is_binary(Binary) ->
    case Binary of
        <<Pre:Index/binary, $<, Post/binary>> ->
            process_binary_match(Pre, <<"&lt;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, $>, Post/binary>> ->
            process_binary_match(Pre, <<"&gt;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, $&, Post/binary>> ->
            process_binary_match(Pre, <<"&amp;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, 34, Post/binary>> ->
            process_binary_match(Pre, <<"&quot;">>, size(Post), escape(Post, 0));
        <<Pre:Index/binary, 39, Post/binary>> ->
            process_binary_match(Pre, <<"&#039;">>, size(Post), escape(Post, 0));
        <<_:Index/binary, _, _/binary>> ->
            escape(Binary, Index + 1);
        Binary ->
            Binary
    end;
escape([], Acc) ->
    lists:reverse(Acc);
escape("<" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&lt;", Acc));
escape(">" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&gt;", Acc));
escape("&" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&amp;", Acc));
escape("\"" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&quot;", Acc));
escape("'" ++ Rest, Acc) ->
    escape(Rest, lists:reverse("&#039;", Acc));
escape([C | Rest], Acc) ->
    escape(Rest, [C | Acc]).


escapejs([], Acc) ->
    lists:reverse(Acc);
escapejs("'" ++ Rest, Acc) ->
    escapejs(Rest, lists:reverse("\\'", Acc));
escapejs("\"" ++ Rest, Acc) ->
    escapejs(Rest, lists:reverse("\\\"", Acc));
escapejs([C | Rest], Acc) ->
    escapejs(Rest, [C | Acc]);
escapejs(Binary, Index) when is_binary(Binary) ->
    case Binary of
        <<Pre:Index/binary, 39, Post/binary>> ->
            process_binary_match(Pre, <<"\\'">>, size(Post), escapejs(Post, 0));
        <<Pre:Index/binary, 34, Post/binary>> ->
            process_binary_match(Pre, <<"\\\"">>, size(Post), escapejs(Post, 0));
        <<_:Index/binary, _/binary>> ->
            escapejs(Binary, Index + 1);
        _ ->
            Binary
    end.

fix_ampersands(Input, Index) when is_binary(Input) ->
    case Input of
        <<Pre:Index/binary, $&, Post/binary>> ->
            process_binary_match(Pre, <<"&amp;">>, size(Post), Post);
        <<_:Index/binary, _/binary>> ->
            fix_ampersands(Input, Index + 1);
        _ ->
            Input
    end;
fix_ampersands([], Acc) ->
    lists:reverse(Acc);
fix_ampersands("&" ++ Rest, Acc) ->
    fix_ampersands(Rest, lists:reverse("&amp;", Acc));
fix_ampersands([C | Rest], Acc) ->
    fix_ampersands(Rest, [C | Acc]).

linebreaksbr(Input, Index) when is_binary(Input) ->
    Break = <<"<br />">>,
    case Input of
        <<Pre:Index/binary, $\r, $\n, Post/binary>> ->
            process_binary_match(Pre, Break, size(Post), linebreaksbr(Post, 0));
        <<Pre:Index/binary, $\n, Post/binary>> ->
            process_binary_match(Pre, Break, size(Post), linebreaksbr(Post, 0));
        <<_:Index/binary, _/binary>> ->
            linebreaksbr(Input, Index + 1);
        _ ->
            Input
    end;
linebreaksbr([], Acc) ->
    lists:reverse(Acc);
linebreaksbr("\r\n" ++ Rest, Acc) ->
    linebreaksbr(Rest, lists:reverse("<br />", Acc));
linebreaksbr("\n" ++ Rest, Acc) ->
    linebreaksbr(Rest, lists:reverse("<br />", Acc));
linebreaksbr([C | Rest], Acc) ->
    linebreaksbr(Rest, [C | Acc]).

lower(Input, Index) ->
    case Input of
        <<Pre:Index/binary, Byte, Post/binary>> when Byte >= $A andalso Byte =< $Z ->
            process_binary_match(Pre, <<(Byte - $A + $a)>>, size(Post), lower(Post, 0));
        <<_:Index/binary, _/binary>> ->
            lower(Input, Index + 1);
        _ ->
            Input
    end.

% Taken from quote_plus of mochiweb_util

urlencode(Input, Index) when is_binary(Input) ->
    case Input of
        <<_:Index/binary, Byte, _/binary>> when ?NO_ENCODE(Byte) ->
            urlencode(Input, Index + 1);
        <<Pre:Index/binary, $\s, Post/binary>> ->
            process_binary_match(Pre, <<"+">>, size(Post), urlencode(Post, 0));
        <<Pre:Index/binary, Hi:4, Lo:4, Post/binary>> ->
            HiDigit = hexdigit(Hi),
            LoDigit = hexdigit(Lo),
            Code = <<$\%, HiDigit, LoDigit>>,
            process_binary_match(Pre, Code, size(Post), urlencode(Post, 0));
        Input ->
            Input
    end;
urlencode([], Acc) ->
    lists:reverse(Acc);
urlencode([C | Rest], Acc) when ?NO_ENCODE(C) ->
    urlencode(Rest, [C | Acc]);
urlencode([$\s | Rest], Acc) ->
    urlencode(Rest, [$+ | Acc]);
urlencode([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    urlencode(Rest, [hexdigit(Lo), hexdigit(Hi), $\% | Acc]).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

process_binary_match(Pre, Insertion, SizePost, Post) ->
    case {size(Pre), SizePost} of
        {0, 0} -> Insertion;
        {0, _} -> [Insertion, Post];
        {_, 0} -> [Pre, Insertion];
        _ -> [Pre, Insertion, Post]
    end.
