%% @author Marc Worrell
%% @copyright 2009 Marc Worrell
%% 
%% Based on wf_utils.erl which is Copyright (c) 2008-2009 Rusty Klophaus
%%
%% @doc Misc utility functions for zophrenic

-module(zp_utils).
-include("zophrenic.hrl").

-export ([
    now/0,
	get_seconds/0,
	f/1,
	f/2,
	encode/2,
	decode/2,
	hex_encode/1,
	hex_decode/1,
	checksum/1,
	checksum_assert/2,
	pickle/1,
	depickle/1,
	url_encode/1,
	os_escape/1,
	js_escape/1,
	js_object/1,
	js_object/2,
	combine/2,
	combine_defined/2,
	prefix/2,
	replace/3,
	coalesce/1,
	is_process_alive/1,
	trim/1,
	is_true/1,
	assert/2
]).

%%% FORMAT %%%

f(S) -> f(S, []).
f(S, Args) -> lists:flatten(io_lib:format(S, Args)).


%% @doc Return the current tick count
now() ->
    {M,S,_M} = erlang:now(),
    M*1000 + S.


%% @doc Return the current universal time in seconds
get_seconds() -> calendar:datetime_to_gregorian_seconds(calendar:universal_time()).


%% @doc Multinode is_process_alive check
is_process_alive(Pid) ->
	case is_pid(Pid) of
		true -> 
			% If node(Pid) is down, rpc:call returns something other than
			% true or false.
			case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
				true -> true;
				_ -> false
			end;
		_ -> false
	end.

%%% HEX ENCODE and HEX DECODE

hex_encode(Data) -> encode(Data, 16).
hex_decode(Data) -> decode(Data, 16).

encode(Data, Base) when is_binary(Data) -> encode(binary_to_list(Data), Base);
encode(Data, Base) when is_list(Data) ->
	F = fun(C) when is_integer(C) ->
		case erlang:integer_to_list(C, Base) of
			[C1, C2] -> [C1, C2];
			[C1]     -> [$0, C1]
		end
	end,
	[F(I) || I <- Data].
	
decode(Data, Base) when is_binary(Data) -> decode(binary_to_list(Data), Base);
decode(Data, Base) when is_list(Data) -> 	
	inner_decode(Data, Base).

inner_decode(Data, Base) when is_list(Data) ->
	case Data of
		[C1, C2|Rest] -> 
			I = erlang:list_to_integer([C1, C2], Base),
			[I|inner_decode(Rest, Base)];
		[] -> 
			[]
	end.


%%% CHECKSUM %%%

checksum(Data) ->
    Sign = zp_ids:sign_key_simple(),
    zp_utils:hex_encode(erlang:md5([Sign,Data])).

checksum_assert(Data, Checksum) ->
    Sign = zp_ids:sign_key_simple(),
    assert(list_to_binary(zp_utils:hex_decode(Checksum)) == erlang:md5([Sign,Data]), checksum_invalid).

%%% PICKLE / UNPICKLE %%%

pickle(Data) ->
    BData = erlang:term_to_binary(Data),
	Nonce = zp_ids:number(1 bsl 31),
	Sign  = zp_ids:sign_key(),
	SData = <<BData/binary, Nonce:32, Sign/binary>>,
	<<C1:64,C2:64>> = erlang:md5(SData),
	base64:encode(<<C1:64, C2:64, Nonce:32, BData/binary>>).
	
depickle(Data) ->
    try
        <<C1:64, C2:64, Nonce:32, BData/binary>> = base64:decode(Data),
    	Sign  = zp_ids:sign_key(),
    	SData = <<BData/binary, Nonce:32, Sign/binary>>,
    	<<C1:64, C2:64>> = erlang:md5(SData),
    	erlang:binary_to_term(BData)
    catch
        _M:_E -> erlang:throw("Postback data invalid, could not depickle: "++Data)
    end.
	
%%% URL ENCODE %%%

url_encode(S) -> quote_plus(S).

% quote_plus and hexdigit are from Mochiweb.

-define(PERCENT, 37).  % $\%
-define(FULLSTOP, 46). % $\.
-define(QS_SAFE(C), ((C >= $a andalso C =< $z) orelse
                     (C >= $A andalso C =< $Z) orelse
                     (C >= $0 andalso C =< $9) orelse
                     (C =:= ?FULLSTOP orelse C =:= $- orelse C =:= $~ orelse
                      C =:= $_))).

hexdigit(C) when C < 10 -> $0 + C;
hexdigit(C) when C < 16 -> $A + (C - 10).

quote_plus(Atom) when is_atom(Atom) ->
    quote_plus(atom_to_list(Atom));
quote_plus(Int) when is_integer(Int) ->
    quote_plus(integer_to_list(Int));
quote_plus(String) ->
    quote_plus(String, []).

quote_plus([], Acc) ->
    lists:reverse(Acc);
quote_plus([C | Rest], Acc) when ?QS_SAFE(C) ->
    quote_plus(Rest, [C | Acc]);
quote_plus([$\s | Rest], Acc) ->
    quote_plus(Rest, [$+ | Acc]);
quote_plus([C | Rest], Acc) ->
    <<Hi:4, Lo:4>> = <<C>>,
    quote_plus(Rest, [hexdigit(Lo), hexdigit(Hi), ?PERCENT | Acc]).

%% @spec os_escape(String) -> String
%% @doc Simple escape function for command line arguments
os_escape(A) when is_binary(A) ->
    os_escape(binary_to_list(A));
os_escape(A) when is_list(A) -> 
    os_escape(lists:flatten(A), []).

os_escape([], Acc) ->
    lists:reverse(Acc);
os_escape([C|Rest], Acc) when 
                (C >= $A andalso C =< $Z)
         orelse (C >= $a andalso C =< $z)
         orelse (C >= $0 andalso C =< $9)
         orelse C == $_
         orelse C == $.
         orelse C == $-
         orelse C == $+
         orelse C == $/
         orelse C == $(
         orelse C == $)
         orelse C == 32
    -> 
    os_escape(Rest, [C|Acc]);
os_escape([C|Rest], Acc) when 
                C >= 32
        orelse  C == $\r
        orelse  C == $\n
        orelse  C == $\t
    ->
    os_escape(Rest, [C,$\\|Acc]).


%%% ESCAPE JAVASCRIPT %%%

%% @doc Javascript escape, see also: http://code.google.com/p/doctype/wiki/ArticleXSSInJavaScript

js_escape(undefined) -> [];
js_escape([]) -> [];
js_escape(<<>>) -> [];
js_escape(Value) when is_atom(Value) ->  js_escape(atom_to_list(Value), []);
js_escape(Value) when is_binary(Value) -> js_escape(binary_to_list(Value), []);
js_escape(Value) -> js_escape(Value, []).

js_escape([], Acc) -> lists:reverse(Acc);
js_escape([$\\|T], Acc) -> js_escape(T, [$\\,$\\|Acc]);
js_escape([$\n|T], Acc) -> js_escape(T, [$n,$\\|Acc]);
js_escape([$\r|T], Acc) -> js_escape(T, [$r,$\\|Acc]);
js_escape([$\t|T], Acc) -> js_escape(T, [$t,$\\|Acc]);
js_escape([$'|T], Acc) -> js_escape(T, [$7,$2,$x,$\\|Acc]);
js_escape([$"|T], Acc) -> js_escape(T, [$7,$2,$x,$\\|Acc]);
js_escape([$<|T], Acc) -> js_escape(T, [$c,$3,$x,$\\|Acc]);
js_escape([$>|T], Acc) -> js_escape(T, [$e,$3,$x,$\\|Acc]);
js_escape([$=|T], Acc) -> js_escape(T, [$d,$3,$x,$\\|Acc]);
js_escape([$&|T], Acc) -> js_escape(T, [$6,$2,$x,$\\|Acc]);
js_escape([16#85|T], Acc) -> js_escape(T, [$5,$8,$0,$0,$u,$\\|Acc]);
js_escape([16#2028|T],Acc)-> js_escape(T, [$8,$2,$0,$2,$u,$\\|Acc]);
js_escape([16#2029|T],Acc)-> js_escape(T, [$9,$2,$0,$2,$u,$\\|Acc]);
js_escape([16#e2,16#80,16#a8|T],Acc)-> js_escape(T, [$8,$2,$0,$2,$u,$\\|Acc]);
js_escape([16#e2,16#80,16#a9|T],Acc)-> js_escape(T, [$9,$2,$0,$2,$u,$\\|Acc]);
js_escape([H|T], Acc) when is_integer(H) -> 
    js_escape(T, [H|Acc]);
js_escape([H|T], Acc) -> 
    H1 = js_escape(H),
    js_escape(T, [H1|Acc]).

%% js_escape(<<"<script", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "<scr\" + \"ipt">>);
%% js_escape(<<"script>", Rest/binary>>, Acc) -> js_escape(Rest, <<Acc/binary, "scr\" + \"ipt>">>);


%% @doc Create a javascript object from a proplist
js_object([]) -> <<"{}">>;
js_object(L) -> js_object(L,[]).

js_object(L, []) -> js_object1(L, []);
js_object(L, [Key|T]) -> js_object(proplists:delete(Key,L), T).

%% recursively add all properties as object properties
js_object1([], Acc) -> 
    [${, combine($,,Acc), $}];
js_object1([{Key,Value}|T], Acc) ->
    Prop = [atom_to_list(Key), $:, js_prop_value(Key, Value)],
    js_object1(T, [Prop|Acc]).

js_prop_value(_, undefined) -> <<"null">>;
js_prop_value(_, true) -> <<"true">>;
js_prop_value(_, false) -> <<"true">>;
js_prop_value(_, Atom) when is_atom(Atom) -> [$",js_escape(erlang:atom_to_list(Atom)), $"];
js_prop_value(pattern, [$/|T]=List) ->
    %% Check for regexp
    case length(T) of
        Len when Len =< 2 -> 
            [$",js_escape(List),$"];
        _Len ->
            case string:rchr(T, $/) of
                0 -> 
                    [$",js_escape(List),$"];
                N -> 
                    {_Re, [$/|Options]} = lists:split(N-1,T),
                    case only_letters(Options) of
                        true -> List;
                        false -> [$",js_escape(List),$"]
                    end
            end
    end;
js_prop_value(_, Int) when is_integer(Int) -> io_lib:write(Int);
js_prop_value(_, Value) -> [$",js_escape(Value),$"].


only_letters([]) -> 
    true;
only_letters([C|T]) when (C >= $a andalso C =< $z) orelse (C >= $A andalso C =< $Z) ->
    only_letters(T);
only_letters(_) ->
    false.

combine_defined(Sep, List) ->
    List2 = lists:filter(fun(X) -> X /= undefined end, List),
    combine(Sep, List2).

combine(_Sep, []) -> [];
combine(_Sep, [A]) -> [A];
combine(Sep, [H|T]) -> [H, prefix(Sep, T)].

prefix(Sep, List) -> prefix(Sep,List,[]).

prefix(_Sep, [], Acc) -> lists:reverse(Acc);
prefix(Sep, [H|T], Acc) -> prefix(Sep, T, [H,Sep|Acc]).


%%% STRING REPLACE %%%

replace([], _, _) -> [];
replace(String, S1, S2) when is_list(String), is_list(S1), is_list(S2) ->
	Length = length(S1),
	case string:substr(String, 1, Length) of 
		S1 -> 
			S2 ++ replace(string:substr(String, Length + 1), S1, S2);
		_ -> 
			[hd(String)|replace(tl(String), S1, S2)]
	end.
	
%%% COALESCE %%%

coalesce([]) -> undefined;
coalesce([H]) -> H;
coalesce([undefined|T]) -> coalesce(T);
coalesce([[]|T]) -> coalesce(T);
coalesce([H|_]) -> H.

%% @doc Remove whitespace at the start and end of the string
%% @todo Check if we want to use a regexp (re) instead, needed for stripping newline, tab etc.
trim(S) -> string:strip(S, both).


%% @doc Check if the parameter could represent the logical value of "true"    
is_true([$t|_T]) -> true;
is_true([$y|_T]) -> true;
is_true("on") -> true;
is_true("1") -> true;

is_true(<<"true">>) -> true;
is_true(<<"yes">>) -> true;
is_true(<<"on">>) -> true;
is_true(<<"1">>) -> true;

is_true(true) -> true;
is_true(yes) -> true;
is_true(on) -> true;

is_true(N) when is_integer(N) andalso N /= 0 -> true;

is_true(_) -> false.

%% @spec assert(bool(), error) -> none()
%% @doc Check if an assertion is ok or failed
assert(false, Error) -> erlang:error(Error);
assert(_, _) -> ok.
