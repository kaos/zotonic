%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%%
%% @doc String related functions

-module(z_string).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    trim/1,
    is_string/1,
    line/1,
    to_rootname/1,
    to_name/1,
    to_slug/1,
    to_lower/1,
    to_upper/1,
    replace/3,
	truncate/2,
	truncate/3
]).

-include_lib("include/zotonic.hrl").


%% @doc Remove whitespace at the start and end of the string
%% @todo Check if we want to use a regexp (re) instead, needed for stripping newline, tab etc.
trim(S) -> string:strip(S, both).


%% @doc Check if the variable is a one dimensional list, probably a string
is_string([]) -> 
    true;
is_string([C|Rest]) when is_integer(C) andalso (C >= 32 orelse C == 9 orelse C == 10 orelse C == 12 orelse C == 13) ->
    is_string(Rest);
is_string(_) -> 
    false.


%% @doc Make sure that the string is on one line only, replace control characters with spaces
line(B) when is_binary(B) ->
    line(binary_to_list(B));
line(L) ->
    line1(L, []).
    
    line1([], Acc) ->
        lists:reverse(Acc);
    line1([H|T], Acc) when H < 32 ->
        line1(T, [32 , Acc]);
    line1([H|T], Acc) ->
        line1(T, [H|Acc]).


%% @doc Return a lowercase string for the input
%% @spec to_lower(Value) -> String
to_lower(B) when is_binary(B) ->
    string:to_lower(binary_to_list(B));
to_lower(L) when is_list(L) ->
    string:to_lower(lists:flatten(L));
to_lower(A) when is_atom(A) ->
    string:to_lower(atom_to_list(A)).


%% @doc Return a uppercase string for the input
%% @spec to_upper(Value) -> String
to_upper(B) when is_binary(B) ->
    string:to_upper(binary_to_list(B));
to_upper(L) when is_list(L) ->
    string:to_upper(lists:flatten(L));
to_upper(A) when is_atom(A) ->
    string:to_upper(atom_to_list(A)).


%% @doc Filter a filename so that we obtain a basename that is safe to use.
%% @spec to_rootname(string()) -> string()
to_rootname(Filename) ->
    to_slug(filename:rootname(filename:basename(Filename))).


%% @doc Map a string to a slug that can be used in the uri of a page. Same as a name, but then with dashes instead of underscores.
%% @spec to_slug(String) -> String
to_slug(Title) ->
    Slug = to_name(Title),
    [ case C of $_ -> $-; _ -> C end || C <- Slug ].


%% @doc Map a string to a value that can be used as a name or slug. Maps all characters to lowercase and remove non digalpha chars
%% @spec to_name(String) -> String
to_name(Name) when is_binary(Name) ->
    to_name(binary_to_list(Name));
to_name(Name) when is_atom(Name) ->
    to_name(atom_to_list(Name));
to_name(Name) ->
    to_name(Name, [], 0).

to_name([], Acc, _I) ->
    case string:strip(lists:reverse(Acc), both, $_) of
        [] -> "_";
        Name -> Name
    end;
to_name(_, Acc, 80) ->
    to_name([], Acc, 80);
to_name([C|T], Acc, I) when C >= $A andalso C =< $Z ->
    to_name(T, [C+32|Acc], I+1);
to_name([C|T], Acc, I) when (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse C =:= $_ ->
    to_name(T, [C|Acc], I+1);
to_name("ä"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("ë"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("ï"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("ü"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("ö"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("Ä"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("Ë"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("Ï"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("Ü"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("Ö"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("é"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("è"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("É"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("È"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("í"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("ì"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("Í"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("Ì"++T, Acc, I) -> to_name(T, [$i|Acc], I+1);
to_name("ú"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("ù"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("Ú"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("Ù"++T, Acc, I) -> to_name(T, [$u|Acc], I+1);
to_name("ó"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("ò"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("Ó"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("Ò"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("ß"++T, Acc, I) -> to_name(T, [$s,$s|Acc], I+1);
to_name("ç"++T, Acc, I) -> to_name(T, [$c|Acc], I+1);
to_name("Ç"++T, Acc, I) -> to_name(T, [$c|Acc], I+1);
to_name("ø"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("Ø"++T, Acc, I) -> to_name(T, [$o|Acc], I+1);
to_name("å"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("Å"++T, Acc, I) -> to_name(T, [$a|Acc], I+1);
to_name("€"++T, Acc, I) -> to_name(T, [$e|Acc], I+1);
to_name("ÿ"++T, Acc, I) -> to_name(T, [$i,$j|Acc], I+1);
to_name("@"++T, Acc, I) -> to_name(T, [$_,$t,$a,$_|Acc], I+1);
to_name([_C|T], [$_|_] = Acc, I) ->
    to_name(T, Acc, I+1);
to_name([_C|T], Acc, I) ->
    to_name(T, [$_|Acc], I+1).


%% @doc Replace a string inside another string
%% @copyright 2008 Rusty Klophaus  (Nitrogen, MIT License)
replace([], _, _) -> [];
replace(String, S1, S2) when is_list(String), is_list(S1), is_list(S2) ->
	Length = length(S1),
	case string:substr(String, 1, Length) of 
		S1 -> 
			S2 ++ replace(string:substr(String, Length + 1), S1, S2);
		_ -> 
			[hd(String)|replace(tl(String), S1, S2)]
	end.


%% @doc Truncate a string.  Append the '…' character at the place of break off.
%% @spec truncate(String, int()) -> String
truncate(L, N) ->
	truncate(L, N, "…").

truncate(B, N, Append) when is_binary(B) ->
	truncate(z_convert:to_list(B), N, Append);
truncate(_L, N, _Append) when N =< 0 ->
	[];
truncate(L, N, Append) ->
	truncate(L, N, Append, in_word, [], []).
	

	truncate([], _, _Append, _LastState, _Last, Acc) ->
		lists:reverse(Acc);
	truncate(_, 0, _Append, sentence, Last, _Acc) ->
		lists:reverse(Last);
	truncate(_, 0, Append, _, [], Acc) ->
		lists:reverse(insert_acc(Append, Acc));
	truncate(_, 0, Append, _LastState, Last, _Acc) ->
		lists:reverse(insert_acc(Append, Last));
	truncate([C|Rest], N, Append, LastState, Last, Acc) 
		when C == $.; C == $!; C == $? ->
			case LastState of
				in_word -> truncate(Rest, N-1, Append, sentence, [C|Acc], [C|Acc]);
				word    -> truncate(Rest, N-1, Append, sentence, [C|Acc], [C|Acc]);
				_ 		-> truncate(Rest, N-1, Append, LastState, Last, [C|Acc])
			end;
	truncate([C|Rest], N, Append, LastState, Last, Acc) 
		when C == $;; C == $-; C == $, ->
			case LastState of
				in_word -> truncate(Rest, N-1, Append, sentence, Acc, [C|Acc]);
				_ 		-> truncate(Rest, N-1, Append, LastState, Last, [C|Acc])
			end;
	truncate([C|Rest], N, Append, LastState, Last, Acc) 
		when C == 32; C == 9; C == 10; C == 13; C == $/; C == $|; C == $(; C == $); C == $" ->
			case LastState of
				in_word -> truncate(Rest, N-1, Append, word, Acc, [C|Acc]);
				_       -> truncate(Rest, N-1, Append, LastState, Last, [C|Acc])
			end;
	truncate([$&|_]=Input, N, Append, LastState, Last, Acc) ->
		{Rest1,Acc1} = get_entity(Input,Acc),
		case LastState of
			in_word -> truncate(Rest1, N-1, Append, word, Acc1, Acc1);
			_ 		-> truncate(Rest1, N-1, Append, LastState, Last, Acc1)
		end;
	truncate([C|Rest], N, Append, LastState, Last, Acc) ->
		truncate(Rest, N-1, Append, LastState, Last, [C|Acc]).

	insert_acc([], Acc) ->
		Acc;
	insert_acc([H|T], Acc) ->
		insert_acc(T, [H|Acc]).
	
	get_entity([], Acc) ->
		{[],Acc};
	get_entity([$;|Rest], Acc) ->
		{Rest,[$;|Acc]};
	get_entity([C|Rest], Acc) ->
		get_entity(Rest, [C|Acc]).
	
