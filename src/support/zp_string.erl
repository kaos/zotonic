%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-26
%%
%% @doc String related functions

-module(zp_string).
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
    replace/3
]).


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
    to_name(Name, []).

    to_name([], Acc) ->
        case string:strip(lists:reverse(Acc), both, $_) of
            [] -> "_";
            Name -> Name
        end;
    to_name([C|T], Acc) when C >= $A andalso C =< $Z ->
        to_name(T, [C+32|Acc]);
    to_name([C|T], Acc) when (C >= $a andalso C =< $z) orelse (C >= $0 andalso C =< $9) orelse C =:= $_ ->
        to_name(T, [C|Acc]);
    to_name("ä"++T, Acc) -> to_name(T, [$a|Acc]);
    to_name("ë"++T, Acc) -> to_name(T, [$e|Acc]);
    to_name("ï"++T, Acc) -> to_name(T, [$i|Acc]);
    to_name("ü"++T, Acc) -> to_name(T, [$u|Acc]);
    to_name("ö"++T, Acc) -> to_name(T, [$o|Acc]);
    to_name("Ä"++T, Acc) -> to_name(T, [$a|Acc]);
    to_name("Ë"++T, Acc) -> to_name(T, [$e|Acc]);
    to_name("Ï"++T, Acc) -> to_name(T, [$i|Acc]);
    to_name("Ü"++T, Acc) -> to_name(T, [$u|Acc]);
    to_name("Ö"++T, Acc) -> to_name(T, [$o|Acc]);
    to_name("é"++T, Acc) -> to_name(T, [$e|Acc]);
    to_name("è"++T, Acc) -> to_name(T, [$e|Acc]);
    to_name("É"++T, Acc) -> to_name(T, [$e|Acc]);
    to_name("È"++T, Acc) -> to_name(T, [$e|Acc]);
    to_name("í"++T, Acc) -> to_name(T, [$i|Acc]);
    to_name("ì"++T, Acc) -> to_name(T, [$i|Acc]);
    to_name("Í"++T, Acc) -> to_name(T, [$i|Acc]);
    to_name("Ì"++T, Acc) -> to_name(T, [$i|Acc]);
    to_name("ú"++T, Acc) -> to_name(T, [$u|Acc]);
    to_name("ù"++T, Acc) -> to_name(T, [$u|Acc]);
    to_name("Ú"++T, Acc) -> to_name(T, [$u|Acc]);
    to_name("Ù"++T, Acc) -> to_name(T, [$u|Acc]);
    to_name("ó"++T, Acc) -> to_name(T, [$o|Acc]);
    to_name("ò"++T, Acc) -> to_name(T, [$o|Acc]);
    to_name("Ó"++T, Acc) -> to_name(T, [$o|Acc]);
    to_name("Ò"++T, Acc) -> to_name(T, [$o|Acc]);
    to_name("ß"++T, Acc) -> to_name(T, [$s,$s|Acc]);
    to_name("ç"++T, Acc) -> to_name(T, [$c|Acc]);
    to_name("Ç"++T, Acc) -> to_name(T, [$c|Acc]);
    to_name("ø"++T, Acc) -> to_name(T, [$o|Acc]);
    to_name("Ø"++T, Acc) -> to_name(T, [$o|Acc]);
    to_name("å"++T, Acc) -> to_name(T, [$a|Acc]);
    to_name("Å"++T, Acc) -> to_name(T, [$a|Acc]);
    to_name("€"++T, Acc) -> to_name(T, [$e|Acc]);
    to_name("ÿ"++T, Acc) -> to_name(T, [$i,$j|Acc]);
    to_name("@"++T, Acc) -> to_name(T, [$_,$t,$a,$_|Acc]);
    to_name([_C|T], [$_|_] = Acc) ->
        to_name(T, Acc);
    to_name([_C|T], Acc) ->
        to_name(T, [$_|Acc]).


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

