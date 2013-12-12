%%%%% THIS IS A SLEX GENERATED FILE %%%%%

%%%-------------------------------------------------------------------
%%%
%%% Copyright (c) 2013 Andreas Stenius <kaos@astekk.se>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%% 
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------
-module(ztl_scanner).

%% This file was generated 2013-12-12 01:29:02 UTC by slex 0.2.0-1-g15e42ff.
%% http://github.com/erlydtl/slex
-slex_source(['ztl_scanner.slex',
	      {erlydtl, "src/erlydtl_scanner.slex"}]).

-export([scan/1, scan/4]).

-compile(nowarn_unused_vars).

-export([resume/1]).

-record(scanner_state,
	{template = [], scanned = [], pos = {1, 1},
	 state = in_text}).

resume(#scanner_state{template = Template,
		      scanned = Scanned, pos = Pos, state = State}) ->
    scan(Template, Scanned, Pos, State).

to_atom(L) when is_list(L) -> list_to_atom(L).

to_keyword(L, P) -> {to_atom(L ++ "_keyword"), P, L}.

atomize(L, T) -> setelement(3, T, to_atom(L)).

is_keyword(Class, {_, _, L} = T) ->
    L1 = lists:reverse(L),
    case is_keyword(Class, L1) of
      true -> to_keyword(L1, element(2, T));
      false -> atomize(L1, T)
    end;
is_keyword([C | Cs], L) ->
    is_keyword(C, L) orelse is_keyword(Cs, L);
is_keyword(all, L) -> is_keyword([any, open, close], L);
is_keyword(open_tag, L) -> is_keyword([any, open], L);
is_keyword(close_tag, L) -> is_keyword([any, close], L);
is_keyword(any, "in") -> true;
is_keyword(any, "not") -> true;
is_keyword(any, "or") -> true;
is_keyword(any, "and") -> true;
is_keyword(any, "as") -> true;
is_keyword(any, "by") -> true;
is_keyword(any, "with") -> true;
is_keyword(close, "only") -> true;
is_keyword(close, "parsed") -> true;
is_keyword(close, "noop") -> true;
is_keyword(close, "reversed") -> true;
is_keyword(close, "openblock") -> true;
is_keyword(close, "closeblock") -> true;
is_keyword(close, "openvariable") -> true;
is_keyword(close, "closevariable") -> true;
is_keyword(close, "openbrace") -> true;
is_keyword(close, "closebrace") -> true;
is_keyword(close, "opencomment") -> true;
is_keyword(close, "closecomment") -> true;
is_keyword(open, "autoescape") -> true;
is_keyword(open, "endautoescape") -> true;
is_keyword(open, "block") -> true;
is_keyword(open, "endblock") -> true;
is_keyword(open, "comment") -> true;
is_keyword(open, "endcomment") -> true;
is_keyword(open, "cycle") -> true;
is_keyword(open, "extends") -> true;
is_keyword(open, "filter") -> true;
is_keyword(open, "endfilter") -> true;
is_keyword(open, "firstof") -> true;
is_keyword(open, "for") -> true;
is_keyword(open, "empty") -> true;
is_keyword(open, "endfor") -> true;
is_keyword(open, "if") -> true;
is_keyword(open, "elif") -> true;
is_keyword(open, "else") -> true;
is_keyword(open, "endif") -> true;
is_keyword(open, "ifchanged") -> true;
is_keyword(open, "endifchanged") -> true;
is_keyword(open, "ifequal") -> true;
is_keyword(open, "endifequal") -> true;
is_keyword(open, "ifnotequal") -> true;
is_keyword(open, "endifnotequal") -> true;
is_keyword(open, "include") -> true;
is_keyword(open, "now") -> true;
is_keyword(open, "regroup") -> true;
is_keyword(open, "endregroup") -> true;
is_keyword(open, "spaceless") -> true;
is_keyword(open, "endspaceless") -> true;
is_keyword(open, "ssi") -> true;
is_keyword(open, "templatetag") -> true;
is_keyword(open, "widthratio") -> true;
is_keyword(open, "call") -> true;
is_keyword(open, "endwith") -> true;
is_keyword(open, "trans") -> true;
is_keyword(open, "blocktrans") -> true;
is_keyword(open, "endblocktrans") -> true;
is_keyword(_, _) -> false.

scan(Template) when is_list(Template) ->
    scan(Template, [], {1, 1}, in_text).

scan("{{" ++ T, S, {R, C} = P, in_text) ->
    scan(T,
	 [{open_var, P, "{{"} | post_process(S, open_var)],
	 {R, C + 2}, {in_code, "}}"});
scan("{%" ++ T, S, {R, C} = P, in_text) ->
    scan(T,
	 [{open_tag, P, "{%"} | post_process(S, open_tag)],
	 {R, C + 2}, {in_code, "%}"});
scan("<!--{{" ++ T, S, {R, C} = P, in_text) ->
    scan(T,
	 [{open_var, P, "<!--{{"} | post_process(S, open_var)],
	 {R, C + 6}, {in_code, "}}-->"});
scan("<!--{%" ++ T, S, {R, C} = P, in_text) ->
    scan(T,
	 [{open_tag, P, "<!--{%"} | post_process(S, open_tag)],
	 {R, C + 6}, {in_code, "%}-->"});
scan("{_" ++ T, S, {R, C} = P, in_text) ->
    scan(T,
	 [{trans_literal, P, ""}, {open_trans, P, "{_"}
	  | post_process(S, open_trans)],
	 {R, C + 2}, {in_trans, "_}"});
scan("<!--{_" ++ T, S, {R, C} = P, in_text) ->
    scan(T,
	 [{trans_literal, P, ""}, {open_trans, P, "<!--{_"}
	  | post_process(S, open_trans)],
	 {R, C + 6}, {in_trans, "_}-->"});
scan("{#" ++ T, S, {R, C}, in_text) ->
    scan(T, S, {R, C + 2}, {in_comment, "#}"});
scan("<!--{#" ++ T, S, {R, C}, in_text) ->
    scan(T, S, {R, C + 6}, {in_comment, "#}-->"});
scan("#}-->" ++ T, S, {R, C}, {_, "#}-->"}) ->
    scan(T, S, {R, C + 5}, in_text);
scan("#}" ++ T, S, {R, C}, {_, "#}"}) ->
    scan(T, S, {R, C + 2}, in_text);
scan([H | T], S, {R, C}, {in_comment, E} = St) ->
    scan(T, S,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan([H | T], S, {R, C} = P, in_text = St) ->
    scan(T,
	 case S of
	   [{string, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ -> [{string, P, [H]} | post_process(S, string)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan("\"" ++ T, S, {R, C} = P, {in_code, E}) ->
    scan(T,
	 [{string_literal, P, "\""} | post_process(S,
						   string_literal)],
	 {R, C + 1}, {in_double_quote, E});
scan("'" ++ T, S, {R, C} = P, {in_code, E}) ->
    scan(T,
	 [{string_literal, P, "\""} | post_process(S,
						   string_literal)],
	 {R, C + 1}, {in_single_quote, E});
scan("\"" ++ T, S, {R, C} = P, {in_double_quote, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, "\"" ++ L) | Ss];
	   _ ->
	       [{string_literal, P, "\""} | post_process(S,
							 string_literal)]
	 end,
	 {R, C + 1}, {in_code, E});
scan("'" ++ T, S, {R, C} = P, {in_single_quote, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, "\"" ++ L) | Ss];
	   _ ->
	       [{string_literal, P, "\""} | post_process(S,
							 string_literal)]
	 end,
	 {R, C + 1}, {in_code, E});
scan("\\" ++ T, S, {R, C} = P, {in_double_quote, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, "\\" ++ L) | Ss];
	   _ ->
	       [{string_literal, P, "\\"} | post_process(S,
							 string_literal)]
	 end,
	 {R, C + 1}, {in_double_quote_escape, E});
scan("\\" ++ T, S, {R, C} = P, {in_single_quote, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, "\\" ++ L) | Ss];
	   _ ->
	       [{string_literal, P, "\\"} | post_process(S,
							 string_literal)]
	 end,
	 {R, C + 1}, {in_single_quote_escape, E});
scan([H | T], S, {R, C} = P,
     {in_double_quote, E} = St) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{string_literal, P, [H]} | post_process(S,
							string_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan([H | T], S, {R, C} = P,
     {in_single_quote, E} = St) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{string_literal, P, [H]} | post_process(S,
							string_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan([H | T], S, {R, C} = P,
     {in_double_quote_escape, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{string_literal, P, [H]} | post_process(S,
							string_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_double_quote, E});
scan([H | T], S, {R, C} = P,
     {in_single_quote_escape, E}) ->
    scan(T,
	 case S of
	   [{string_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{string_literal, P, [H]} | post_process(S,
							string_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_single_quote, E});
scan("}}-->" ++ T, S, {R, C} = P, {_, "}}-->"}) ->
    scan(T,
	 [{close_var, P, "}}-->"} | post_process(S, close_var)],
	 {R, C + 5}, in_text);
scan("%}-->" ++ T, S, {R, C} = P, {_, "%}-->"}) ->
    scan(T,
	 [{close_tag, P, "%}-->"} | post_process(S, close_tag)],
	 {R, C + 5}, in_text);
scan("}}" ++ T, S, {R, C} = P, {_, "}}"}) ->
    scan(T,
	 [{close_var, P, "}}"} | post_process(S, close_var)],
	 {R, C + 2}, in_text);
scan("%}" ++ T, S, {R, C} = P, {_, "%}"} = St) ->
    case S of
      [{identifier, _, "mitabrev"}, {open_tag, _, '{%'}
       | Ss] ->
	  scan(T, [{string, {R, C + 2}, ""} | Ss], {R, C + 2},
	       {in_verbatim, undefined});
      [{identifier, _, Tag}, {identifier, _, verbatim},
       {open_tag, _, '{%'}
       | Ss] ->
	  scan(T, [{string, {R, C + 2}, ""} | Ss], {R, C + 2},
	       {in_verbatim, Tag});
      _ ->
	  scan(T,
	       [{close_tag, P, "%}"} | post_process(S, close_tag)],
	       {R, C + 2}, in_text)
    end;
scan("_}" ++ T, S, {R, C} = P, {_, "_}"}) ->
    scan(T,
	 [{close_trans, P, "_}"} | post_process(S, close_trans)],
	 {R, C + 2}, in_text);
scan([H | T], S, {R, C} = P, {in_trans, E} = St) ->
    scan(T,
	 case S of
	   [{trans_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{trans_literal, P, [H]} | post_process(S,
						       trans_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan("{%" ++ T, S, {R, C} = P, {in_verbatim, E} = St) ->
    scan(T, S, {R, C + 2}, {in_verbatim_code, {E, "%{"}});
scan(" " ++ T, S, {R, C} = P,
     {in_verbatim_code, E} = St) ->
    {Tag, Backtrack} = E,
    scan(T, S, {R, C + 1},
	 {in_verbatim_code, {Tag, [$  | Backtrack]}});
scan("endverbatim%}" ++ T, S, {R, C} = P,
     {in_verbatim_code, E} = St)
    when element(1, E) =:= undefined ->
    scan(T, S, {R, C + 13}, in_text);
scan("endverbatim " ++ T, S, {R, C} = P,
     {in_verbatim_code, E} = St) ->
    {Tag, Backtrack} = E,
    scan(T, S, {R, C + 12},
	 {in_endverbatim_code,
	  {Tag, lists:reverse("endverbatim ", Backtrack), ""}});
scan(" " ++ T, S, {R, C} = P,
     {in_endverbatim_code, E} = St)
    when element(3, E) =:= "" ->
    {Tag, Backtrack, EndTag} = E,
    scan(T, S, {R, C + 1},
	 {in_endverbatim_code, {Tag, [$  | Backtrack], EndTag}});
scan([H | T], S, {R, C} = P,
     {in_endverbatim_code, E} = St)
    when H >= $a andalso H =< $z orelse
	   H >= $0 andalso H =< $9 orelse H =:= $_ ->
    {Tag, Backtrack, EndTag} = E,
    scan(T, S, {R, C + 1},
	 {in_endverbatim_code,
	  {Tag, [H | Backtrack], [H | EndTag]}});
scan(" " ++ T, S, {R, C} = P,
     {in_endverbatim_code, E} = St)
    when element(1, E) =:= element(3, E) ->
    {Tag, Backtrack, Tag} = E,
    scan(T, S, {R, C + 1},
	 {in_endverbatim_code, {Tag, [$  | Backtrack], Tag}});
scan("%}" ++ T, S, {R, C} = P,
     {in_endverbatim_code, E} = St)
    when element(1, E) =:= element(3, E) ->
    scan(T, S, {R, C + 2}, in_text);
scan("%}" ++ T, S, {R, C} = P,
     {in_endverbatim_code, E} = St)
    when element(1, E) =:= undefined andalso
	   element(3, E) =:= "" ->
    scan(T, S, {R, C + 2}, in_text);
scan([H | T], S, {R, C} = P,
     {in_endverbatim_code, E} = St) ->
    {Tag, Backtrack, _} = E,
    scan(T,
	 case S of
	   [{string, _, L} = M | Ss] ->
	       [setelement(3, M, [H | Backtrack] ++ L) | Ss];
	   _ -> [{string, P, [H | Backtrack]} | S]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_verbatim, Tag});
scan([H | T], S, {R, C} = P,
     {in_verbatim_code, E} = St) ->
    {Tag, Backtrack} = E,
    scan(T,
	 case S of
	   [{string, _, L} = M | Ss] ->
	       [setelement(3, M, [H | Backtrack] ++ L) | Ss];
	   _ -> [{string, P, [H | Backtrack]} | S]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_verbatim, Tag});
scan([H | T], S, {R, C} = P, {in_verbatim, E} = St) ->
    scan(T,
	 case S of
	   [{string, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ -> [{string, P, [H]} | S]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_verbatim, E});
scan("==" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'==', P} | post_process(S, '==')], {R, C + 2},
	 {in_code, E});
scan("!=" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'!=', P} | post_process(S, '!=')], {R, C + 2},
	 {in_code, E});
scan(">=" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'>=', P} | post_process(S, '>=')], {R, C + 2},
	 {in_code, E});
scan("<=" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'<=', P} | post_process(S, '<=')], {R, C + 2},
	 {in_code, E});
scan(">" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'>', P} | post_process(S, '>')], {R, C + 1},
	 {in_code, E});
scan("<" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'<', P} | post_process(S, '<')], {R, C + 1},
	 {in_code, E});
scan("(" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'(', P} | post_process(S, '(')], {R, C + 1},
	 {in_code, E});
scan(")" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{')', P} | post_process(S, ')')], {R, C + 1},
	 {in_code, E});
scan("," ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{',', P} | post_process(S, ',')], {R, C + 1},
	 {in_code, E});
scan("|" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'|', P} | post_process(S, '|')], {R, C + 1},
	 {in_code, E});
scan("=" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'=', P} | post_process(S, '=')], {R, C + 1},
	 {in_code, E});
scan(":" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{':', P} | post_process(S, ':')], {R, C + 1},
	 {in_code, E});
scan("." ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'.', P} | post_process(S, '.')], {R, C + 1},
	 {in_code, E});
scan("_(" ++ T, S, {R, C} = P, {_, E}) ->
    scan(T, [{'(', P}, {'_', P} | post_process(S, '_')],
	 {R, C + 2}, {in_code, E});
scan(" " ++ T, S, {R, C}, {_, E}) ->
    scan(T, S, {R, C + 1}, {in_code, E});
scan("\r\n" ++ T, S, {R, C}, {_, E}) ->
    scan(T, S, {R + 1, 1}, {in_code, E});
scan("\t" ++ T, S, {R, C}, {_, E}) ->
    scan(T, S, {R, C + 1}, {in_code, E});
%%% 111 \r any: skip, in_code. NYI: slex doesn't cope with single CR as new line marker.
scan("\n" ++ T, S, {R, C}, {_, E}) ->
    scan(T, S, {R + 1, 1}, {in_code, E});
scan("_ " ++ T, S, {R, C} = P, {in_code, E} = St) ->
    scan(T,
	 [{'__keyword', P} | post_process(S, '__keyword')],
	 {R, C + 2}, St);
scan([H | T], S, {R, C} = P, {in_code, E})
    when H >= $a andalso H =< $z orelse
	   H >= $A andalso H =< $Z orelse H == $_ ->
    scan(T,
	 [{identifier, P, [H]} | post_process(S, identifier)],
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_identifier, E});
scan([H | T], S, {R, C} = P, {in_code, E})
    when H >= $0 andalso H =< $9 orelse H == $- ->
    scan(T,
	 [{number_literal, P, [H]} | post_process(S,
						  number_literal)],
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_number, E});
scan([H | T], S, {R, C} = P, {in_code, E} = St) ->
    {error,
     {R, erlydtl_scanner,
      lists:concat(["Illegal character in column ", C])},
     #scanner_state{template = [H | T],
		    scanned = post_process(S, err), pos = P, state = St}};
scan([H | T], S, {R, C} = P, {in_number, E} = St)
    when H >= $0 andalso H =< $9 ->
    scan(T,
	 case S of
	   [{number_literal, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{number_literal, P, [H]} | post_process(S,
							number_literal)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 St);
scan([H | T], S, {R, C} = P, {in_number, E} = St) ->
    {error,
     {R, erlydtl_scanner,
      lists:concat(["Illegal character in column ", C])},
     #scanner_state{template = [H | T],
		    scanned = post_process(S, err), pos = P,
		    state = {in_code, E}}};
scan([H | T], S, {R, C} = P, {in_identifier, E})
    when H >= $a andalso H =< $z orelse
	   H >= $A andalso H =< $Z orelse
	     H >= $0 andalso H =< $9 orelse H == $_ ->
    scan(T,
	 case S of
	   [{identifier, _, L} = M | Ss] ->
	       [setelement(3, M, [H | L]) | Ss];
	   _ ->
	       [{identifier, P, [H]} | post_process(S, identifier)]
	 end,
	 case H of
	   $\n -> {R + 1, 1};
	   _ -> {R, C + 1}
	 end,
	 {in_identifier, E});
scan([H | T], S, {R, C} = P, {in_identifier, E} = St) ->
    {error,
     {R, erlydtl_scanner,
      lists:concat(["Illegal character in column ", C])},
     #scanner_state{template = [H | T],
		    scanned = post_process(S, err), pos = P,
		    state = {in_code, E}}};
scan([], S, {R, C} = P, in_text = St) ->
    {ok, lists:reverse(post_process(S, eof))};
scan([], S, {R, C} = P, {in_comment, E} = St) ->
    {error, "Reached end of file inside a comment."};
scan([], S, {R, C} = P, {in_trans, E} = St) ->
    {error,
     "Reached end of file inside a translation "
     "tag."};
scan([], S, {R, C} = P, {_, E} = St) ->
    {error, "Reached end of file inside a code block."}.

post_process(_, {string, _, L} = T, _) ->
    setelement(3, T, begin L1 = lists:reverse(L), L1 end);
post_process(_, {string_literal, _, L} = T, _) ->
    setelement(3, T, begin L1 = lists:reverse(L), L1 end);
post_process(_, {number_literal, _, L} = T, _) ->
    setelement(3, T, begin L1 = lists:reverse(L), L1 end);
post_process(_, {open_var, _, L} = T, _) ->
    setelement(3, T, begin L1 = to_atom(L), L1 end);
post_process(_, {close_var, _, L} = T, _) ->
    setelement(3, T, begin L1 = to_atom(L), L1 end);
post_process(_, {open_tag, _, L} = T, _) ->
    setelement(3, T, begin L1 = to_atom(L), L1 end);
post_process(_, {close_tag, _, L} = T, _) ->
    setelement(3, T, begin L1 = to_atom(L), L1 end);
post_process([{open_tag, _, _} | _],
	     {identifier, _, L} = T, close_tag) ->
    is_keyword(all, T);
post_process([{open_tag, _, _} | _],
	     {identifier, _, L} = T, _) ->
    is_keyword(open_tag, T);
post_process(_, {identifier, _, L} = T, close_tag) ->
    is_keyword(close_tag, T);
post_process(_, {identifier, _, L} = T, _) ->
    is_keyword(any, T);
post_process(_, {open_trans, _, L} = T, _) ->
    setelement(3, T, begin L1 = to_atom(L), L1 end);
post_process(_, {close_trans, _, L} = T, _) ->
    setelement(3, T, begin L1 = to_atom(L), L1 end);
post_process(_, {trans_literal, _, L} = T, _) ->
    {string_literal, element(2, T),
     tl(lists:reverse(tl(L)))};
post_process(_, T, _) -> T.

post_process([S | Ss], N) ->
    [post_process(Ss, S, N) | Ss];
post_process(T, N) -> post_process(undefined, T, N).
