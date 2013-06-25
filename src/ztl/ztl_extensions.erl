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
-module(ztl_extensions).
-author('Andreas Stenius <kaos@astekk.se>').

%% API
-export([scan/1, parse/1, compile_ast/3, setup_render_ast/0]).

-include_lib("erlydtl/include/erlydtl_ext.hrl").

scan(#scanner_state{ template="#" ++ T, pos={L, C}=P, scanned=Q}=S) ->
    {ok, S#scanner_state{ template=T, pos={L, C+1}, scanned=[{hash, P, "#"}|Q] }};
scan(#scanner_state{}) ->
    undefined.

parse(State) ->
    ztl_parser:resume(State).

compile_ast({autoid, {identifier, _, Name}}, _Context, TreeWalker) ->
    {
      {erl_syntax:application(
        erl_syntax:atom(lists), erl_syntax:atom(append),
        [erl_syntax:variable("AutoId"), 
         erl_syntax:string([$-|atom_to_list(Name)])]),
       #ast_info{}}, 
      TreeWalker
    };
compile_ast(_Ast, _Context, _TreeWalker) ->
    undefined.

setup_render_ast() ->
    [
     erl_syntax:match_expr(
       erl_syntax:variable("AutoId"),
       erl_syntax:application(
         erl_syntax:atom(z_ids),
         erl_syntax:atom(id),
         [erl_syntax:integer(8)]))
    ].
