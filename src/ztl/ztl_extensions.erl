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
-export([
         init_context/1,
         init_treewalker/1,
         scan/1, post_scan/1,
         parse/1, 
         compile_ast/3,
         custom_tag_ast/2,
         setup_render_ast/2,
         translate_ast/3,
         to_list_ast/4,
         finder_function/1
        ]).

-include_lib("erlydtl/include/erlydtl_ext.hrl").
-record(treewalker_extension, 
        {
          has_auto_id = false
        }).

%%% ----------------------------------------------------------------------------
%%% Exported functions
%%% ----------------------------------------------------------------------------

init_context(#dtl_context{ local_scopes=Scopes }=Context) ->
    {ok, Context#dtl_context{ 
           local_scopes=
               [
                [{'$autoid', erl_syntax:variable("AutoId_"++z_ids:identifier())}]
                | Scopes
               ]}}.

init_treewalker(TreeWalker) ->
    {ok, TreeWalker#treewalker{ extension=#treewalker_extension{} }}.

scan(#scanner_state{ template=[T|Ts], pos={L, C}=P, scanned=Q}=S) ->
    case scan_char(T, P) of
        undefined -> undefined;
        R -> {ok, S#scanner_state{ template=Ts, pos={L, C+1}, scanned=[R|Q] }}
    end;
scan(#scanner_state{}) ->
    undefined.

post_scan(Tokens) ->
    {ok, post_scan(Tokens, [])}.

parse(State) ->
    ztl_parser:resume(State).

compile_ast({auto_id, Tag}, Context, TreeWalker) ->
    auto_id_ast(Tag, Context, TreeWalker);
compile_ast({value, E}, Context, TreeWalker) ->
    erlydtl_compiler:value_ast(E, false, false, Context, TreeWalker);
compile_ast({value, E, With}, Context, TreeWalker) ->
    {{ValueAst, ValueInfo}, ValueTreeWalker} = value_ast(E, With, true, Context, TreeWalker),
    {{erlydtl_compiler:format(ValueAst, Context, ValueTreeWalker), ValueInfo}, ValueTreeWalker};
compile_ast({atom_literal, Atom}, _Context, TreeWalker) ->
    {{erl_syntax:atom(Atom), #ast_info{}}, TreeWalker};
compile_ast({index_value, Variable, Index}, Context, TreeWalker) ->
    {{IndexAst, IndexInfo}, TreeWalker1} = value_ast(Index, [], false, Context, TreeWalker),
    {{VarAst, VarInfo}, TreeWalker2} = value_ast(Variable, [], false, Context, TreeWalker1),
    FileNameAst = case Context#dtl_context.parse_trail of
                      [] -> erl_syntax:atom(undefined);
                      [H|_] -> erl_syntax:string(H)
                  end,
    Pos = case Index of
              {variable, {_, P, _}} -> P;
              {number_literal, P, _} -> P
          end,
    {{erl_syntax:application(
        erl_syntax:atom(ztl_runtime),
        erl_syntax:atom(find_value),
        [IndexAst, VarAst,
         erl_syntax:list(
           [FileNameAst,
            erl_syntax:abstract({pos, Pos}),
            erl_syntax:tuple([erl_syntax:atom(record_info),
                              erl_syntax:variable("_RecordInfo")]),
            erl_syntax:tuple([erl_syntax:atom(render_options),
                              erl_syntax:variable("RenderOptions")])
           ])
        ]),
      erlydtl_compiler:merge_info(IndexInfo, VarInfo)},
     TreeWalker2};
compile_ast({value_list, Values}, Context, TreeWalker) ->
    lists:foldr(
      fun(V, {{Acc,Info},TreeW0}) ->
              {{Ast,InfoV}, TreeW} = value_ast(V, [], false, Context, TreeW0),
              {{[Ast|Acc], erlydtl_compiler:merge_info(Info, InfoV)}, TreeW}
      end,
      {{[], #ast_info{}}, TreeWalker},
      Values);
compile_ast({tuple_value, {identifier, _, TupleName}, TupleArgs}, Context, TreeWalker) ->
    {{Args, Info}, TW} = erlydtl_compiler:interpret_args(TupleArgs, Context, TreeWalker),
    {{erl_syntax:tuple([erl_syntax:atom(TupleName), erl_syntax:list(Args)]), Info}, TW};
compile_ast({trans_ext, {string_literal,_,String}, Tr}, Context, TreeWalker) ->
    UnescapedString = erlydtl_compiler:unescape_string_literal(String),
    translate_ast(
      {trans,
       [{en, UnescapedString}
        |[{Lang, erlydtl_compiler:unescape_string_literal(Trans)}
          || {{identifier,_,Lang}, {string_literal,_,Trans}} <- Tr]]},
      Context, TreeWalker);
compile_ast(_Ast, _Context, _TreeWalker) ->
    undefined.


custom_tag_ast(Context, TreeWalker) ->
    ArgsAst = erl_syntax:list(
                [erl_syntax:tuple(
                   [erl_syntax:atom('$render_variables'),
                    erl_syntax:application(
                      erl_syntax:atom(proplists),
                      erl_syntax:atom(get_value),
                      [erl_syntax:atom('$render_variables'),
                       erl_syntax:variable("RenderOptions"),
                       erl_syntax:list([])
                      ])
                   ])
                ],
                erl_syntax:variable("_Variables")),
    AppAst = erl_syntax:application(
               erl_syntax:atom(z_scomp),
               erl_syntax:atom(render),
               [   erl_syntax:variable("TagName"), 
                   ArgsAst,
                   z_context_ast(Context)
               ]
              ),
    RenderedAst = erl_syntax:variable("Rendered"),
    OkAst = erl_syntax:clause(
              [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
              none,
              [RenderedAst]),
    ReasonAst = erl_syntax:variable("Reason"),
    ErrStrAst = erl_syntax:application(
                  erl_syntax:atom(io_lib),
                  erl_syntax:atom(format),
                  [erl_syntax:string("error: ~p"), erl_syntax:list([ReasonAst])]),
    ErrorAst = erl_syntax:clause(
                 [erl_syntax:tuple([erl_syntax:atom(error), ReasonAst])], 
                 none,
                 [ErrStrAst]),
    CallAst = erl_syntax:case_expr(AppAst, [OkAst, ErrorAst]),
    {{CallAst, #ast_info{}}, TreeWalker}.

setup_render_ast(Context, #treewalker{
                             extension=#treewalker_extension{
                                          has_auto_id=true }}) ->
    [
     erl_syntax:match_expr(
       erlydtl_compiler:resolve_scoped_variable_ast('$autoid', Context),
       erl_syntax:application(
         erl_syntax:atom(z_ids),
         erl_syntax:atom(identifier),
         [erl_syntax:integer(8)]))
     | setup_z_context_ast()];
setup_render_ast(_Context, _TreeWalker) ->
    setup_z_context_ast().

translate_ast(Trans, Context, TreeWalker) ->
    ZContext = proplists:get_value(z_context, Context#dtl_context.all_options),
    Ast =
        case z_trans:translations(Trans, ZContext) of
            {trans, Tr} ->
                Tr1 = [ {z_convert:to_atom(Lang), z_convert:to_binary(S)} || {Lang,S} <- Tr ],
                erl_syntax:application(
                  erl_syntax:atom(z_trans),
                  erl_syntax:atom(lookup_fallback),
                  [erl_syntax:abstract({trans, Tr1}),
                   z_context_ast(Context)
                  ]);
            S when is_binary(S) ->
                erl_syntax:abstract(S);
            L when is_list(L) ->
                erl_syntax:abstract(list_to_binary(L))
        end,
    {{Ast, #ast_info{}}, TreeWalker}.

to_list_ast(Value, IsReversed, _Context, _TreeWalker) ->
    erl_syntax:application(
      erl_syntax:atom(ztl_runtime),
      erl_syntax:atom(to_list),
      [Value, IsReversed,
       erl_syntax:variable("_Z_context")]).

finder_function(_) -> {ztl_runtime, find_value}.


%%% ----------------------------------------------------------------------------
%%% Internal functions
%%% ----------------------------------------------------------------------------

scan_char(Char, Pos) ->
    case [{T, Pos, [Char]}
          || {C, T} <- [{$#, hash},
                        {$[, open_bracket},
                        {$], close_bracket},
                        {${, open_curly},
                        {$}, close_curly}
                          ],
             C == Char] of
        [Res] -> Res;
        [] -> undefined
    end.


post_scan([], Acc) ->
    lists:reverse(Acc);
post_scan([{open_tag, _, _}=Open, T|Ts], Acc) ->
    %% look for a keyword identifier following a open tag
    %% and translate it to a proper keyword token
    post_scan(Ts, [post_open_tag(T),Open|Acc]);
post_scan([T|Ts], Acc) ->
    post_scan(Ts, [T|Acc]).

post_open_tag({identifier, Pos, Identifier}=T) ->
    case keyword(Identifier) of
        undefined -> T;
        Keyword -> {Keyword, Pos, Identifier}
    end;
post_open_tag(T) -> T.

keyword(print) -> print_keyword;
keyword(url) -> url_keyword;
keyword(_) -> undefined.
    
setup_z_context_ast() ->
    [erl_syntax:match_expr(
       erl_syntax:variable("_Z_context"),
       erl_syntax:application(
         erl_syntax:atom(proplists),
         erl_syntax:atom(get_value),
         [erl_syntax:atom(z_context),
          erl_syntax:variable("RenderOptions")]))].

z_context_ast(Context) ->
    case erlydtl_compiler:resolve_scoped_variable_ast('$z_context', Context) of
        undefined -> erl_syntax:variable("_Z_context"); 
        Ast -> Ast
    end.

set_has_auto_id(Value, #treewalker{ extension=E }=TreeWalker) ->
    TreeWalker#treewalker{ extension=E#treewalker_extension{ has_auto_id=Value }}.

auto_id_ast({identifier, _, Name}, Context, TreeWalker) ->
    {{   erl_syntax:application(
           erl_syntax:atom(lists), erl_syntax:atom(append),
           [erlydtl_compiler:resolve_scoped_variable_ast('$autoid', Context), erl_syntax:string([$-|atom_to_list(Name)])]),
         #ast_info{}
     }, set_has_auto_id(true, TreeWalker)};

auto_id_ast({{identifier, _, Name}, {identifier, _, _} = Var}, Context, TreeWalker) ->
    {{V, _, VarInfo}, TreeWalker1} = value_ast({variable, Var}, [], true, Context, TreeWalker),
    {{   erl_syntax:application(
           erl_syntax:atom(lists), erl_syntax:atom(append),
           [   
               erl_syntax:list([
                                erlydtl_compiler:resolve_scoped_variable_ast('$autoid', Context), 
                                erl_syntax:string([$-|atom_to_list(Name)]++"-"),
                                erl_syntax:application(
                                  erl_syntax:atom(z_convert),
                                  erl_syntax:atom(to_list),
                                  [V])
                               ])
           ]),
         VarInfo
     }, set_has_auto_id(true, TreeWalker1)}.

value_ast(ValueToken, Args, AsString, Context, TreeWalker) ->
    value_ast(ValueToken, Args, AsString, Context, TreeWalker, []).

value_ast(ValueToken, [], AsString, Context, TreeWalker, []) ->
    erlydtl_compiler:value_ast(ValueToken, AsString, true, Context, TreeWalker);
value_ast(ValueToken, [], AsString, Context, TreeWalker, ExtraArgs) ->
    NewContextAst = erl_syntax:application(erl_syntax:atom(z_context),
                                           erl_syntax:atom(set),
                                           [erl_syntax:atom(extra_args),
                                            erl_syntax:list([ erl_syntax:tuple([erl_syntax:atom(X),XAst]) || {X,XAst} <- ExtraArgs]),
                                            z_context_ast(Context)]),
    ContextVarAst = erl_syntax:variable("WithContext_" ++ [$_|z_ids:identifier()]),
    LocalScope = [ {'$z_context', ContextVarAst} ],
    WithContext = Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]},
    {{InnerAst,InfoValue}, TreeWalker1} = erlydtl_compiler:value_ast(ValueToken, AsString, true, WithContext, TreeWalker),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(ContextVarAst, NewContextAst), InnerAst]),
    {{WithAst, InfoValue}, TreeWalker1};
value_ast(ValueToken, [{{identifier,_,sudo}, true}|Args], AsString, Context, TreeWalker, ExtraArgs) ->
    NewContextAst = erl_syntax:application(erl_syntax:atom(z_acl),
                                           erl_syntax:atom(sudo),
                                           [z_context_ast(Context)]),
    ContextVarAst = erl_syntax:variable("WithContext_" ++ [$_|z_ids:identifier()]),
    LocalScope = [ {'$z_context', ContextVarAst} ],
    WithContext = Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]},
    {{InnerAst,InfoValue}, TreeWalker1} = value_ast(ValueToken, Args, AsString, WithContext, TreeWalker, ExtraArgs),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(ContextVarAst, NewContextAst), InnerAst]),
    {{WithAst, InfoValue}, TreeWalker1};
value_ast(ValueToken, [{{identifier,_,anondo}, true}|Args], AsString, Context, TreeWalker, ExtraArgs) ->
    NewContextAst = erl_syntax:application(erl_syntax:atom(z_acl),
                                           erl_syntax:atom(anondo),
                                           [z_context_ast(Context)]),
    ContextVarAst = erl_syntax:variable("WithContext_" ++ [$_|z_ids:identifier()]),
    LocalScope = [ {'$z_context', ContextVarAst} ],
    WithContext = Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]},
    {{InnerAst,InfoValue}, TreeWalker1} = value_ast(ValueToken, Args, AsString, WithContext, TreeWalker, ExtraArgs),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(ContextVarAst, NewContextAst), InnerAst]),
    {{WithAst, InfoValue}, TreeWalker1};
value_ast(ValueToken, [{{identifier,_,"z_language"}, Lang}|Args], AsString, Context, TreeWalker, ExtraArgs) ->
    {{LangAst,InfoValue1}, TreeWalker1} = erlydtl_compiler:value_ast(Lang, false, false, Context, TreeWalker),
    NewContextAst = erl_syntax:application(erl_syntax:atom(z_context), 
                                           erl_syntax:atom(set_language),
                                           [LangAst, z_context_ast(Context)]),
    ContextVarAst = erl_syntax:variable("WithContext_" ++ [$_|z_ids:identifier()]),
    LocalScope = [ {'$z_context', ContextVarAst} ],
    WithContext = Context#dtl_context{local_scopes=[LocalScope | Context#dtl_context.local_scopes]},
    {{InnerAst,InfoValue2}, TreeWalker2} = value_ast(ValueToken, Args, AsString, WithContext, TreeWalker1, ExtraArgs),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(ContextVarAst, NewContextAst), InnerAst]),
    {{WithAst, erlydtl_compiler:merge_info(InfoValue1,InfoValue2)}, TreeWalker2};
value_ast(ValueToken, [{{identifier,_,Var}, Value}|Args], AsString, Context, TreeWalker, ExtraArgs) ->
    {{ValueAst,InfoValue1}, TreeWalker1} = erlydtl_compiler:value_ast(Value, false, false, Context, TreeWalker),
    VarAst = erl_syntax:variable("WithContext_" ++ [$_|z_ids:identifier()]),
    WithContext = Context#dtl_context{local_scopes=[ [{Var, VarAst}] | Context#dtl_context.local_scopes]},
    {{InnerAst,InfoValue2}, TreeWalker2} = value_ast(ValueToken, Args, AsString, WithContext, TreeWalker1, [{Var, VarAst}|ExtraArgs]),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(VarAst, ValueAst), InnerAst]),
    {{WithAst, erlydtl_compiler:merge_info(InfoValue1,InfoValue2)}, TreeWalker2}.
