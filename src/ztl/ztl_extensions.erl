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
         setup_render_ast/2
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

scan(#scanner_state{ template="#" ++ T, pos={L, C}=P, scanned=Q}=S) ->
    {ok, S#scanner_state{ template=T, pos={L, C+1}, scanned=[{hash, P, "#"}|Q] }};
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
compile_ast({url, {identifier, _, Name}, Args}, Context, TreeWalker) ->
    url_ast(Name, Args, Context, TreeWalker);
compile_ast({atom_literal,Atom}, _Context, TreeWalker) ->
    {{erl_syntax:atom(Atom), #ast_info{}}, TreeWalker};
compile_ast({print, E}, Context, TreeWalker) ->
    print_ast(E, Context, TreeWalker);
compile_ast(_Ast, _Context, _TreeWalker) ->
    undefined.

custom_tag_ast(Context, TreeWalker) ->
    AppAst = erl_syntax:application(
               erl_syntax:atom(z_scomp),
               erl_syntax:atom(render),
               [   erl_syntax:variable("TagName"), 
                   erl_syntax:variable("_Variables"),
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


%%% ----------------------------------------------------------------------------
%%% Internal functions
%%% ----------------------------------------------------------------------------

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

keyword(url) -> url_keyword;
keyword(print) -> print_keyword;
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
    {{V, _, VarInfo}, TreeWalker1} = erlydtl_compiler:resolve_variable_ast({variable, Var}, Context, TreeWalker),
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

url_ast(Name, Args, Context, TreeWalker) ->
    %% Check if the 'escape' argument is there
    {{ArgsAst, ArgsInfo}, TreeWalker1} = list_args_ast(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
               erl_syntax:atom(z_dispatcher),
               erl_syntax:atom(url_for),
               [   erl_syntax:atom(Name), 
                   ArgsAst,
                   z_context_ast(Context)
               ]
              ),
    {{AppAst, ArgsInfo}, TreeWalker1}.  

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

list_args_ast(Args, Context, TreeWalker) ->
    {ArgsAst, TreeWalker1} = interpreted_args(Args, Context, TreeWalker),
    {PropListAst, AstInfo} = 
        lists:foldr(
          fun ({Key, {Ast, Info}}, {Acc, AccInfo}) ->
                  {[erl_syntax:tuple([erl_syntax:atom(Key), Ast])|Acc],
                   erlydtl_compiler:merge_info(Info, AccInfo)}
          end,
          {[], #ast_info{}},
          ArgsAst),
    {{erl_syntax:list(PropListAst), AstInfo}, TreeWalker1}.

interpreted_args(Args, Context, TreeWalker) ->
    lists:foldr(
      fun
          ({{identifier, _, postback}, {Literal, Pos, Value}}, {Acc, TW}) when Literal == string_literal; Literal == trans_literal ->
              %% string postbacks are always translated to atoms
                       {Ast, TW1} = erlydtl_compiler:value_ast({atom_literal, Pos, Value}, false, false, Context, TW),
                       {[{postback, Ast}|Acc], TW1};
          ({{identifier, _, Key}, Value}, {Acc, TW}) ->
              %% a normal key=value argument
                       {Ast, TW1} = erlydtl_compiler:value_ast(Value, false, false, Context, TW),
                       {[{Key, Ast}|Acc], TW1}
               end,
      {[], TreeWalker},
      Args).

print_ast(E, Context, TreeWalker) ->
    {{ValueAst,ValueInfo}, ValueTree} = erlydtl_compiler:value_ast(E, false, false, Context, TreeWalker),
    PrintAst = erl_syntax:application(
                erl_syntax:atom(io_lib),
                erl_syntax:atom(format),
                [   erl_syntax:string("~p"), 
                    erl_syntax:list([ValueAst])
                ]
            ),
    FlattenAst = erl_syntax:application(
                erl_syntax:atom(lists),
                erl_syntax:atom(flatten),
                [PrintAst]
            ),
    EscapeAst = erl_syntax:application(
                  erl_syntax:atom(mochiweb_html),
                  erl_syntax:atom(escape),
                  [FlattenAst]
            ),
    PreAst = erl_syntax:list([
                erl_syntax:string("<pre>"),
                EscapeAst,
                erl_syntax:string("</pre>")
            ]),
    {{PreAst, ValueInfo}, ValueTree}. 
