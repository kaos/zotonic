%%%-------------------------------------------------------------------
%%% File:      erlydtl_compiler.erl
%%% @author    Roberto Saccon <rsaccon@gmail.com> [http://rsaccon.com]
%%% @author    Evan Miller <emmiller@gmail.com>
%%% @copyright 2008 Roberto Saccon, Evan Miller
%%% @doc  
%%% ErlyDTL template compiler
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
%%% @since 2007-12-16 by Roberto Saccon, Evan Miller
%%%-------------------------------------------------------------------
-module(erlydtl_compiler).
-author('rsaccon@gmail.com').
-author('emmiller@gmail.com').

-include_lib("zophrenic.hrl").

%% --------------------------------------------------------------------
%% Definitions
%% --------------------------------------------------------------------
-export([compile/2, compile/3]).

-record(dtl_context, {
    local_scopes = [], 
    block_dict = dict:new(), 
    auto_escape = off, 
    parse_trail = [],
    vars = [],
    custom_tags_dir = [],
    reader = {file, read_file},
    finder = undefined,
    module = [],
    compiler_options = [verbose, report_errors],
    force_recompile = false}).

-record(ast_info, {
    dependencies = [],
    var_names = [],
    pre_render_asts = []}).
    
-record(treewalker, {
    counter = 0,
    has_auto_id = false,
    custom_tags = []}).    

compile(Binary, Module) when is_binary(Binary) ->
    compile(Binary, Module, []);

compile(File, Module) ->
    compile(File, Module, []).

compile(Binary, Module, Options) when is_binary(Binary) ->
    File = "",
    CheckSum = "",
    case parse(Binary) of
        {ok, DjangoParseTree} ->
            case compile_to_binary(File, DjangoParseTree, 
                    init_dtl_context(File, Module, Options), CheckSum) of
                {ok, Module1, _} ->
                    {ok, Module1};
                Err ->
                    Err
            end;
        Err ->
            Err
    end;
    
compile(File, Module, Options) ->  
    crypto:start(),
    Context = init_dtl_context(File, Module, Options),
    case parse(File, Context) of  
        ok ->
            ok;
        {ok, DjangoParseTree, CheckSum} ->
            case compile_to_binary(File, DjangoParseTree, Context, CheckSum) of
                {ok, Module1, Bin} ->
                    OutDir = proplists:get_value(out_dir, Options, "ebin"),       
                    BeamFile = filename:join([OutDir, atom_to_list(Module1) ++ ".beam"]),
                    case file:write_file(BeamFile, Bin) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            {error, lists:concat(["beam generation failed (", Reason, "): ", BeamFile])}
                    end;
                Err ->
                    Err
            end;
        {error, {ErrLineCol, ErrModule, ErrMsg}} ->
            {error, {ErrLineCol, ErrModule, lists:flatten(ErrMsg)}};
        Err ->
            Err
    end.
    

%%====================================================================
%% Internal functions
%%====================================================================

compile_to_binary(File, DjangoParseTree, Context, CheckSum) ->
    try body_ast(DjangoParseTree, Context, #treewalker{}) of
        {{Ast, Info}, TreeWalker} ->
            case compile:forms(forms(File, Context#dtl_context.module, Ast, Info, CheckSum, Context, TreeWalker), 
                    Context#dtl_context.compiler_options) of
                {ok, Module1, Bin} -> 
                    code:purge(Module1),
                    case code:load_binary(Module1, atom_to_list(Module1) ++ ".erl", Bin) of
                        {module, _} -> {ok, Module1, Bin};
                        _ -> {error, lists:concat(["code reload failed: ", Module1])}
                    end;
                error ->
                    {error, lists:concat(["compilation failed: ", File])};
                OtherError ->
                    OtherError
            end
    catch 
        throw:Error -> Error
    end.
                
init_dtl_context(File, Module, Options) when is_list(Module) ->
    init_dtl_context(File, list_to_atom(Module), Options);
init_dtl_context(File, Module, Options) ->
    Ctx = #dtl_context{},
    #dtl_context{
        local_scopes = [ [{'$autoid', erl_syntax:variable("AutoId_"++zp_ids:identifier())}] ],
        parse_trail = [File], 
        module = Module,
        custom_tags_dir = proplists:get_value(custom_tags_dir, Options, Ctx#dtl_context.custom_tags_dir),
        vars = proplists:get_value(vars, Options, Ctx#dtl_context.vars), 
        reader = proplists:get_value(reader, Options, Ctx#dtl_context.reader),
        finder = proplists:get_value(finder, Options, Ctx#dtl_context.finder),
        compiler_options = proplists:get_value(compiler_options, Options, Ctx#dtl_context.compiler_options),
        force_recompile = proplists:get_value(force_recompile, Options, Ctx#dtl_context.force_recompile)}.


is_up_to_date(_, #dtl_context{force_recompile = true}) ->
    false;
is_up_to_date(CheckSum, Context) ->
    Module = Context#dtl_context.module,
    {M,F}  = Context#dtl_context.reader,
    case catch Module:source() of
        {_, CheckSum} -> 
            case catch Module:dependencies() of
                L when is_list(L) ->
                    RecompileList = lists:foldl(fun
                            ({XFile, XCheckSum}, Acc) ->
                                case catch M:F(XFile) of
                                    {ok, Data} ->
                                        case binary_to_list(crypto:sha(Data)) of
                                            XCheckSum ->
                                                Acc;
                                            _ ->
                                                [recompile | Acc]
                                        end;
                                    _ ->
                                        [recompile | Acc]
                                end                                        
                        end, [], L),
                    case RecompileList of
                        [] -> true; 
                        _ -> false
                    end;
                _ ->
                    false
            end;
        _ ->
            false
    end.
    
    
parse(File, Context) ->  
    {M,F} = Context#dtl_context.reader,
    case catch M:F(File) of
        {ok, Data} ->
            CheckSum = binary_to_list(crypto:sha(Data)),
            parse(CheckSum, Data, Context);
        Error ->
            {error, io_lib:format("reading ~p failed (~p)", [File, Error])}  
    end.
        
parse(CheckSum, Data, Context) ->
    case is_up_to_date(CheckSum, Context) of
        true ->
            ok;
        _ ->
            case parse(Data) of
                {ok, Val} ->
                    {ok, Val, CheckSum};
                Err ->
                    Err
            end
    end.

parse(Data) ->
    case erlydtl_scanner:scan(binary_to_list(Data)) of
        {ok, Tokens} ->
            erlydtl_parser:parse(Tokens);
        Err ->
            Err
    end.        
  
forms(File, Module, BodyAst, BodyInfo, CheckSum, Context, TreeWalker) ->
    Function2 = erl_syntax:application(none, erl_syntax:atom(render2), 
        [erl_syntax:variable("Variables"), erl_syntax:variable("ZpContext")]),
    ClauseOk = erl_syntax:clause([erl_syntax:variable("Val")], none,
        [erl_syntax:tuple([erl_syntax:atom(ok), erl_syntax:variable("Val")])]),     
    ClauseCatch = erl_syntax:clause([erl_syntax:variable("Err")], none,
        [erl_syntax:tuple([erl_syntax:atom(error), erl_syntax:variable("Err")])]),            
    Render2FunctionAst = erl_syntax:function(erl_syntax:atom(render),
        [erl_syntax:clause([erl_syntax:variable("Variables"), erl_syntax:variable("ZpContext")], none, 
            [erl_syntax:try_expr([Function2], [ClauseOk], [ClauseCatch])])]),  
     
    SourceFunctionTuple = erl_syntax:tuple(
        [erl_syntax:string(File), erl_syntax:string(CheckSum)]),
    SourceFunctionAst = erl_syntax:function(
        erl_syntax:atom(source),
            [erl_syntax:clause([], none, [SourceFunctionTuple])]),
    
    DependenciesFunctionAst = erl_syntax:function(
        erl_syntax:atom(dependencies), [erl_syntax:clause([], none, 
            [erl_syntax:list(lists:map(fun 
                    ({XFile, XCheckSum}) -> 
                        erl_syntax:tuple([erl_syntax:string(XFile), erl_syntax:string(XCheckSum)])
                end, BodyInfo#ast_info.dependencies))])]),     

	BodyLanguageAst = erl_syntax:match_expr(
							erl_syntax:variable("Language"),
							erl_syntax:application(
						        erl_syntax:atom(zp_context), 
						        erl_syntax:atom(language),
						        [ erl_syntax:variable("ZpContext") ]
							)
					),

    BodyRenderAsts = case TreeWalker#treewalker.has_auto_id of
        false ->
            [BodyLanguageAst, BodyAst];
        true -> 
            AutoIdVar = resolve_scoped_variable_ast("$autoid", Context),
            BodyAutoIdAst = erl_syntax:match_expr(
                                    AutoIdVar,
                                    erl_syntax:application(
                                                erl_syntax:atom(zp_ids),
                                                erl_syntax:atom(identifier),
                                                [erl_syntax:integer(8)]
                                    )
                             ),
            [BodyAutoIdAst, BodyLanguageAst, BodyAst]
    end,

    RenderInternalFunctionAst = erl_syntax:function(
        erl_syntax:atom(render2), 
            [ erl_syntax:clause(
					[erl_syntax:variable("Variables"), erl_syntax:variable("ZpContext")], 
					none, 
                	BodyRenderAsts)
			]),   
    
    ModuleAst = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Module)]),
    
    ExportAst = erl_syntax:attribute(erl_syntax:atom(export),
        [erl_syntax:list([
					erl_syntax:arity_qualifier(erl_syntax:atom(render), erl_syntax:integer(2)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(source), erl_syntax:integer(0)),
                    erl_syntax:arity_qualifier(erl_syntax:atom(dependencies), erl_syntax:integer(0))])]),
    
    [erl_syntax:revert(X) || X <- [ModuleAst, ExportAst,
            Render2FunctionAst, SourceFunctionAst, DependenciesFunctionAst, RenderInternalFunctionAst
            | BodyInfo#ast_info.pre_render_asts]].    

        
% child templates should only consist of blocks at the top level
body_ast([{extends, {string_literal, _Pos, String}} | ThisParseTree], Context, TreeWalker) ->
    Extends = unescape_string_literal(String),
    case full_path(Extends, Context#dtl_context.finder) of
        {ok, File} ->
            case lists:member(File, Context#dtl_context.parse_trail) of
                true ->
                    throw({error, "Circular file inclusion: " ++ File});
                _ ->
                    case parse(File, Context) of
                        {ok, ParentParseTree, CheckSum} ->
                            BlockDict = lists:foldl(
                                fun
                                    ({block, {identifier, _, Name}, Contents}, Dict) ->
                                        dict:store(Name, Contents, Dict);
                                    (_, Dict) ->
                                        Dict
                                end, dict:new(), ThisParseTree),
                            with_dependency({File, CheckSum}, body_ast(ParentParseTree, Context#dtl_context{
                                block_dict = dict:merge(fun(_Key, _ParentVal, ChildVal) -> ChildVal end,
                                    BlockDict, Context#dtl_context.block_dict),
                                        parse_trail = [File | Context#dtl_context.parse_trail]}, TreeWalker));
                        Err ->
                            throw(Err)
                    end        
            end;
        {error, Reason} ->
            ?ERROR("body_ast: could not find template ~p (~p)", [Extends, Reason]),
            throw({error, "Could not find the template \" ++ Extends ++ \""}),
            {{erl_syntax:string(""), #ast_info{}}, TreeWalker}
    end;

    
body_ast(DjangoParseTree, Context, TreeWalker) ->
    {AstInfoList, TreeWalker2} = lists:mapfoldl(
        fun
            ({'block', {identifier, _, Name}, Contents}, TreeWalkerAcc) ->
                Block = case dict:find(Name, Context#dtl_context.block_dict) of
                    {ok, ChildBlock} ->
                        ChildBlock;
                    _ ->
                        Contents
                end,
                body_ast(Block, Context, TreeWalkerAcc);
            ({'comment', _Contents}, TreeWalkerAcc) ->
                empty_ast(TreeWalkerAcc);
			({'trans', {trans_text, _Pos, TransLiteral}}, TreeWalkerAcc) ->
				trans_ast(TransLiteral, Context, TreeWalkerAcc);
			({'trans_ext', {string_literal, _Pos, String}, Args}, TreeWalkerAcc) ->
				trans_ext_ast(String, Args, Context, TreeWalkerAcc);
            ({'date', 'now', {string_literal, _Pos, FormatString}}, TreeWalkerAcc) ->
                now_ast(FormatString, Context, TreeWalkerAcc);
            ({'autoescape', {identifier, _, OnOrOff}, Contents}, TreeWalkerAcc) ->
                body_ast(Contents, Context#dtl_context{auto_escape = list_to_atom(OnOrOff)}, 
                    TreeWalkerAcc);
            ({'text', _Pos, String}, TreeWalkerAcc) -> 
                string_ast(String, TreeWalkerAcc);
            ({'include', {string_literal, _, File}, Args, All}, TreeWalkerAcc) ->
                include_ast(unescape_string_literal(File), Args, All, Context, TreeWalkerAcc);
            ({'if', {'not', Variable}, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = empty_ast(TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(Contents, Context, TreeWalker1),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'if', Variable, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifelse', {'not', Variable}, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(ElseContents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(IfContents, Context, TreeWalker1),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context, TreeWalker2);                  
            ({'ifelse', Variable, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context, TreeWalker1),
                ifelse_ast(Variable, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifequal', Args, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(Contents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = empty_ast(TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifequalelse', Args, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(IfContents, Context, TreeWalkerAcc), 
                {ElseAstInfo, TreeWalker2} = body_ast(ElseContents, Context,TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);                
            ({'ifnotequal', Args, Contents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = empty_ast(TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(Contents, Context, TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);
            ({'ifnotequalelse', Args, IfContents, ElseContents}, TreeWalkerAcc) ->
                {IfAstInfo, TreeWalker1} = body_ast(ElseContents, Context, TreeWalkerAcc),
                {ElseAstInfo, TreeWalker2} = body_ast(IfContents, Context, TreeWalker1),
                ifequalelse_ast(Args, IfAstInfo, ElseAstInfo, Context, TreeWalker2);                    
            ({'with', [Expr, {'identifier', _, Identifier}], WithContents}, TreeWalkerAcc) ->
                with_ast(Expr, Identifier, WithContents, Context, TreeWalkerAcc);
            ({'for', {'in', IteratorList, Variable}, Contents}, TreeWalkerAcc) ->
                for_loop_ast(IteratorList, Variable, Contents, none, Context, TreeWalkerAcc);
            ({'for', {'in', IteratorList, Variable}, Contents, EmptyPartContents}, TreeWalkerAcc) ->
                for_loop_ast(IteratorList, Variable, Contents, EmptyPartContents, Context, TreeWalkerAcc);
            ({'load', Names}, TreeWalkerAcc) ->
                load_ast(Names, Context, TreeWalkerAcc);
            ({'tag', {'identifier', _, Name}, Args, All}, TreeWalkerAcc) ->
                tag_ast(Name, Args, All, Context, TreeWalkerAcc);
            ({'call', {'identifier', _, Name}}, TreeWalkerAcc) ->
            	call_ast(Name, TreeWalkerAcc);
            ({'call', {'identifier', _, Name}, With}, TreeWalkerAcc) ->
            	call_with_ast(Name, With, Context, TreeWalkerAcc);
            ({'cycle', Names}, TreeWalkerAcc) ->
                cycle_ast(Names, Context, TreeWalkerAcc);
            ({'cycle_compat', Names}, TreeWalkerAcc) ->
                cycle_compat_ast(Names, Context, TreeWalkerAcc);
            ({'image', Variable, Args}, TreeWalkerAcc) ->
                image_ast(Variable, Args, Context, TreeWalkerAcc);
            ({'image_url', Variable, Args}, TreeWalkerAcc) ->
                image_url_ast(Variable, Args, Context, TreeWalkerAcc);
            ({'url', {'identifier', _, Name}, Args}, TreeWalkerAcc) ->
                url_ast(Name, Args, Context, TreeWalkerAcc);
            ({'print', Value}, TreeWalkerAcc) ->
                print_ast(Value, Context, TreeWalkerAcc);
            (ValueToken, TreeWalkerAcc) -> 
                {{ValueAst,ValueInfo},ValueTreeWalker} = value_ast(ValueToken, true, Context, TreeWalkerAcc),
                {{format(ValueAst, Context),ValueInfo},ValueTreeWalker}
        end, TreeWalker, DjangoParseTree),
    
    {AstList, {Info, TreeWalker3}} = lists:mapfoldl(
        fun({Ast, Info}, {InfoAcc, TreeWalkerAcc}) -> 
                PresetVars = lists:foldl(fun
                        (X, Acc) ->
                            case proplists:lookup(list_to_atom(X), Context#dtl_context.vars) of
                                none ->
                                    Acc;
                                Val ->
                                    [erl_syntax:abstract(Val) | Acc]
                            end
                    end, [], Info#ast_info.var_names),
                case PresetVars of
                    [] ->
                        {Ast, {merge_info(Info, InfoAcc), TreeWalkerAcc}};
                    _ ->
                        Counter = TreeWalkerAcc#treewalker.counter,
                        Name = lists:concat([pre_render, Counter]),
                        Ast1 = erl_syntax:application(none, erl_syntax:atom(Name),
                            [erl_syntax:list(PresetVars)]),
                        PreRenderAst = erl_syntax:function(erl_syntax:atom(Name),
                            [erl_syntax:clause([erl_syntax:variable("Variables")], none, [Ast])]),
                        PreRenderAsts = Info#ast_info.pre_render_asts,
                        Info1 = Info#ast_info{pre_render_asts = [PreRenderAst | PreRenderAsts]},     
                        {Ast1, {merge_info(Info1, InfoAcc), TreeWalkerAcc#treewalker{counter = Counter + 1}}}
                end
        end, {#ast_info{}, TreeWalker2}, AstInfoList),
    {{erl_syntax:list(AstList), Info}, TreeWalker3}.


merge_info(Info1, Info2) ->
    #ast_info{dependencies = 
        lists:merge(
            lists:sort(Info1#ast_info.dependencies), 
            lists:sort(Info2#ast_info.dependencies)),
        var_names = 
            lists:merge(
                lists:sort(Info1#ast_info.var_names), 
                lists:sort(Info2#ast_info.var_names)),
        pre_render_asts = 
            lists:merge(
                Info1#ast_info.pre_render_asts,
                Info2#ast_info.pre_render_asts)}.


with_dependencies([], Args) ->
    Args;
with_dependencies([H, T], Args) ->
     with_dependencies(T, with_dependency(H, Args)).
        
with_dependency(FilePath, {{Ast, Info}, TreeWalker}) ->
    {{Ast, Info#ast_info{dependencies = [FilePath | Info#ast_info.dependencies]}}, TreeWalker}.


empty_ast(TreeWalker) ->
    {{erl_syntax:list([]), #ast_info{}}, TreeWalker}.


value_ast(ValueToken, AsString, Context, TreeWalker) ->
    case ValueToken of
        {'string_literal', _Pos, String} ->
            {{auto_escape(erl_syntax:string(unescape_string_literal(String)), Context), 
                    #ast_info{}}, TreeWalker};
		{'trans_literal', _Pos, String} ->
            {{auto_escape(trans_literal_ast(String), Context), 
                    #ast_info{}}, TreeWalker};
        {'number_literal', _Pos, Number} ->
            case AsString of
                true  -> string_ast(Number, TreeWalker);
                false -> {{erl_syntax:integer(list_to_integer(Number)), #ast_info{}}, TreeWalker}
            end;
        {'auto_id', Name} ->
            auto_id_ast(Name, Context, TreeWalker);
        {'apply_filter', Variable, Filter} ->
            filter_ast(Variable, Filter, Context, TreeWalker);
        {'attribute', _} = Variable ->
            {{Ast, VarName, VarInfo}, TreeWalker1} = resolve_variable_ast(Variable, Context, TreeWalker),
            {{Ast, merge_info(VarInfo,#ast_info{var_names = [VarName]})}, TreeWalker1};
        {'variable', _} = Variable ->
            {{Ast, VarName, VarInfo}, TreeWalker1} = resolve_variable_ast(Variable, Context, TreeWalker),
            {{Ast, merge_info(VarInfo, #ast_info{var_names = [VarName]})}, TreeWalker1};
        {'index_value', _, _} = Variable ->
            {{Ast, VarName, VarInfo}, TreeWalker1} = resolve_indexvariable_ast(Variable, Context, TreeWalker),
            {{Ast, merge_info(VarInfo, #ast_info{var_names = [VarName]})}, TreeWalker1};
        {model, {identifier, _, VarName}} = Model ->
            {{Ast, VarName, VarInfo}, TreeWalker1} = resolve_variable_ast(Model, Context, TreeWalker),
            {{Ast, merge_info(VarInfo, #ast_info{var_names = [VarName]})}, TreeWalker1};
        {tuple_value, {identifier, _, TupleName}, TupleArgs} ->
            TupleNameAst = erl_syntax:atom(TupleName),
            {TupleArgsAst, TreeWalker1} = scomp_ast_list_args(TupleArgs, Context, TreeWalker),
            {{erl_syntax:tuple([TupleNameAst, TupleArgsAst]), #ast_info{}}, TreeWalker1};
        {value_list, Values} ->
            {ValueAstList, ValueInfo, TreeWalker1} = lists:foldl(
                        fun(V, {Acc,Info,TreeW}) ->
                            {{Ast,InfoV}, TreeW1} = value_ast(V, false, Context, TreeW),
                            {[Ast|Acc], merge_info(Info,InfoV), TreeW1}
                        end,
                        {[], #ast_info{}, TreeWalker}, 
                        Values),
            {{erl_syntax:list(lists:reverse(ValueAstList)), ValueInfo},TreeWalker1}
    end.

string_ast(String, TreeWalker) ->
    % {{erl_syntax:string(String), #ast_info{}}, TreeWalker}. %% less verbose AST, better for development and debugging
    {{erl_syntax:binary([erl_syntax:binary_field(erl_syntax:integer(X)) || X <- String]), #ast_info{}}, TreeWalker}.       


include_ast(File, Args, All, Context, TreeWalker) ->
    UseScomp = lists:foldl( fun({{identifier, _, Key}, _}, IsC) -> 
                                case Key of
                                    "maxage" -> true;
                                    "vary"   -> true;
                                    "scomp"  -> true;
                                    "visible_for" -> true;
                                    _ -> IsC
                                end
                            end,
                            false,
                            Args),
    case UseScomp of
        false ->
            {InterpretedArgs, TreeWalker1} = interpreted_args(Args, Context, TreeWalker),
            {ScopedArgs, ArgAsts} = lists:foldr(
                fun({AKey, AAst}, {ScopeAcc, AstAcc}) ->
                    Var = "Arg_" ++ zp_ids:identifier(10),
                    AssignAst = erl_syntax:match_expr(erl_syntax:variable(Var), AAst),
                    { [{AKey, erl_syntax:variable(Var)}|ScopeAcc], [AssignAst|AstAcc] }
                end,
                {[], []},
                InterpretedArgs),
            
            % {AstList, Info, TreeWalker}
            IncludeFun = fun(FilePath, {AstList, InclInfo, TreeW}) ->
                    case parse(FilePath, Context) of
                        {ok, InclusionParseTree, CheckSum} ->
                            AutoIdVar = "AutoId_"++zp_ids:identifier(),
                            IncludeScope = [ {'$autoid', erl_syntax:variable(AutoIdVar)} | ScopedArgs ],

                            {{Ast,Info}, InclTW2} = 
                                            with_dependency({FilePath, CheckSum}, 
                                                    body_ast(
                                                        InclusionParseTree,
                                                        Context#dtl_context{
                                                                local_scopes = [ IncludeScope | Context#dtl_context.local_scopes ],
                                                                parse_trail = [FilePath | Context#dtl_context.parse_trail]}, 
                                                        TreeW#treewalker{has_auto_id=false})),
                            Ast1 = case InclTW2#treewalker.has_auto_id of
                                false -> Ast;
                                true ->  erl_syntax:block_expr(
                                            [
                                            erl_syntax:match_expr(
                                                    erl_syntax:variable(AutoIdVar), 
                                                    erl_syntax:application(
                                                        erl_syntax:atom(zp_ids),
                                                        erl_syntax:atom(identifier),
                                                        [])),
                                            Ast])
                            end,
                            {[Ast1|AstList], merge_info(InclInfo, Info), InclTW2#treewalker{has_auto_id=TreeW#treewalker.has_auto_id}};
                        Err ->
                            throw(Err)
                    end
            end,
            
            % Compile all included files, put them in a block expr with a single assignment of the argument vars at the start.
            case lists:foldl(IncludeFun, {[], #ast_info{}, TreeWalker1}, full_path(File, All, Context#dtl_context.finder)) of
                {[], _, TreeWalkerN} ->
                    case All of
                        false -> ?LOG("include_ast: could not find template ~p", [File]);
                        true -> ok
                    end,
                    {{erl_syntax:string(""), #ast_info{}}, TreeWalkerN};
                {AstList, AstInfo, TreeWalkerN} ->
                    AstN = erl_syntax:block_expr(ArgAsts ++ lists:reverse(AstList)),
                    {{AstN, AstInfo}, TreeWalkerN}
            end;
        true ->
            Args1 = [{{identifier, none, "file"},{string_literal, none, File}} | Args],
            scomp_ast("include", Args1, All, Context, TreeWalker)
    end.


filter_ast(Variable, Filter, Context, TreeWalker) ->
    % the escape filter is special; it is always applied last, so we have to go digging for it

    % AutoEscape = 'did' means we (will have) decided whether to escape the current variable,
    % so don't do any more escaping
    {{UnescapedAst, Info}, TreeWalker2} = filter_ast_noescape(Variable, Filter, Context#dtl_context{auto_escape = did}, TreeWalker),
    case search_for_escape_filter(Variable, Filter, Context) of
        on ->
            {{erl_syntax:application(
                    erl_syntax:atom(erlydtl_filters), 
                    erl_syntax:atom(force_escape), 
                    [UnescapedAst]), 
                Info}, TreeWalker2};
        _ ->
            {{UnescapedAst, Info}, TreeWalker2}
    end.

filter_ast_noescape(Variable, [{identifier, _, "escape"}], Context, TreeWalker) ->
    value_ast(Variable, true, Context, TreeWalker);
filter_ast_noescape(Variable, Filter, Context, TreeWalker) ->
    {{VariableAst,Info},TreeWalker2} = value_ast(Variable, true, Context, TreeWalker),
    {{FilterAst,Info2},TreeWalker3} = filter_ast1(Filter, VariableAst, Context, TreeWalker2),
    {{FilterAst, merge_info(Info, Info2)}, TreeWalker3}.

filter_ast1([{identifier, _, Name}], VariableAst, _Context, TreeWalker) ->
    FilterAst = erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(Name), [VariableAst]),
    {{FilterAst, #ast_info{}}, TreeWalker};
filter_ast1([{identifier, _, Name}, Arg], VariableAst, Context, TreeWalker) ->
    {{ArgAst, Info},TreeWalker2} = value_ast(Arg, false, Context, TreeWalker),
    FilterAst = erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(Name), [VariableAst, ArgAst]),
    {{FilterAst, Info}, TreeWalker2}.
    
 
search_for_escape_filter(_, _, #dtl_context{auto_escape = on}) ->
    on;
search_for_escape_filter(_, _, #dtl_context{auto_escape = did}) ->
    off;
search_for_escape_filter(Variable, Filter, _) ->
    search_for_escape_filter(Variable, Filter).

search_for_escape_filter(_, [{identifier, _, "escape"}]) ->
    on;
search_for_escape_filter({apply_filter, Variable, Filter}, _) ->
    search_for_escape_filter(Variable, Filter);
search_for_escape_filter(_Variable, _Filter) ->
    off.


resolve_variable_ast(VarTuple, Context, TreeWalker) ->
    opttrans_variable_ast(resolve_variable_ast(VarTuple, Context, TreeWalker, 'fetch_value')).

resolve_ifvariable_ast(VarTuple, Context, TreeWalker) ->
    opttrans_variable_ast(resolve_variable_ast(VarTuple, Context, TreeWalker, 'find_value')).

resolve_indexvariable_ast(VarTuple, Context, TreeWalker) ->
    opttrans_variable_ast(resolve_variable_ast(VarTuple, Context, TreeWalker, 'fetch_value')).


opttrans_variable_ast({{Ast, VarName, Info}, TreeWalker}) ->
    Ast1 = erl_syntax:application(
            erl_syntax:atom(erlydtl_filters), 
            erl_syntax:atom(opttrans),
			[
				Ast,
				erl_syntax:variable("Language")
			]),
	{{Ast1, VarName, Info}, TreeWalker}.

resolve_variable_ast({model, {identifier, _, VarName}}, _Context, TreeWalker, _FinderFunction) ->
    Ast = erl_syntax:tuple([
            erl_syntax:atom(m),
            erl_syntax:atom("m_" ++ VarName),
            erl_syntax:atom(undefined)
        ]),
    {{Ast, "m", #ast_info{}}, TreeWalker};

resolve_variable_ast({index_value, Variable, Index}, Context, TreeWalker, FinderFunction) ->
    {{IndexAst,Info},TreeWalker2} = value_ast(Index, false, Context, TreeWalker),
    {{VarAst, VarName, Info2}, TreeWalker3} = resolve_variable_ast(Variable, Context, TreeWalker2, FinderFunction),
    Ast = erl_syntax:application(
            erl_syntax:atom(erlydtl_runtime), 
            erl_syntax:atom(FinderFunction),
            [IndexAst, VarAst, erl_syntax:variable("ZpContext")]),
    {{Ast, VarName, merge_info(Info, Info2)}, TreeWalker3};
           
resolve_variable_ast({attribute, {{identifier, _, AttrName}, Variable}}, Context, TreeWalker, FinderFunction) ->
    {{VarAst, VarName, Info}, TreeWalker2} = resolve_variable_ast(Variable, Context, TreeWalker, FinderFunction),
    Ast = erl_syntax:application(
            erl_syntax:atom(erlydtl_runtime),
            erl_syntax:atom(FinderFunction),
            [erl_syntax:atom(AttrName), VarAst, erl_syntax:variable("ZpContext")]),
    {{Ast, VarName, Info}, TreeWalker2};

resolve_variable_ast({variable, {identifier, _, VarName}}, Context, TreeWalker, FinderFunction) ->
    Ast = case resolve_scoped_variable_ast(VarName, Context) of
        undefined ->
            erl_syntax:application(
                erl_syntax:atom(erlydtl_runtime), 
                erl_syntax:atom(FinderFunction),
                [erl_syntax:atom(VarName), erl_syntax:variable("Variables"), erl_syntax:variable("ZpContext")]);
        Val ->
            Val
    end,
    {{Ast, VarName, #ast_info{}}, TreeWalker};

resolve_variable_ast({apply_filter, Variable, Filter}, Context, TreeWalker, FinderFunction) ->
    {{VarAst, VarName, Info}, TreeWalker2} = resolve_variable_ast(Variable, Context, TreeWalker, FinderFunction),
    ValueAst = erl_syntax:application(
            erl_syntax:atom(erlydtl_runtime),
            erl_syntax:atom(to_value),
            [VarAst, erl_syntax:variable("ZpContext")]
        ),
    {{VarValue, Info2}, TreeWalker3} = filter_ast1(Filter, ValueAst, Context, TreeWalker2),
    {{VarValue, VarName, merge_info(Info, Info2)}, TreeWalker3};

resolve_variable_ast(What, _Context, TreeWalker, _FinderFunction) ->
   error_logger:error_msg("~p:resolve_variable_ast unhandled: ~p~n", [?MODULE, What]),
   {{erl_syntax:atom(undefined), "$error", #ast_info{}}, TreeWalker}.

resolve_scoped_variable_ast(VarName, Context) ->
    lists:foldl(fun(Scope, Value) ->
                case Value of
                    undefined -> proplists:get_value(list_to_atom(VarName), Scope);
                    _ -> Value
                end
        end, undefined, Context#dtl_context.local_scopes).


format(Ast, Context) ->
    auto_escape(stringify(Ast), Context).

stringify(Ast) ->
    erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(stringify),
        [Ast]).

auto_escape(Value, Context) ->
    case Context#dtl_context.auto_escape of
        on ->
            erl_syntax:application(erl_syntax:atom(erlydtl_filters), erl_syntax:atom(force_escape),
                [Value]);
        _ ->
            Value
    end.


ifelse_ast(Variable, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    VarNames = Info#ast_info.var_names,
    {{Ast, VarName, VarInfo}, TreeWalker1} = resolve_ifvariable_ast(Variable, Context, TreeWalker),
    {{erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(is_false), [Ast]),
        [erl_syntax:clause([erl_syntax:atom(true)], none, 
                [ElseContentsAst]),
            erl_syntax:clause([erl_syntax:underscore()], none,
                [IfContentsAst])
        ]), merge_info(VarInfo, Info#ast_info{var_names = [VarName | VarNames]})}, TreeWalker1}.

        
ifequalelse_ast(Args, {IfContentsAst, IfContentsInfo}, {ElseContentsAst, ElseContentsInfo}, Context, TreeWalker) ->
    Info = merge_info(IfContentsInfo, ElseContentsInfo),
    {[Arg1Ast, Arg2Ast], VarNames, Info1, TreeWalker1} = lists:foldl(fun
            (X, {Asts, AccVarNames, Inf, TW}) ->
                case X of
					{string_literal, _, Literal} ->
					    {[erl_syntax:string(unescape_string_literal(Literal)) | Asts], AccVarNames, Inf, TW};
				    {trans_literal, _, Literal} ->
				        {[trans_literal_ast(Literal) | Asts], AccVarNames, Inf, TW};
                    {number_literal, _, Literal} ->
                        {[erl_syntax:integer(list_to_integer(Literal)) | Asts], AccVarNames, Inf, TW};
                    Variable ->
                        {{Ast, VarName, VarInfo}, TW1} = resolve_ifvariable_ast(Variable, Context, TW),
                        {[Ast | Asts], [VarName | AccVarNames], merge_info(Inf, VarInfo), TW1}
                end                
        end,
        {[], Info#ast_info.var_names, #ast_info{}, TreeWalker},
        Args),
    Ast = erl_syntax:case_expr(erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(are_equal),
            [Arg1Ast, Arg2Ast]),
        [
            erl_syntax:clause([erl_syntax:atom(true)], none, [IfContentsAst]),
            erl_syntax:clause([erl_syntax:underscore()], none, [ElseContentsAst])
        ]),
    {{Ast, merge_info(Info1, Info#ast_info{var_names = VarNames})}, TreeWalker1}.         


with_ast(Value, Variable, Contents, Context, TreeWalker) ->
    Postfix = zp_ids:identifier(),
    VarName = "With_" ++ Variable ++ [$_|Postfix],
    VarAst = erl_syntax:variable(VarName),
    {{ValueAst, ValueInfo}, TreeWalker1} = value_ast(Value, false, Context, TreeWalker),
    {{InnerAst, InnerInfo}, TreeWalker2} = body_ast(
            Contents,
            Context#dtl_context{local_scopes=[[{list_to_atom(Variable), VarAst}] | Context#dtl_context.local_scopes]}, 
            TreeWalker1),
    WithAst = erl_syntax:block_expr([erl_syntax:match_expr(VarAst, ValueAst), InnerAst]),
    {{WithAst, merge_info(ValueInfo,InnerInfo)}, TreeWalker2}.
    
    
for_loop_ast(IteratorList, Variable, Contents, EmptyPartContents, Context, TreeWalker) ->
    Vars = lists:map(fun({identifier, _, Iterator}) -> 
                    erl_syntax:variable("Var_" ++ Iterator) 
            end, IteratorList),
    {{InnerAst, Info}, TreeWalker2} = body_ast(Contents,
        Context#dtl_context{local_scopes = [
                [{'forloop', erl_syntax:variable("Counters")} | lists:map(
                    fun({identifier, _, Iterator}) ->
                            {list_to_atom(Iterator), erl_syntax:variable("Var_" ++ Iterator)} 
                    end, IteratorList)] | Context#dtl_context.local_scopes]}, TreeWalker),
    CounterAst = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), 
        erl_syntax:atom(increment_counter_stats), [erl_syntax:variable("Counters")]),
    {{VarAst, VarName, VarInfo}, TreeWalker3} = resolve_variable_ast(Variable, Context, TreeWalker2),
    ListAst = erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(to_list), [VarAst, erl_syntax:variable("ZpContext")]),
    CounterVars0 = case resolve_scoped_variable_ast("forloop", Context) of
        undefined ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [ListAst]);
        Value ->
            erl_syntax:application(erl_syntax:atom(erlydtl_runtime), erl_syntax:atom(init_counter_stats), [ListAst, Value])
    end,

    ForLoopF = fun(BaseListAst) -> erl_syntax:application(
            erl_syntax:atom('erlang'), erl_syntax:atom('element'),
            [erl_syntax:integer(1), erl_syntax:application(
                    erl_syntax:atom('lists'), erl_syntax:atom('mapfoldl'),
                    [erl_syntax:fun_expr([
                                erl_syntax:clause([erl_syntax:tuple(Vars), erl_syntax:variable("Counters")], none, 
                                    [erl_syntax:tuple([InnerAst, CounterAst])]),
                                erl_syntax:clause(case Vars of [H] -> [H, erl_syntax:variable("Counters")];
                                        _ -> [erl_syntax:list(Vars), erl_syntax:variable("Counters")] end, none, 
                                    [erl_syntax:tuple([InnerAst, CounterAst])])
                            ]),
                        CounterVars0, BaseListAst])])
    end,

    {CompleteForLoopAst, Info2, TreeWalker4} = case EmptyPartContents of
        none ->
            {ForLoopF(ListAst), Info, TreeWalker3};
        _ -> 
            {{EmptyPartAst, EmptyPartInfo}, EmptyWalker} = body_ast(EmptyPartContents, Context, TreeWalker3),
            LAst = erl_syntax:variable("L"),
            EmptyClauseAst = erl_syntax:clause(
        		 [erl_syntax:list([])], 
        		 none,
        		 [EmptyPartAst]),
            LoopClauseAst = erl_syntax:clause(
                [LAst],
                none,
                [ForLoopF(LAst)]),
            {erl_syntax:case_expr(ListAst, [EmptyClauseAst, LoopClauseAst]), merge_info(Info,EmptyPartInfo), EmptyWalker} 
    end,
    {{CompleteForLoopAst, merge_info(VarInfo, Info2#ast_info{var_names = [VarName]})}, TreeWalker4}.

load_ast(Names, _Context, TreeWalker) ->
    CustomTags = lists:merge([X || {identifier, _ , X} <- Names], TreeWalker#treewalker.custom_tags),
    {{erl_syntax:list([]), #ast_info{}}, TreeWalker#treewalker{custom_tags = CustomTags}}.  

cycle_ast(Names, Context, TreeWalker) ->
    {NamesTuple, TreeWalker1} = lists:foldr(
                        fun
                        ({string_literal, _, Str}, {Acc,TW}) ->
                            {[ erl_syntax:string(unescape_string_literal(Str)) | Acc], TW};
						({trans_literal, _, Str}, {Acc,TW}) ->
						  	{[ trans_literal_ast(Str) | Acc ], TW};
                        ({number_literal, _, Num}, {Acc,TW}) ->
                            V = format(erl_syntax:integer(Num), Context),
                            {[ V | Acc ], TW};
                        ({variable, _}=Var, {Acc,TW}) ->
                            {{V, _VarName, _VarInfo},TW2}  = resolve_variable_ast(Var, Context, TW),
                            {[ V | Acc ], TW2};
                        ({auto_id, Name}, {Acc,TW}) ->
                            {{V, _}, TW1} = auto_id_ast(Name, Context, TW),
                            {[ V |Acc ], TW1};
                        (_, {Acc,TW}) ->
                           {[ erl_syntax:atom(undefined) | Acc ], TW}
                        end,
                        {[],TreeWalker},
                        Names),

    {{erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), erl_syntax:variable("Counters"), erl_syntax:variable("ZpContext")]), #ast_info{}}, TreeWalker1}.

%% Older Django templates treat cycle with comma-delimited elements as strings
cycle_compat_ast(Names, _Context, TreeWalker) ->
    NamesTuple = [erl_syntax:string(X) || {identifier, _, X} <- Names],
    {{erl_syntax:application(
        erl_syntax:atom('erlydtl_runtime'), erl_syntax:atom('cycle'),
        [erl_syntax:tuple(NamesTuple), erl_syntax:variable("Counters")]), #ast_info{}}, TreeWalker}.


%% @author Marc Worrell
%% @doc Output the trans record with the translation call to zp_trans 
%% @todo Optimization for the situation where all parameters are constants
trans_ast(TransLiteral, _Context, TreeWalker) ->
	% Remove the first and the last character, these were separating the string from the {_ and _} tokens
	Lit = lists:reverse(tl(lists:reverse(tl(TransLiteral)))),
	{{erl_syntax:application(
		erl_syntax:atom(zp_trans),
		erl_syntax:atom(trans),
		[
			erl_syntax:tuple([
				erl_syntax:atom(trans),
				erl_syntax:list([
					erl_syntax:tuple([ erl_syntax:atom('en'), erl_syntax:string(Lit) ])
				])
			]),
			erl_syntax:variable("Language")
		]
	), #ast_info{}}, TreeWalker}.


trans_ext_ast(String, Args, Context, TreeWalker) ->
	Lit = unescape_string_literal(string:strip(String, both, 34), [], noslash),
	{ArgsTrans, TreeWalker1} = interpreted_args(Args, Context, TreeWalker),
	ArgsTransAst = [
		erl_syntax:tuple([erl_syntax:atom(Lang), Ast]) || {Lang,Ast} <- ArgsTrans
	],
	{{erl_syntax:application(
		erl_syntax:atom(zp_trans),
		erl_syntax:atom(trans),
		[
			erl_syntax:tuple([
				erl_syntax:atom(trans),
				erl_syntax:list([
					erl_syntax:tuple([erl_syntax:atom('en'), erl_syntax:string(Lit)]) | ArgsTransAst
				])
			]),
			erl_syntax:variable("Language")
		]
	), #ast_info{}}, TreeWalker1}.
	


now_ast(FormatString, _Context, TreeWalker) ->
    % Note: we can't use unescape_string_literal here
    % because we want to allow escaping in the format string.
    % We only want to remove the surrounding escapes,
    % i.e. \"foo\" becomes "foo"
    UnescapeOuter = string:strip(FormatString, both, 34),
    {{erl_syntax:application(
        erl_syntax:atom(erlydtl_dateformat),
        erl_syntax:atom(format),
        [erl_syntax:string(UnescapeOuter)]),
        #ast_info{}}, TreeWalker}.

unescape_string_literal(String) ->
    unescape_string_literal(string:strip(String, both, 34), [], noslash).

unescape_string_literal([], Acc, noslash) ->
    lists:reverse(Acc);
unescape_string_literal([$\\ | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, Acc, slash);
unescape_string_literal([C | Rest], Acc, noslash) ->
    unescape_string_literal(Rest, [C | Acc], noslash);
unescape_string_literal("n" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\n | Acc], noslash);
unescape_string_literal("r" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\r | Acc], noslash);
unescape_string_literal("t" ++ Rest, Acc, slash) ->
    unescape_string_literal(Rest, [$\t | Acc], noslash);
unescape_string_literal([C | Rest], Acc, slash) ->
    unescape_string_literal(Rest, [C | Acc], noslash).


full_path(File, FinderFun) ->
    case full_path(File, false, FinderFun) of
        [Filename] -> {ok, Filename};
        [] -> {error, enoent}
    end.

full_path(File, All, FinderFun) ->
    FinderFun(File, All).



%%-------------------------------------------------------------------
%% Custom tags
%%-------------------------------------------------------------------

tag_ast(Name, Args, All, Context, TreeWalker) ->
    case lists:member(Name, TreeWalker#treewalker.custom_tags) of
        true ->
            {InterpretedArgs, TreeWalker1} = interpreted_args(Args, Context, TreeWalker),
            DefaultFilePath = filename:join([erlydtl_deps:get_base_dir(), "priv", "custom_tags", Name]),
            case Context#dtl_context.custom_tags_dir of
                [] ->
                    case parse(DefaultFilePath, Context) of
                        {ok, TagParseTree, CheckSum} ->
                            tag_ast2({DefaultFilePath, CheckSum}, TagParseTree, InterpretedArgs, Context, TreeWalker1);
                        _ ->
                            Reason = lists:concat(["Loading tag source for '", Name, "' failed: ", 
                                DefaultFilePath]),
                            throw({error, Reason})
                    end;
                _ ->
                    CustomFilePath = filename:join([Context#dtl_context.custom_tags_dir, Name]),
                    case parse(CustomFilePath, Context) of
                        {ok, TagParseTree, CheckSum} ->
                            tag_ast2({CustomFilePath, CheckSum},TagParseTree, InterpretedArgs, Context, TreeWalker1);
                        _ ->
                            case parse(DefaultFilePath, Context) of
                                {ok, TagParseTree, CheckSum} ->
                                    tag_ast2({DefaultFilePath, CheckSum}, TagParseTree, InterpretedArgs, Context, TreeWalker1);
                                _ ->
                                    Reason = lists:concat(["Loading tag source for '", Name, "' failed: ", 
                                        CustomFilePath, ", ", DefaultFilePath]),
                                    throw({error, Reason})
                            end
                    end
            end;
        _ ->
            % Dynamic scomp call
            % throw({error, lists:concat(["Custom tag '", Name, "' not loaded"])})
            scomp_ast(Name, Args, All, Context, TreeWalker)
    end.
 
 tag_ast2({Source, _} = Dep, TagParseTree, InterpretedArgs, Context, TreeWalker) ->
    with_dependency(Dep, body_ast(TagParseTree, Context#dtl_context{
        local_scopes = [ InterpretedArgs | Context#dtl_context.local_scopes ],
        parse_trail = [ Source | Context#dtl_context.parse_trail ]}, TreeWalker)).


call_ast(Module, TreeWalkerAcc) ->
    call_ast(Module, erl_syntax:variable("Variables"), #ast_info{}, TreeWalkerAcc).

call_with_ast(Module, Variable, Context, TreeWalker) ->
    {{VarAst, VarName, VarInfo}, TreeWalker1} = resolve_variable_ast(Variable, Context, TreeWalker),
    call_ast(Module, VarAst, merge_info(VarInfo, #ast_info{var_names=[VarName]}), TreeWalker1).
        
call_ast(Module, Variable, AstInfo, TreeWalker) ->
     AppAst = erl_syntax:application(
		erl_syntax:atom(Module),
		erl_syntax:atom(render),
		[Variable]),
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
    Module2 = list_to_atom(Module),
    with_dependencies(Module2:dependencies(), {{CallAst, AstInfo}, TreeWalker}).


%% @author Marc Worrell
%% @doc Generate an image tag based on the image name and the arguments
%% @todo Optimization for the situation where all parameters are constants
image_ast(FilenameValue, Args, Context, TreeWalker) ->
    FilenameAst = resolve_value_ast(FilenameValue, Context, TreeWalker),
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                        erl_syntax:atom(zp_media_tag),
                        erl_syntax:atom(tag),
                        [   FilenameAst,
                            ArgsAst
                        ]
                    ),
    RenderedAst = erl_syntax:variable("Rendered"),
    OkAst = erl_syntax:clause(
                      [erl_syntax:tuple([erl_syntax:atom(tag), RenderedAst])], 
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
    {{CallAst, #ast_info{}}, TreeWalker1}.


%% @author Marc Worrell
%% @doc Generate an image url based on the image name and the arguments
%% @todo Optimization for the situation where all parameters are constants
image_url_ast(FilenameValue, Args, Context, TreeWalker) ->
    FilenameAst = resolve_value_ast(FilenameValue, Context, TreeWalker),
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                        erl_syntax:atom(zp_media_tag),
                        erl_syntax:atom(url),
                        [   FilenameAst,
                            ArgsAst
                        ]
                    ),
    RenderedAst = erl_syntax:variable("Rendered"),
    OkAst = erl_syntax:clause(
                      [erl_syntax:tuple([erl_syntax:atom(url), RenderedAst])], 
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
    {{CallAst, #ast_info{}}, TreeWalker1}.
    

%% Added by Marc Worrell - handle url generation using the url patterns
url_ast(Name, Args, Context, TreeWalker) ->
    % Check if the 'escape' argument is there
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                erl_syntax:atom(zp_dispatcher),
                erl_syntax:atom(url_for),
                [   erl_syntax:atom(Name), 
                    ArgsAst,
                    erl_syntax:variable("ZpContext")
                ]
            ),
    {{AppAst, #ast_info{}}, TreeWalker1}.  



print_ast(Value, Context, TreeWalker) ->
    ValueAst = resolve_value_ast(Value, Context, TreeWalker),
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
    {{PreAst, #ast_info{}}, TreeWalker}. 


resolve_value_ast(Value, Context, TreeWalker) ->
    {{Ast,_Info},_TreeWalker} = value_ast(Value, false, Context, TreeWalker),
    Ast.


%% Added by Marc Worrell - handle evaluation of scomps by zp_scomp
scomp_ast(ScompName, Args, false = _All, Context, TreeWalker) ->
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                erl_syntax:atom(zp_scomp),
                erl_syntax:atom(render),
                [   erl_syntax:atom(ScompName), 
                    ArgsAst,
                    erl_syntax:variable("Variables"),
                    erl_syntax:variable("ZpContext")
                ]
            ),
    RenderedAst = erl_syntax:variable("Rendered"),
    CleanedAst = erl_syntax:application(
                erl_syntax:atom(zp_context),
                erl_syntax:atom(prune_for_template),
                [RenderedAst]
            ),
    OkAst = erl_syntax:clause(
                      [erl_syntax:tuple([erl_syntax:atom(ok), RenderedAst])], 
                      none,
                      [CleanedAst]),
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
    {{CallAst, #ast_info{}}, TreeWalker1};
scomp_ast(ScompName, Args, true, Context, TreeWalker) ->
    {ArgsAst, TreeWalker1} = scomp_ast_list_args(Args, Context, TreeWalker),
    AppAst = erl_syntax:application(
                erl_syntax:atom(zp_scomp),
                erl_syntax:atom(render_all),
                [   erl_syntax:atom(ScompName), 
                    ArgsAst,
                    erl_syntax:variable("Variables"),
                    erl_syntax:variable("ZpContext")
                ]
            ),
    {{AppAst, #ast_info{}}, TreeWalker1}.


scomp_ast_list_args(Args, Context, TreeWalker) ->
    {ArgsAst, TreeWalker1}= interpreted_args(Args, Context, TreeWalker),
    PropListAst = [ erl_syntax:tuple([erl_syntax:atom(A), B]) || {A,B} <- ArgsAst ],
    { erl_syntax:list(PropListAst), TreeWalker1}.


%%  lists:append(AutoId,"-Name")
auto_id_ast({identifier, _, Name}, Context, TreeWalker) ->
    {{   erl_syntax:application(
                    erl_syntax:atom(lists), erl_syntax:atom(append),
                    [resolve_scoped_variable_ast("$autoid", Context), erl_syntax:string([$-|Name])]),
        #ast_info{}
    }, TreeWalker#treewalker{has_auto_id=true}};

auto_id_ast({{identifier, _, Name}, {identifier, _, _} = Var}, Context, TreeWalker) ->
    {{V, _, VarInfo}, TreeWalker1} = resolve_variable_ast({variable, Var}, Context, TreeWalker),
    {{   erl_syntax:application(
                    erl_syntax:atom(lists), erl_syntax:atom(append),
                    [   
                        erl_syntax:list([
                            resolve_scoped_variable_ast("$autoid", Context), 
                            erl_syntax:string([$-|Name]++"-"),
                            erl_syntax:application(
                                erl_syntax:atom(zp_convert),
                                erl_syntax:atom(to_list),
                                [V])
                        ])
                    ]),
       VarInfo
    }, TreeWalker1#treewalker{has_auto_id=true}}.


interpreted_args(Args, Context, TreeWalker) ->
    lists:foldr(
        fun
            ({{identifier, _, "postback"}, {Literal, _, Value}}, {Acc, TW}) when Literal == string_literal; Literal == trans_literal ->
                % string postbacks are always translated to atoms
                { [ {list_to_atom("postback"), erl_syntax:atom(unescape_string_literal(Value))} | Acc ], TW };
            ({{identifier, _, Key}, Value}, {Acc, TW}) ->
                % a normal key=value argument
                {Ast, TW1} = interpreted_argval(Value, Context, TW),
                { [ {list_to_atom(Key), Ast} | Acc ], TW1 }
        end,
        {[], TreeWalker},
        Args).

interpreted_argval({number_literal, _, Value}, _Context, TreeWalker) -> 
    {erl_syntax:integer(list_to_integer(Value)), TreeWalker};
interpreted_argval({string_literal, _, Value}, _Context, TreeWalker) -> 
    {erl_syntax:string(unescape_string_literal(Value)), TreeWalker};
interpreted_argval({trans_literal, _, Value}, _Context, TreeWalker) ->
    {trans_literal_ast(Value), TreeWalker};
interpreted_argval({auto_id, Name}, Context, TreeWalker) ->
    {{V, _}, TreeWalker1} = auto_id_ast(Name, Context, TreeWalker), 
    {V, TreeWalker1};
interpreted_argval({tuple_value, {identifier, _, TupleName}, TupleArgs}, Context, TreeWalker) ->
    {ArgList, TreeWalker1} = scomp_ast_list_args(TupleArgs, Context, TreeWalker),
    {erl_syntax:tuple([erl_syntax:atom(TupleName), ArgList]), TreeWalker1};
interpreted_argval({value_list, Values}, Context, TreeWalker) ->
    {List, TreeWalker1} = lists:foldr(
        fun(V, {Acc, TW}) -> 
            {VAst, TW1} = interpreted_argval(V, Context, TW),
            {[VAst|Acc], TW1}
        end,
        {[], TreeWalker},
        Values
    ),
    {erl_syntax:list(List), TreeWalker1};
interpreted_argval(true, _Context, TreeWalker) ->
    {erl_syntax:atom(true), TreeWalker};
interpreted_argval(Value, Context, TreeWalker) ->
    {{Ast, _VarName, _VarInfo}, TreeWalker1} = resolve_variable_ast(Value, Context, TreeWalker),
    {Ast, TreeWalker1}.

trans_literal_ast(String) ->
	Lit = unescape_string_literal(String),
	erl_syntax:application(
		erl_syntax:atom(zp_trans),
		erl_syntax:atom(trans),
		[
			erl_syntax:tuple([
				erl_syntax:atom(trans),
				erl_syntax:list([
					erl_syntax:tuple([
						erl_syntax:atom('en'),
						erl_syntax:string(Lit)
					])
				])
			]),
			erl_syntax:variable("Language")
		]
	).
