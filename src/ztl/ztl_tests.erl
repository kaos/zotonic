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
-module(ztl_tests).
-author('Andreas Stenius <kaos@astekk.se>').

-include_lib("eunit/include/eunit.hrl").

-record(engine, {module, compile_options=[], render_options=[]}).
-record(test_case, {
          title="",
          input, 
          expect_output,
          compile_options=[],
          render_options=[],
          engines=[
                   #engine{ module=zerlydtl },
                   #engine{ module=ztl, 
                            compile_options=[{compiler_options, 
                                              [
                                               debug_compiler, 
                                               verbose, report_errors, report_warnings]}, 
                                             {extension_module, ztl_extensions}]
                          }
                  ]
         }).

run_test_() ->
    test_generator(all_test_cases()).

test_generator([]) -> [];
test_generator([T|Ts]) ->
    {generator, 
     fun () ->
             L = fun(P) -> string:join([P, T#test_case.title], ": ") end,
             M = output_matcher(T#test_case.expect_output),
             [{T#test_case.title,
               fun () ->
                       ?debugTime(L("test case"), 
                                  run_test_case(L, T#test_case{ expect_output=M }, E))
               end} || E <- T#test_case.engines]
                 ++ test_generator(Ts)
     end
    }.

output_matcher({re, Re}) ->
    {ok, R} = re:compile(Re),
    fun ({ok, O}) ->
            B = iolist_to_binary(O),
            case re:run(B, R, [{capture, none}]) of
                match -> ok;
                nomatch ->
                    {re_mismatch, Re, B}
            end;
        (Err) ->
            {render_failure, Err}
    end;
output_matcher(Output) ->
    fun ({ok, O}) ->
            B = iolist_to_binary(O),
            ?assertEqual(Output, B),
            ok;
        (Err) ->
            {render_failure, Err}
    end.

run_test_case(L,
              #test_case{ 
                 input=I,
                 expect_output=Matcher,
                 compile_options=Co,
                 render_options=Ro
                },
              #engine{
                 module=E,
                 compile_options=Eco,
                 render_options=Ero
                }
             ) ->
    ?debugMsg(["-- engine ", atom_to_list(E)]),
    C = z:c(testsandbox),

    {ok, M} = ?debugTime(
                 L("compiling"),
                 E:compile(I, ztl_template_test, Co ++ Eco, C)),

    Args = case E of
               zerlydtl -> [Ro ++ Ero, C];
               ztl -> [[], Ro ++ Ero]
           end,

    ?assertEqual(
       ok, Matcher(
             ?debugTime(
                L("rendering"), 
                apply(M, render, Args))
            )).

all_test_cases() ->
    [
     #test_case{
        title= "simple test",
        input= <<"foo">>,
        expect_output= <<"foo">>
       },
     #test_case{
        title="auto id",
        input= <<"{{ #test }}">>,
        expect_output= {re, "\\w{8}-test"}
       }
    ].
