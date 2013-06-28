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

-export([run/1]).

-ifdef(PROFILE).
-define(profile(Title, Expr),?debugTime(Title, Expr)).
-define(message(Msg),?debugMsg(Msg)).
-else.
-define(profile(Title, Expr),Expr).
-define(message(Msg),nop).
-endif.

-record(engine, {module, 
                 compile_options=[], 
                 render_options=[],
                 vars=[]}).
-record(test_case, {
          title="",
          input, 
          expect_output,
          compile_options=[],
          render_options=[],
          vars=[],
          engines=[
                   #engine{ module=zerlydtl },
                   #engine{ module=ztl, 
                            compile_options=
                                [{compiler_options, 
                                  [debug_compiler, 
                                   verbose, report_errors, report_warnings]}
                                ]
                          }
                  ]
         }).

%% run a specific crafted temporary test case
run({Input, Output}) ->
    run_tests(test_generator([#test_case{
                                title="interactive",
                                input=Input,
                                expect_output=Output
                                }]));
%% run selected subset of test cases outside of eunit (don't trunc my output, d*mn it!)
run(Search) ->
    run_tests(test_generator(test_case(Search))).

test_case([S|_]=Search) when is_list(S); is_atom(S) ->
    lists:flatten([test_case(C) || C <- Search]);
test_case(Search) when is_atom(Search) ->
    test_case(atom_to_list(Search));
test_case(Search) when is_list(Search) ->
    [T || T <- lists:flatten(all_tests()), T#test_case.title == Search].

run_tests({generator, G}) ->    
    run_tests(G());
run_tests([{Title, Test}|Ts]) -> 
    io:format("test case: ~s~n", [Title]),
    try
        Test()
    catch
        Class:Reason ->
            io:format("test case ~p failed: ~p:~p~n", [Title, Class, Reason])
    end,
    run_tests(Ts);
run_tests([]) -> 
    ok.

%% eunit test entry point
run_test_() ->
    test_generator(
      lists:flatten(
        all_tests())).

test_generator([]) -> [];
test_generator([T|Ts]) ->
    {generator, 
     fun () ->
             L = fun(P) -> string:join([P, T#test_case.title], ": ") end,
             M = output_matcher(T#test_case.expect_output),
             [{L(atom_to_list(E#engine.module)),
               fun () ->
                       ?profile(L("test case"), 
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
output_matcher(undefined) ->
    fun (Result) ->
            io:format("rendered: ~p~n", [Result])
    end;
output_matcher(Output) ->
    fun ({ok, O}) ->
            B = iolist_to_binary(O),
            ?assertEqual(Output, B),
            ok;
        (Err) ->
            {render_failure, Err}
    end.

run_test_case(_L,
              #test_case{ 
                 input=I,
                 expect_output=Matcher,
                 compile_options=Co,
                 render_options=Ro,
                 vars=V
                },
              #engine{
                 module=E,
                 compile_options=Eco,
                 render_options=Ero,
                 vars=Ev
                }
             ) ->
    ?message(["-- engine ", atom_to_list(E)]),
    C = z:c(testsandbox),

    {ok, M} = ?profile(
                 _L("compiling"),
                 E:compile(I, ztl_template_test, Co ++ Eco, C)),

    Args = case E of
               zerlydtl -> [V ++ Ev, C]; %% does not have any render options
               ztl -> [V ++ Ev, Ro ++ Ero ++ [{z_context, C}]]
           end,

    %io:format("render ~p with args ~p = ~p~n", [M, Args, apply(M, render, Args)]),
    ?assertEqual(
       ok, Matcher(
             ?profile(
                _L("rendering"), 
                apply(M, render, Args))
            )).

all_tests() ->
    [
     test_suite(
       "Basic tests",
       [
        {"Simple test", <<"foo">>, <<"foo">>},
        {"Auto id", <<"{{ #test }}">>, {re, "\\w{8}-test"}},
        {"Url tag", <<"{% url test %}">>, <<"/test">>},
        {"Loremipsum tag", <<"{% loremipsum words=5 %}">>, <<"Lorem ipsum dolor sit amet.">>},
        {"Print tag", <<"{% print \"foo\" %}">>, {re, "<pre>.*\"foo\".*</pre>"}}
       ]),
     test_suite(
       "Somewhat basic tests",
       [
        {"Pass auto id to tag", <<"{% loremipsum words=2 dummy=#test %}">>, <<"Lorem ipsum.">>}
       ])
    ].

test_case(Title, Input, Output) ->
    #test_case{
      title=Title,
      input=Input,
      expect_output=Output }.

test_suite(Suite, Tests) ->
    [test_case(string:join([Suite, Title], ": "), I, O) || {Title, I, O} <- Tests].
