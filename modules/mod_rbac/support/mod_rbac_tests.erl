%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2012 Andreas Stenius
%% Date: 2012-11-09
%% @doc Role Based Access Control module

%% Copyright 2012 Andreas Stenius
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_rbac_tests).
-author("Andreas Stenius <git@astekk.se>").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").
-include("../include/rbac.hrl").
-include("rbac_tests.hrl").

-export([setup/0, setup_acl/2, setup_acl/3]).

%% setup test context for manual debugging of tests
%% Run debugger from eshell with: im().
%% load source modules of interest, adding break points as needed.
%% get a context to use when testing: Context = mod_rbac_tests:setup().
%% call function to debug using this context, e.g: m_rbac:user_roles(2, Context).
setup() ->
    {setup, Setup, _, _} = mod_rbac_test_(),
    Setup().


%% run tests for module rbac too...
rbac_test_() ->
     rbac.

%% mod RBAC test fixture
mod_rbac_test_() ->
    {setup,
     fun() ->
             Ctx = case z_depcache:start_link([{host, rbac_test}]) of
                       {ok, Pid} ->              
                           #context{ depcache=Pid };
                       {error, {already_started, Pid}} ->
                           C = #context{ depcache=Pid },
                           z_depcache:flush(C),
                           C
                   end,
             setup_state(Ctx)
     end,
     fun(#context{ depcache=Depcache }) ->
             exit(Depcache, normal)
     end,
     fun(Ctx) ->
             [
              all_tests_with_context(Ctx),
              m_rbac_tests:all_tests_with_context(Ctx)
             ]
     end
    }.

%% Default test state
setup_state(Ctx) ->
    %% fill depcache with data to avoid hitting the db
    [ok = z_depcache:set(Key, Value, Ctx)
     || {Key, Value} <- 
            lists:flatten(
              [
               {{category_is_a, ?PREDICATE}, [predicate]},
               [[{{category_id_to_name, Id}, Name},
                {{category_id_to_name, z_convert:to_atom(Name)}, 
                 z_convert:to_atom(Name)}] || {Id, Name} <- ?RSC_NAMES],
               [{{rsc_name, Name}, Id} || {Id, Name} <- ?RSC_NAMES],
               [{Id, [{name, Name}]} || {Id, Name} <- ?RSC_NAMES],
               [{Id, [{category_id, ?PREDICATE}]} || Id <- ?PREDICATES],
               [{{subjects, ?RBAC_DOMAIN_RSC, Rsc}, [Domain]} 
                || {Domain, Rscs} <- ?DOMAIN_RSCS, Rsc <- Rscs],
               [{{objects, ?RBAC_DOMAIN_RSC, Domain}, Rscs} || {Domain, Rscs} <- ?DOMAIN_RSCS],
               [{{objects, ?RBAC_DOMAIN_ROLE, Domain}, Roles} || {Domain, Roles} <- ?DOMAIN_ROLES],
               [{{objects, ?RBAC_DOMAIN_ROLE, Role}, Roles} || {Role, Roles} <- ?ROLE_HIERARCHY],
               [{{objects, ?RBAC_ROLE_OPERATION, Role}, Ops} || {Role, Ops} <- ?ROLE_OPS],
               [{{objects, ?RBAC_ROLE_MEMBER, User}, Roles} || {User, Roles} <- ?USER_ROLES]
              ])
    ],
    Ctx.

%% test state helpers
setup_acl(UserId, Ctx) ->
    setup_acl(UserId, [], Ctx).

-spec setup_acl(integer(), [{integer(), list()}], #context{}) -> #context{}.
setup_acl(UserId, Domains, Ctx) ->
    Ctx#context{ 
      user_id=UserId, 
      acl=#rbac_state{ 
             domains=
                 [
                  {Id, #rbac_domain{ operations=Ops }}
                  || {Id, Ops} <- Domains
                 ]
            }
     }.


%%------------------------------------------------------------
%% tests
%%------------------------------------------------------------

all_tests_with_context(Ctx) ->
    [
     ?_test(
        ?assertEqual(
           setup_acl(
             ?USR2, 
             Ctx
            ),
           mod_rbac:observe_acl_logon(
             #acl_logon{ id=?USR2 }, 
             Ctx)
          )
       ),

     ?_test(
        %% Domain info already in Acl context
        ?assert(mod_rbac:observe_acl_is_allowed(
                  #acl_is_allowed{ action=update, object=?RSC1 },
                  setup_acl(?USR2, [{?DOMAIN1, [update]}], Ctx)
                 )
               )
       ),

     ?_test(
        %% No domain info in Acl context
        ?assert(mod_rbac:observe_acl_is_allowed(
                  #acl_is_allowed{ action=update, object=?RSC1 },
                  setup_acl(?USR2, Ctx)
                 )
               )
       )
    ].

