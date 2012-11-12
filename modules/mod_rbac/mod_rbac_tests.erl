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
-include("include/rbac.hrl").
-include("support/rbac_tests.hrl").


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
              {with, Ctx, [
                           fun logon_user/1,
                           fun can_update_rsc/1
                          ]}
              |m_rbac_tests:all_tests_with_context(Ctx)
             ]
     end
    }.

%% Default test state
setup_state(Ctx) ->
    ok = z_depcache:set({category_id_to_name, ?PREDICATE}, "predicate", Ctx),
    ok = z_depcache:set({category_is_a, ?PREDICATE}, [predicate], Ctx),
    ok = z_depcache:set({rsc_name, "rbac_role"}, ?RBAC_ROLE, Ctx),
    ok = z_depcache:set(?RBAC_ROLE, [{category_id, ?PREDICATE}], Ctx),
    ok = z_depcache:set({objects, ?RBAC_ROLE, ?DOMAIN1}, ?DOMAIN1_ROLES, Ctx),
    Ctx.

%% test state helpers
setup_user(UserId, Ctx) ->
    setup_user(UserId, [], Ctx).
setup_user(UserId, Domains, Ctx) ->
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

logon_user(Ctx) ->
    ?assertEqual(
       setup_user(
         ?USR2, 
         Ctx
        ),
       mod_rbac:observe_acl_logon(
         #acl_logon{ id=?USR2 }, 
         Ctx)
      ).

can_update_rsc(Ctx) ->
    false = mod_rbac:observe_acl_is_allowed(
              #acl_is_allowed{ action=update, object=?RSC1 },
              setup_user(?USR2, Ctx)
             ),
    true = mod_rbac:observe_acl_is_allowed(
             #acl_is_allowed{ action=update, object=?RSC1 },
             setup_user(?USR2, [{?DOMAIN1, [update]}], Ctx)
            ).

