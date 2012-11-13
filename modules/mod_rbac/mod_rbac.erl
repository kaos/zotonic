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

-module(mod_rbac).
-author("Andreas Stenius <git@astekk.se>").

-mod_title("RBAC").
-mod_description("Role Based Access Control.").
-mod_prio(500).
-mod_depends([base]).
-mod_provides([acl]).
-mod_schema(1).

-include_lib("zotonic.hrl").
-include("include/rbac.hrl").


%% interface functions
-export([
         manage_schema/2,
         observe_acl_logon/2,
         observe_acl_logoff/2,
         observe_acl_is_allowed/2
        ]).


observe_acl_logon(#acl_logon{ id=UserId }, Context) ->
    Context#context{ 
      user_id=UserId, 
      acl=#rbac_state{} % fix me (pick up cached domains)
     }.

observe_acl_logoff(#acl_logoff{}, Context) ->
    Context#context{ user_id=undefined, acl=undefined }.

observe_acl_is_allowed(#acl_is_allowed{ action=Operation, object=Rsc }, 
                       #context{ 
                          acl=#rbac_state{ domains=AssignedDomains } 
                         } = Context) ->
    Acl = m_rsc:get_acl_props(Rsc, Context),
    RscDomain = m_rbac:domain(Rsc, Context),
    Domain = case proplists:get_value(RscDomain, AssignedDomains) of
                 undefined -> assign_operations(RscDomain, Context);
                 D -> D
             end,
    rbac:check_operation_for(Domain, Operation, Acl);
observe_acl_is_allowed(_, _) ->
    undefined.


manage_schema(install, _Context) ->
    #datamodel{
       categories=
           [
            {rbac_role, meta, [{title, <<"RBAC Role">>}]},
            {rbac_operation, meta, [{title, <<"RBAC Operation">>}]}
           ],
       predicates=
           [
            {rbac_domain,
             [{title, <<"RBAC Domain">>}],
             [{undefined, undefined}]
            },
            {rbac_role_domain, 
             [{title, <<"RBAC Role Domain">>}],
             [{undefined, rbac_role}]
            },
            {rbac_role_member, 
             [{title, <<"RBAC Role Member">>}],
             [{person, rbac_role}]
            },
            {rbac_role_operation, 
             [{title, <<"RBAC Role Operation">>}],
             [{rbac_role, rbac_operation}]
            }
           ],
       resources=
           [

           ]
      }.


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

assign_operations(RscDomain, Context) ->
    %% todo: cache result
    #rbac_domain{
       operations=
           lists:flatmap(
             fun(Id) ->
                     [
                      z_convert:to_atom(
                        m_rsc:p_no_acl(Op, name, Context)
                       ) || Op <- m_rbac:role_operations(Id, Context)
                     ]
             end,
             m_rbac:user_roles(RscDomain, Context)
            )
      }.
