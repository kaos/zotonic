%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2012 Andreas Stenius
%% Date: 2012-11-12
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

-module(m_rbac).
-author("Andreas Stenius <git@astekk.se>").
-behaviour(gen_model).

-include_lib("zotonic.hrl").

-export([
         m_find_value/3,
         m_to_list/2,
         m_value/2,
         
         domain/2,
         domain_roles/2,
         user_roles/1,
         user_roles/2,
         role_operations/2
        ]).

%% todo: implement api for accessing rbac info from templates...
m_find_value(_, _, _) ->
    ok.

m_to_list(_, _) ->
    ok.

m_value(_, _) ->
    ok.


domain(Id, Context) ->
    case m_edge:objects(Id, rbac_domain, Context) of
        [] -> Id; % default to self if no domain specified
        [Domain|_] -> Domain
    end.

domain_roles(Id, Context) ->
    Roles = m_edge:objects(Id, rbac_role_domain, Context),
    lists:flatten(
      [
       Roles | [
                domain_roles(Role, Context)
                || Role <- Roles
               ]
      ]
     ).

user_roles(#context{ user_id=UserId}=Context) ->
    m_edge:objects(UserId, rbac_role_member, Context).

user_roles(Domain, Context) ->
    UserRoles = user_roles(Context),
    [Role || Role <- domain_roles(Domain, Context),
             lists:member(Role, UserRoles)
    ].

role_operations(Role, Context) ->
    m_edge:objects(Role, rbac_role_operation, Context).
