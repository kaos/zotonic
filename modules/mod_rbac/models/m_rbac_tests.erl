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

-module(m_rbac_tests).
-author("Andreas Stenius <git@astekk.se>").

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").
-include("../support/rbac_tests.hrl").


all_tests_with_context(Ctx) ->
    [
     ?_test(?DOMAIN1_ROLES = m_rbac:domain_roles(?DOMAIN1, Ctx)),
     ?_test(?DOMAIN2_ROLES++?DOMAIN2_ROLES_IMPLICIT = m_rbac:domain_roles(?DOMAIN2, Ctx)),
     [?_test(Domain = m_rbac:domain(Rsc, Ctx)) || {Rsc, Domain} <- ?RSC_DOMAINS]
    ].
