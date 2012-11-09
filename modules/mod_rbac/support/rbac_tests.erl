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

-module(rbac_tests).
-author("Andreas Stenius <git@astekk.se>").

-include_lib("eunit/include/eunit.hrl").
-include_lib("zotonic.hrl").
-include("../include/rbac.hrl").


visible_world_test() ->
    Acl = #acl_props{}, % the defaults are fine
    NoSession = undefined, % i.e. anonymous requests
    UserSession = #rbac_session{},
    ?assert(rbac:check_operation_for(NoSession, view, Acl)),
    ?assertNot(rbac:check_operation_for(NoSession, update, Acl)),
    %% should work equally well for any logged in user
    ?assert(rbac:check_operation_for(UserSession, view, Acl)),
    ?assertNot(rbac:check_operation_for(UserSession, update, Acl)).    

visible_community_test() ->
    Acl = #acl_props{ visible_for=?ACL_VIS_COMMUNITY },
    UserSession = #rbac_session{},
    ?assertNot(rbac:check_operation_for(undefined, view, Acl)),
    ?assert(rbac:check_operation_for(UserSession, view, Acl)).

visible_group_test() ->
    Acl = #acl_props{ visible_for=?ACL_VIS_GROUP },
    NonMember = #rbac_session{},
    Member = #rbac_session{ operations=[view] },
    ?assertNot(rbac:check_operation_for(undefined, view, Acl)),
    ?assertNot(rbac:check_operation_for(NonMember, view, Acl)),
    ?assert(rbac:check_operation_for(Member, view, Acl)).

visible_user_test() ->
    Acl = #acl_props{ visible_for=?ACL_VIS_USER },
    NotOwner = #rbac_session{},
    Member = #rbac_session{ operations=[view] },
    Owner = #rbac_session{ is_owner=true },
    ?assertNot(rbac:check_operation_for(undefined, view, Acl)),
    ?assertNot(rbac:check_operation_for(NotOwner, view, Acl)),
    ?assertNot(rbac:check_operation_for(Member, view, Acl)),
    ?assert(rbac:check_operation_for(Owner, view, Acl)).
