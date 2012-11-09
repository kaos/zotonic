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

-module(rbac).
-author("Andreas Stenius <git@astekk.se>").

-include_lib("zotonic.hrl").
-include("../include/rbac.hrl").

%% interface functions
-export([
         check_operation_for/3
]).


%% @doc return true if user is allowed to perform operation
check_operation_for(Session, view, Acl) ->
    check_view_for(Session, Acl);
check_operation_for(_Session, _Operation, _Acl) ->
    false.


%% ------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------

check_view_for(undefined, Acl) ->
    check_view_level(?ACL_VIS_PUBLIC, Acl);
check_view_for(Session, Acl) ->
    check_view_level(?ACL_VIS_COMMUNITY, Acl) orelse 
        check_group_view_for(Session, Acl) orelse
        check_owner(Session).

check_view_level(Level, #acl_props{ is_published=true, 
                                    visible_for=Visible,
                                    publication_start=Start,
                                    publication_end=End
                                  }) when Visible =< Level ->
    Date = calendar:local_time(),
    Start =< Date andalso Date =< End;
check_view_level(_, _) ->
    false.

check_group_view_for(Session, Acl) ->
    check_view_level(?ACL_VIS_GROUP, Acl) andalso
        check_assigned(view, Session).

check_assigned(Operation, #rbac_session{ operations=Assigned }) ->
    lists:member(Operation, Assigned).

check_owner(#rbac_session{ is_owner=Owner }) ->
    Owner.
