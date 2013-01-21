%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2013 Andreas Stenius
%% Date: 2013-01-20
%% @doc Show a site status dashboard.

%% Copyright 2013 Andreas Stenius
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

-module(mod_admin_status).
-author("Andreas Stenius <git@astekk.se>").

-mod_title("Admin status dashboard").
-mod_description("Give admins a overview of the system status.").
-mod_prio(800).
-mod_depends([admin]).
-mod_provides([status]).


%% interface functions
-export([
    observe_admin_menu/3
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").


observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_config,
                parent=admin_system,
                label=?__("Status Dashboard", Context),
                url={admin_status_dashboard},
                visiblecheck={acl, use, mod_admin_status}}
     
     |Acc].
