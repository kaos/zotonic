%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Overview of modules, allows activating/deactivating the modules.

-module(resource_admin_module_manager).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    z_auth:wm_is_authorized(ReqData, Context).


html(Context) ->
    Vars = [
        {page_admin_modules, true},
        {modules, mod_admin_modules:all(Context)}
    ],
	Html = z_template:render("admin_modules.tpl", Vars, Context),
	z_context:output(Html, Context).
