%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Overview of modules, allows activating/deactivating the modules.

-module(resource_admin_module_admin).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(ReqData, Context).


html(Context) ->
    Vars = [
        {modules, mod_module_admin:all(Context)}
    ],
	Html = zp_template:render("admin_module_admin.tpl", Vars, Context),
	zp_context:output(Html, Context).
