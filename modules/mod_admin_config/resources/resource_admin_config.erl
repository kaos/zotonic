%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-08-07
%% @doc Overview of all config settings with string values.

-module(resource_admin_config).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1), 
    case z_acl:has_role(admin, Context2) of
        false -> z_auth:wm_output_logon(Context2);
        true ->  ?WM_REPLY(true, Context2)
    end.


html(Context) ->
    All = m_config:all(Context),
    AllWithValue = lists:sort(lists:map(fun only_value_config/1, All)),
    Vars = [
        {page_admin_config, true},
        {config, AllWithValue}
    ],
	Html = z_template:render("admin_config.tpl", Vars, Context),
	z_context:output(Html, Context).


%% @doc Check if the config does not have a non-string setting.  We only edit string values.
%% @spec is_value_config({module, [{key,}]}) -> bool()
only_value_config({Module, Keys}) ->
    {Module, lists:filter(fun is_value_config_key/1, Keys)}.

    is_value_config_key({_Key, Props}) ->
        is_value_config_props(Props).

    is_value_config_props([]) ->
        true;
    is_value_config_props([{Prop,_}|Rest]) when Prop == created; Prop == modified; Prop == value; Prop == id; Prop == module; Prop == key ->
        is_value_config_props(Rest);
    is_value_config_props([{props,<<>>}|Rest]) ->
        is_value_config_props(Rest);
    is_value_config_props([{props,undefined}|Rest]) ->
        is_value_config_props(Rest);
    is_value_config_props(_X) ->
        false.

