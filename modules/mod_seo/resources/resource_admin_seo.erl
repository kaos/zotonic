%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-08-07
%% @doc Page with all SEO settings.

-module(resource_admin_seo).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2,
    event/2
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
    Vars = [
        {page_admin_seo, true}
    ],
	Html = z_template:render("admin_seo.tpl", Vars, Context),
	z_context:output(Html, Context).


event({submit, admin_seo, _TriggerId, _TargetId}, Context) ->
    case z_acl:has_role(admin, Context) of
        true ->
            save_settings(z_context:get_q_all(Context), Context),
            z_render:growl("Saved the SEO settings.", Context);
        false ->
            z_render:growl("Only administrators can change the SEO settings.", Context)
    end.


save_settings([], Context) ->
    Context;
save_settings([{"seo" ++ _ = Key, Value} | T], Context) ->
    Value1 = clean(string:strip(Value, both), []),
    [Key1, Key2] = string:tokens(Key, "-"),
    m_config:set_value(list_to_atom(Key1), list_to_atom(Key2), Value1, Context),
    m_config:set_prop(list_to_atom(Key1), list_to_atom(Key2), no_config_edit, true, Context),
    save_settings(T, Context);
save_settings([_|T], Context) ->
    save_settings(T, Context).
    

clean([], Acc) -> 
    lists:reverse(Acc);
clean([H|T], Acc) when 
    H =:= 10 orelse H =:= 13 orelse H =:= $" orelse H =:= $' orelse 
    H =:= $& orelse H =:= $< orelse H =:= $> ->
        clean(T, [32|Acc]);
clean([H|T], Acc) ->
    clean(T, [H|Acc]).

