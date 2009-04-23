%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_resource.

-module(resource_admin_edit).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    resource_exists/2, 
    event/2
]).

-include_lib("resource_html.hrl").

resource_exists(_ReqProps, Context) ->
    Context1 = zp_context:ensure_all(Context),
    Id = zp_context:get_q("id", Context1),
    try
        IdN = list_to_integer(Id),
        {m_rsc:exists(IdN, Context1), zp_context:set(id, IdN, Context1)}
    catch
        _:_ ->
            {false, Context1}
    end.


html(_ReqProps, Context) ->
    Vars = [
        {id, zp_context:get(id, Context)}
    ],
    F = fun(Ctx) ->
        zp_template:render("admin_edit.tpl", Vars, Ctx)
    end,
    Html = zp_acl:sudo(F, Context),
	zp_context:output(Html, Context).


%% @doc Handle the submit of the resource edit form
event({submit, rscform, _FormId, _TargetId}, Context) ->
    Post = zp_context:get_q_all(Context),
    Props = filter_props(Post),
    Title = proplists:get_value(title, Props),
    Id = proplists:get_value(id, Props),
    Props1 = proplists:delete(id, Props),
    m_rsc:update(zp_convert:to_integer(Id), Props1, Context),
    zp_render:wire({growl, [{text,[["Saved ",zp_html:strip(Title)]]}]}, Context).


%% @doc Remove some properties that are part of the postback
filter_props(Fs) ->
    valid_fields(),
    Remove = [
        "triggervalue",
        "postback",
        "zp_trigger_id",
        "zp_pageid",
        "trigger_value"
    ],
    Props = lists:foldl(fun(P, Acc) -> proplists:delete(P, Acc) end, Fs, Remove),
    %[ {list_to_existing_atom(K), list_to_binary(V)} || {K,V} <- Props ].
    [ {list_to_atom(K), list_to_binary(V)} || {K,V} <- Props ].


valid_fields() ->
    [
        title,
        intro,
        body,
        seo_noindex,
        seo_title,
        seo_keywords,
        seo_desc
    ].
