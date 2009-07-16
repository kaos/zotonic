%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Admin webmachine_resource.

-module(resource_admin_edit).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    resource_exists/2,
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

%% @todo Change this into "visible" and add a view instead of edit template.
is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(true, visible, "id", ReqData, Context).


resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = zp_context:ensure_all(Context1),
    Id = zp_context:get_q("id", Context2),
    try
        IdN = list_to_integer(Id),
        Context3 = zp_context:set(id, IdN, Context2),
        ?WM_REPLY(m_rsc:exists(IdN, Context3), Context3)
    catch
        _:_ -> ?WM_REPLY(false, Context2)
    end.


html(Context) ->
    Vars = [
        {id, zp_context:get(id, Context)}
    ],
    Html = zp_template:render("admin_edit.tpl", Vars, Context),
	zp_context:output(Html, Context).


%% @doc Handle the submit of the resource edit form
event({submit, rscform, _FormId, _TargetId}, Context) ->
    Post = zp_context:get_q_all(Context),
    Props = filter_props(Post),
    Title = ?TR(proplists:get_value("title", Props), Context),
    Id = zp_convert:to_integer(proplists:get_value("id", Props)),
    Props1 = proplists:delete("id", Props),
    case m_rsc:update(Id, Props1, Context) of
        {ok, _} -> 
            Context1 = zp_render:set_value("field-name", m_rsc:p(Id, name, Context), Context),
            Context2 = zp_render:set_value("field-uri",  m_rsc:p(Id, uri, Context1), Context1),
            Context3 = zp_render:set_value("slug",  m_rsc:p(Id, slug, Context2), Context2),
            Context4 = case zp_convert:to_bool(m_rsc:p(Id, is_protected, Context3)) of
                true ->  zp_render:wire("delete-button", {disable, []}, Context3);
                false -> zp_render:wire("delete-button", {enable, []}, Context3)
            end,
            case proplists:is_defined("save_view", Post) of
                true ->
                    PageUrl = m_rsc:p(Id, page_url, Context),
                    zp_render:wire({redirect, [{location, PageUrl}]}, Context);
                false ->
                    zp_render:growl(["Saved ",zp_html:strip(Title),"."], Context4)
            end;
        {error, duplicate_uri} ->
            zp_render:growl_error("Error, duplicate uri. Please change the uri.", Context);
        {error, duplicate_name} ->
            zp_render:growl_error("Error, duplicate name. Please change the name.", Context);
        {error, eacces} ->
            zp_render:growl_error("You don't have permission to edit this page.", Context);
        {error, _Reason} ->
            zp_render:growl_error("Something went wrong. Sorry.", Context)
    end;

event({postback, {reload_media, Opts}, _TriggerId, _TargetId}, Context) ->
    RscId = proplists:get_value(rsc_id, Opts),
    DivId = proplists:get_value(div_id, Opts),
    {Html, Context1} = zp_template:render_to_iolist("_edit_media.tpl", [{id,RscId}], Context),
    zp_render:update(DivId, Html, Context1).



%% @doc Remove some properties that are part of the postback
filter_props(Fs) ->
    Remove = [
        "triggervalue",
        "postback",
        "zp_trigger_id",
        "zp_pageid",
        "trigger_value",
        "save_view",
        "save_stay"
    ],
    lists:foldl(fun(P, Acc) -> proplists:delete(P, Acc) end, Fs, Remove).
    %[ {list_to_existing_atom(K), list_to_binary(V)} || {K,V} <- Props ].


