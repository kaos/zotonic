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
    z_auth:wm_is_authorized(true, visible, "id", ReqData, Context).


resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = z_context:ensure_all(Context1),
    Id = z_context:get_q("id", Context2),
    try
        IdN = list_to_integer(Id),
        Context3 = z_context:set(id, IdN, Context2),
        ?WM_REPLY(m_rsc:exists(IdN, Context3), Context3)
    catch
        _:_ -> ?WM_REPLY(false, Context2)
    end.


html(Context) ->
    Vars = [
        {id, z_context:get(id, Context)}
    ],
    Html = z_template:render("admin_edit.tpl", Vars, Context),
	z_context:output(Html, Context).


%% @doc Handle the submit of the resource edit form
event({submit, rscform, _FormId, _TargetId}, Context) ->
    Post = z_context:get_q_all(Context),
    Props = filter_props(Post),
    Title = ?TR(proplists:get_value("title", Props), Context),
    Id = z_convert:to_integer(proplists:get_value("id", Props)),
    Props1 = proplists:delete("id", Props),
    CatBefore = m_rsc:p(Id, category_id, Context),
    case m_rsc:update(Id, Props1, Context) of
        {ok, _} -> 
            case proplists:is_defined("save_view", Post) of
                true ->
                    PageUrl = m_rsc:p(Id, page_url, Context),
                    z_render:wire({redirect, [{location, PageUrl}]}, Context);
                false ->
                    case m_rsc:p(Id, category_id, Context) of
                        CatBefore ->
                            Context1 = z_render:set_value("field-name", m_rsc:p(Id, name, Context), Context),
                            Context2 = z_render:set_value("field-uri",  m_rsc:p(Id, uri, Context1), Context1),
                            Context3 = z_render:set_value("slug",  m_rsc:p(Id, slug, Context2), Context2),
                            Context4 = case z_convert:to_bool(m_rsc:p(Id, is_protected, Context3)) of
                                true ->  z_render:wire("delete-button", {disable, []}, Context3);
                                false -> z_render:wire("delete-button", {enable, []}, Context3)
                            end,
                            Context5 = z_render:growl(["Saved ",z_html:strip(Title),"."], Context4),
                            case proplists:is_defined("save_duplicate", Post) of
                                true ->
                                    z_render:wire({dialog_duplicate_rsc, [{id, Id}]}, Context5);
                                false ->
                                    Context5
                            end;
                        _CatOther ->
                            z_render:wire({reload, []}, Context)
                    end
            end;
        {error, duplicate_uri} ->
            z_render:growl_error("Error, duplicate uri. Please change the uri.", Context);
        {error, duplicate_name} ->
            z_render:growl_error("Error, duplicate name. Please change the name.", Context);
        {error, eacces} ->
            z_render:growl_error("You don't have permission to edit this page.", Context);
        {error, _Reason} ->
            z_render:growl_error("Something went wrong. Sorry.", Context)
    end;

event({postback, {reload_media, Opts}, _TriggerId, _TargetId}, Context) ->
    RscId = proplists:get_value(rsc_id, Opts),
    DivId = proplists:get_value(div_id, Opts),
    {Html, Context1} = z_template:render_to_iolist("_edit_media.tpl", [{id,RscId}], Context),
    z_render:update(DivId, Html, Context1).



%% @doc Remove some properties that are part of the postback
filter_props(Fs) ->
    Remove = [
        "triggervalue",
        "postback",
        "z_trigger_id",
        "z_pageid",
        "trigger_value",
        "save_view",
        "save_duplicate",
        "save_stay"
    ],
    lists:foldl(fun(P, Acc) -> proplists:delete(P, Acc) end, Fs, Remove).
    %[ {list_to_existing_atom(K), list_to_binary(V)} || {K,V} <- Props ].


