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
    Title = proplists:get_value("title", Props),
    Id = proplists:get_value("id", Props),
    Props1 = proplists:delete("id", Props),
    m_rsc:update(zp_convert:to_integer(Id), Props1, Context),
    zp_render:wire({growl, [{text,[["Saved ",zp_html:strip(Title)]]}]}, Context);

event({postback, {reload_media, Opts}, _TriggerId, _TargetId}, Context) ->
    RscId = proplists:get_value(rsc_id, Opts),
    DivId = proplists:get_value(div_id, Opts),
    Html = zp_template:render("_edit_media.tpl", [{id,RscId}], Context),
    {Html1, Context1} = zp_render:render_to_string(Html, Context),
    zp_render:update(DivId, Html1, Context1).



%% @doc Remove some properties that are part of the postback
filter_props(Fs) ->
    Remove = [
        "triggervalue",
        "postback",
        "zp_trigger_id",
        "zp_pageid",
        "trigger_value"
    ],
    lists:foldl(fun(P, Acc) -> proplists:delete(P, Acc) end, Fs, Remove).
    %[ {list_to_existing_atom(K), list_to_binary(V)} || {K,V} <- Props ].


