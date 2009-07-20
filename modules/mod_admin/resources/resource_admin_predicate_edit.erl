%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Edit/create a predicate.

-module(resource_admin_predicate_edit).
-author("Marc Worrell <marc@worrell.nl>").

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
        case m_predicate:get(IdN, Context) of
            undefined -> ?WM_REPLY(false, Context3);
            _ -> ?WM_REPLY(true, Context3)
        end
    catch
        _:_ -> ?WM_REPLY(false, Context2)
    end.


html(Context) ->
    Id = z_context:get(id, Context),
    Vars = [
        {id, Id}
    ],
    Html = z_template:render("admin_predicate_edit.tpl", Vars, Context),
	z_context:output(Html, Context).


%% @doc Handle the submit of the resource edit form
event({submit, predform, _FormId, _TargetId}, Context) ->
    Post = z_context:get_q_all(Context),
    Props = filter_props(Post),
    Title = proplists:get_value("title", Props),
    Id = proplists:get_value("id", Props),
    Props1 = proplists:delete("id", Props),
    m_predicate:update(z_convert:to_integer(Id), Props1, Context),
    z_render:growl(["Saved ",z_html:strip(?TR(Title, Context))], Context).


%% @doc Remove some properties that are part of the postback
filter_props(Fs) ->
    Remove = [
        "triggervalue",
        "postback",
        "z_trigger_id",
        "z_pageid",
        "trigger_value"
    ],
    lists:foldl(fun(P, Acc) -> proplists:delete(P, Acc) end, Fs, Remove).
    %[ {list_to_existing_atom(K), list_to_binary(V)} || {K,V} <- Props ].


