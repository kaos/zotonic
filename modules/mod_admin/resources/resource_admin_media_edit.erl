%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Admin interface for editing media records.

-module(resource_admin_media_edit).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/2,
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

%% @todo Change this into "visible" and add a view instead of edit template.
is_authorized(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = zp_context:ensure_all(Context1), 
    case Context2#context.user_id == undefined of
        true -> 
            ContextLogon = zp_auth:output_logon(Context2), 
            ?WM_REPLY(?WWW_AUTHENTICATE, ContextLogon);

        false -> 
            Id = zp_context:get_q("id", Context2),
            IdN = try list_to_integer(Id) catch _:_ -> 0 end,
            case m_media:exists(IdN, Context2) of
                false -> 
                    ?WM_REPLY(true, Context2);
                true ->
                    case zp_acl:media_editable(IdN, Context2) of
                        false ->
                            ContextLogon = zp_auth:output_logon(Context2), 
                            ?WM_REPLY(?WWW_AUTHENTICATE, ContextLogon);
                        true ->  
                            ?WM_REPLY(true, Context2)
                    end
            end
    end.


resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = zp_context:ensure_all(Context1),
    Id = zp_context:get_q("id", Context2),
    try
        IdN = list_to_integer(Id),
        Context3 = zp_context:set(id, IdN, Context2),
        ?WM_REPLY(m_media:exists(IdN, Context3), Context3)
    catch
        _:_ -> ?WM_REPLY(false, Context2)
    end.


html(Context) ->
    Id = zp_context:get(id, Context),
    Vars = [
        {id, Id},
        {is_editable, zp_acl:media_editable(Id, Context)},
        {referrers, m_media:get_referrers(Id, Context)}
    ],
    Html = zp_template:render("admin_media_edit.tpl", Vars, Context),
	zp_context:output(Html, Context).


%% @doc Handle the submit of the media edit form
event({submit, mediaform, _FormId, _TargetId}, Context) ->
    Post = zp_context:get_q_all(Context),
    Props = filter_props(Post),
    Title = proplists:get_value("title", Props),
    Id = proplists:get_value("id", Props),
    Props1 = proplists:delete("id", Props),
    m_media:update(zp_convert:to_integer(Id), Props1, Context),
    zp_render:wire({growl, [{text,[["Saved ",zp_html:strip(Title)]]}]}, Context).



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


