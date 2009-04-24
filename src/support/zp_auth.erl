%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-24
%%
%% @doc Handle authentication of zophrenic users.  Also shows the logon screen when authentication is required.

-module(zp_auth).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    is_auth/1,
    is_auth_recent/1,
    output_logon/1,
    logon_from_session/1,
    
    wm_is_authorized/2,
    wm_is_authorized/5,
    
    event/2
]).

-include_lib("zophrenic.hrl").


%% @doc Check if the visitor has been authenticated. Assumes a completely initalized context.
%% @spec is_auth(#context) -> bool()
is_auth(#context{user_id=undefined}) ->
    false;
is_auth(_) ->
    true.

is_auth_recent(#context{user_id=undefined}) ->
    false;
is_auth_recent(_) ->
    true.


%% @doc Check if the session contains an authenticated user id. Called after zp_context:ensure_session. When found
%% then the user_id of the context is set.
%% @spec logon_from_session(#context) -> #context
logon_from_session(Context) ->
    case zp_context:get_session(auth_user_id, Context) of
        undefined -> Context;
        UserId -> zp_acl:logon(UserId, Context)
    end.


%% @doc Convenience function to be called from the is_authorized/2 callback in webmachine resources.
wm_is_authorized(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = zp_context:ensure_all(Context1), 
    case Context2#context.user_id of
        undefined -> 
            ContextLogon = output_logon(Context2), 
            ?WM_REPLY(?WWW_AUTHENTICATE, ContextLogon);
        _ -> 
            ?WM_REPLY(true, Context2)
    end.    


%% @doc Convenience function to be called from the is_authorized/2 callback in webmachine resources.
%% The ReqId is the name of the id in the query string or other parameters.  It must be numerical.
wm_is_authorized(NeedAuth, What, ArgName, ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = zp_context:ensure_all(Context1), 
    case NeedAuth andalso Context2#context.user_id == undefined of
        true -> 
            ContextLogon = output_logon(Context2), 
            ?WM_REPLY(?WWW_AUTHENTICATE, ContextLogon);

        false -> 
            Id = zp_context:get_q(ArgName, Context2),
            IdN = try list_to_integer(Id) catch _:_ -> 0 end,
            case m_rsc:exists(IdN, Context2) of
                false -> 
                    ?WM_REPLY(true, Context2);
                true ->
                    Allow = case What of
                        visible -> zp_acl:rsc_visible(IdN, Context2);
                        editable -> zp_acl:rsc_editable(IdN, Context2)
                    end,
                    case Allow of
                        false ->
                            ContextLogon = output_logon(Context2), 
                            ?WM_REPLY(?WWW_AUTHENTICATE, ContextLogon);
                        true ->  
                            ?WM_REPLY(true, Context2)
                    end
            end
    end.


%% @doc Render the logon screen to the reqdata of the context.
%% @spec render_logon(Context) -> LogonContext
output_logon(Context) ->
    Html = zp_template:render("admin_logon.tpl", [], Context),
    {Data, ContextOut} = zp_context:output(Html, Context),
    RD1 = zp_context:get_reqdata(ContextOut),
    RD2 = wrq:append_to_resp_body(Data, RD1),
    RD3 = wrq:set_resp_header("Content-Type", "text/html; charset=utf-8", RD2),
    zp_context:set_reqdata(RD3, ContextOut).


%% @doc Handle logon events. When successful then reload the current page
event({submit, logon, _TriggerId, _TargetId}, Context) ->
    zp_context:set_session(auth_user_id, 1, Context),
    zp_context:set_session(auth_timestamp, erlang:universaltime(), Context),
    zp_render:wire({reload, []}, Context).
