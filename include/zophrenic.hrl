%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Main definitions for zophrenic

-define(ZOPHRENIC_VERSION, "0.1.0").

-include_lib("deps/webmachine/include/wm_reqdata.hrl").

%% @doc The request context, session information and other
-record(context, {
        %% Webmachine request data
        wm_reqdata,
        
        %% The resource responsible for handling this request
        resource_module,
        
        %% The page (comet), session- and visitor processes associated with the current request
        visitor_pid=undefined,  % might be present on multiple browsers & computers
        session_pid=undefined,  % one session per browser
        page_pid=undefined,     % multiple pages per session
        page_id=undefined,

        %% The database to be used (derived from the Host)
        db=dbdefault,
        %% The database connection used for (nested) transactions, see zp_db
        dbc=undefined,

        %% The language selected, used by zp_trans and others
        language=en,
        
        %% The current logged on person, derived from the session and visitor
        acl=undefined,      %% opaque placeholder managed by the zp_acl module
        user_id=undefined,

        %% The state below is the render state, can be cached and/or merged
        
        %% State of the current rendered template/scomp/page
        updates=[],
        actions=[],
        content_scripts=[],
        scripts=[],
        wire=[],
        validators=[],

        %% iolist with the accumulated html, xml or whatever output
        render=[],

        %% dictionary with metadata, initialised by the controller
        dict
    }).
    
-define(WM_REQ(ReqData, Context), zp_context:set_reqdata(ReqData, Context)).
-define(WM_REPLY(Reply, Context), {Reply, Context#context.wm_reqdata, Context#context{wm_reqdata=undefined}}).

-define(DB(Context), Context#context.db).
-define(DBC(Context), Context#context.dbc).

%% ACL fields for an acl check. Fields are initialized for the visible resource.
%% This is used for fetching the acl fields from a resource record.
-record(acl_props, {
    is_published=true,
    group_id=1,
    visible_for=0,
    publication_start={{1900,1,1},{0,0,0}},
    publication_end={{9999,1,1}, {0,0,0}}
}).

%% Record used for parsing multipart body (see zp_parse_multipart)
-record(multipart_form, {name, data, filename, tmpfile, file, files=[], args=[]}).
-record(upload, {filename, tmpfile}).

%% Model value interface for templates
-record(m, {model, value}).

%% Used for specifying resource ids, see src/models/m_rsc.erl
-record(rsc, {id}).

%% Used for specifying resource id lists, as returned by object/subject lookup
-record(rsc_list, {list}).

%% Default page length for search
-define(SEARCH_PAGELEN, 15).

%% Used for search results
-record(search_result, {result=[], page=1, pagelen, total, all, pages, next, prev}).
-record(m_search_result, {search_name, search_props, result, page, pagelen, total, pages, next, prev}).
-record(search_sql, {select, from, where="", order="", group_by="", limit, tables=[], args=[], run_func, extra, assoc=false}).

%% ACL visibility levels
-define(ACL_VIS_USER, 3).
-define(ACL_VIS_GROUP, 2).
-define(ACL_VIS_COMMUNITY, 1).
-define(ACL_VIS_PUBLIC, 0).

%% drag and drop event message
-record(dragdrop, {tag, delegate, id}).

%% @doc Check if an assumption is true
-define(ASSERT(A,E), zp_utils:assert(A,E)).

%% @doc Call the translate function, 2nd parameter is either language or context
-define(TR(T,L), zp_trans:trans(T,L)).

%% The name of the session request parameter
-define(SESSION_PAGE_Q, "zp_pageid").

%% Number of seconds between two comet polls before the page expires
-define(SESSION_PAGE_TIMEOUT, 20).

%% Number of seconds between session expiration checks
-define(SESSION_CHECK_EXPIRE, 10).

%% Default session expiration in seconds.
%% The first keepalive message must be received before SESSION_EXPIRE_1 seconds
%% Subsequent messages must be received before SESSION_EXPIRE_N
-define(SESSION_EXPIRE_1,   40).
-define(SESSION_EXPIRE_N, 3600).

%% Millisecs of no activity before the visitor process is stopped (if there are no attached sessions).
-define(VISITOR_TIMEOUT, 60 * 1000).

%% Some standard periods in seconds
-define(MINUTE,     60).
-define(HOUR,     3600).
-define(DAY,     86400).
-define(WEEK,   604800).
-define(YEAR, 31557600).

%% Our default WWW-Authenticate header
-define(WWW_AUTHENTICATE, "OAuth-rsn").

%% Below is copied from Nitrogen, which is copyright 2008-2009 Rusty Klophaus

%%% LOGGING %%%
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(LOG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(DEBUG(Msg), error_logger:info_msg("DEBUG: ~p:~p  ~p~n", [?MODULE, ?LINE, Msg])).
-define(ERROR(Msg, Args), error_logger:error_msg("~p:~p "++Msg, [?MODULE, ?LINE|Args])).

%%% EMAIL %%%
-record(email, {from, to, subject, body}).
