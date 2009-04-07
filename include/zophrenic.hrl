%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Main definitions for zophrenic


%% @doc The request context, session information and other
-record(context, {
            %% Request properties
            reqprops,
            
            %% The resource responsible for handling this request
            resource_module,
            
            %% The page (comet), session- and user processes associated with the current request
            person_pid=undefined,
            session_pid=undefined,
            page_pid=undefined,
            page_id=undefined,

            %% The database to be used (derived from the Host)
            db=dbdefault,
            %% The connection for nested transactions
            dbc=undefined,
            
            %% The state below is the real render state, able to be cached and merged
            
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

%% drag and drop event message
-record(dragdrop, {tag, delegate, id}).

%% Used for specifying resource ids, see src/models/m_rsc.erl
-record(rsc, {id}).

%% @doc Check if an assumption is true
-define(ASSERT(A,E), zp_utils:assert(A,E)).

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

%% Millisecs of no activity before the person process is stopped (if there are no attached sessions).
-define(PERSON_TIMEOUT, 60 * 1000).

%% Some standard periods in seconds
-define(MINUTE,     60).
-define(HOUR,     3600).
-define(DAY,     86400).
-define(WEEK,   604800).
-define(YEAR, 31557600).


%% Below is copied from Nitrogen, which is copyright 2008-2009 Rusty Klophaus

%%% LOGGING %%%
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(LOG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(DEBUG(Msg), error_logger:info_msg("DEBUG: ~p:~p  ~p~n", [?MODULE, ?LINE, Msg])).

%%% EMAIL %%%
-record(email, {from, to, subject, body}).
