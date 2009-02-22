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

-record(dragdrop, {tag, delegate, id}).

%% @doc Check if an assumption is true
-define(ASSERT(A,E), zp_utils:assert(A,E)).

%% The name of the session request parameter
-define(SESSION_PAGE_Q, "zp_pageid").

%% Number of seconds between two comet polls before the page expires
-define(SESSION_PAGE_TIMEOUT, 20).

%% Below is copied from Nitrogen, which is copyright 2008-2009 Rusty Klophaus

%%% LOGGING %%%
-define(PRINT(Var), error_logger:info_msg("DEBUG: ~p:~p - ~p: ~p~n", [?MODULE, ?LINE, ??Var, Var])).
-define(LOG(Msg, Args), error_logger:info_msg(Msg, Args)).
-define(DEBUG, error_logger:info_msg("DEBUG: ~p:~p~n", [?MODULE, ?LINE])).

%%% EMAIL %%%
-record(email, {from, to, subject, body}).
