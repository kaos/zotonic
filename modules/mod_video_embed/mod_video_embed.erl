%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-15
%%
%% @doc Enables embedding video's as media pages.  Handles the embed information for showing video's.
%% The embed information is stored in the medium table associated with the page. You can not have embed
%% information and a medium file. Either one or the other.

-module(mod_video_embed).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Video embed").
-mod_description("Embed youtube, vimeo and other movies as media pages.").
-mod_prio(600).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    rsc_update/3,
    media_viewer/2,
    event/2
]).

-include_lib("zophrenic.hrl").

-record(state, {context}).

%% @doc Fantasy mime type to distinguish embeddable html fragments.
-define(EMBED_MIME, <<"text/html-video-embed">>).

%% @doc Check if the update contains video embed information.  If so then update the attached medium item.
%% @spec rsc_update({rsc_update, ResourceId, OldResourceProps}, UpdateProps, Context) -> NewUpdateProps
rsc_update({rsc_update, Id, _OldProps}, Props, Context) ->
    case proplists:is_defined(video_embed_code, Props) of
        true -> 
            case proplists:get_value(video_embed_code, Props) of
                Empty when Empty == undefined; Empty == <<>>; Empty == [] ->
                    % Delete the media record iff the media mime type is our mime type
                    case m_media:identify(Id, Context) of
                        {ok, Props} ->
                            case proplists:get_value(mime, Props) of
                                ?EMBED_MIME -> m_media:delete(Id, Context);
                                _ -> nop
                            end;
                        _ ->
                            nop
                    end;
                EmbedCode ->
                    EmbedCodeRaw = zp_html:unescape(EmbedCode),
                    EmbedService = proplists:get_value(video_embed_service, Props, ""),
                    MediaProps = [
                        {mime, ?EMBED_MIME},
                        {video_embed_code, EmbedCodeRaw},
                        {video_embed_service, EmbedService}
                    ],
                    ok = m_media:replace(Id, MediaProps, Context)
            end,

            proplists:delete(video_embed_code, 
                proplists:delete(video_embed_service, Props));
        false ->
            Props
    end.


%% @doc Return the media viewer for the embedded video (that is, when it is an embedded media).
%% @spec media_viewer(Notification, Context) -> undefined | {ok, Html}
media_viewer({media_viewer, Props, _Filename, _Options}, _Context) ->
    case proplists:get_value(mime, Props) of
        ?EMBED_MIME ->
            case proplists:get_value(video_embed_code, Props) of
                undefined -> undefined;
                EmbedCode -> {ok, EmbedCode}
            end;
        _ ->
            undefined
    end.


%% @doc Handle the form submit from the "new media" dialog.  The form is defined in templates/_media_upload_panel.tpl.
%% @spec event(Event, Context1) -> Context2
event({submit, {add_video_embed, EventProps}, _TriggerId, _TargetId}, Context) ->
    Actions = proplists:get_value(actions, EventProps, []),
    Id = proplists:get_value(id, EventProps),
    EmbedService = zp_context:get_q("video_embed_service", Context),
    EmbedCode = zp_context:get_q_validated("video_embed_code", Context),

    case Id of
        %% Create a new page
        undefined ->
            RscId = proplists:get_value(rsc_id, EventProps),
            Predicate = proplists:get_value(predicate, EventProps, depiction),
            Title   = zp_context:get_q_validated("title", Context),
            GroupId = list_to_integer(zp_context:get_q("group_id", Context)),

            Props = [
                {title, Title},
                {category, video},
                {group_id, GroupId},
                {video_embed_service, EmbedService},
                {video_embed_code, EmbedCode}
            ],

            F = fun(Ctx) ->
                case m_rsc:insert(Props, Context) of
                    {ok, MediaRscId} ->
                        case RscId of
                            undefined -> nop;
                            _ -> m_edge:insert(RscId, Predicate, MediaRscId, Ctx)
                        end,
                        {ok, MediaRscId};
                    {error, Error} ->
                        throw({error, Error})
                end 
            end,
    
            case zp_db:transaction(F, Context) of
                {ok, MediaId} ->
                    ContextRedirect = case RscId of
                        undefined -> zp_render:wire({redirect, [{dispatch, "admin_edit_rsc"}, {id, MediaId}]}, Context);
                        _ -> Context
                    end,
                    zp_render:wire([{growl, [{text, "Made the media page."}]} | Actions], ContextRedirect);
                {rollback, {_Error, _Trace}} ->
                    ?ERROR("~p~n~p", [_Error, _Trace]),
                    zp_render:growl_error("Could not create the media page.", Context)
            end;
        
        %% Update the current page
        N when is_integer(N) ->
            Props = [
                {category, video},
                {video_embed_service, EmbedService},
                {video_embed_code, EmbedCode}
            ],
            case m_rsc:update(Id, Props, Context) of
                {ok, _} ->
                    zp_render:wire([{dialog_close, []} | Actions], Context);
                {error, _} ->
                    zp_render:growl_error("Could not update the page with the new embed code.", Context)
            end
    end.
    


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() -> 
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    zp_notifier:observe(rsc_update, {?MODULE, rsc_update}, Context),
    zp_notifier:observe(media_viewer, {?MODULE, media_viewer}, Context),
    {ok, #state{context=zp_context:new_for_host(Context)}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    zp_notifier:detach(rsc_update, {?MODULE, rsc_update}, State#state.context),
    zp_notifier:detach(media_viewer, {?MODULE, media_viewer}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

