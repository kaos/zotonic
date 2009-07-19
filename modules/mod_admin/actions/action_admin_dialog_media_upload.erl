%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-27
%%
%% @doc Open a dialog with some fields to upload a new media.

-module(action_admin_dialog_media_upload).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Title = proplists:get_value(title, Args),
    Id = proplists:get_value(id, Args),
    SubjectId = proplists:get_value(subject_id, Args),
    GroupId = proplists:get_value(group_id, Args),
    Predicate = proplists:get_value(predicate, Args, depiction),
    Actions = proplists:get_all_values(action, Args),
    Stay = proplists:get_value(stay, Args, false),
    Postback = {media_upload_dialog, Title, Id, SubjectId, GroupId, Predicate, Stay, Actions},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {media_upload_dialog, Title, Id, SubjectId, GroupId, Predicate, Stay, Actions}, _TriggerId, _TargetId}, Context) ->
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {id, Id },
        {subject_id, SubjectId },
        {group_id, GroupId},
        {title, Title},
        {actions, Actions},
        {predicate, Predicate},
        {stay, Stay}
    ],
    DTitle = case Id of undefined -> "Add a new media file"; _ -> "Replace current medium." end,
    zp_render:dialog(DTitle, "_action_dialog_media_upload.tpl", Vars, Context);


event({submit, {media_upload, EventProps}, _TriggerId, _TargetId}, Context) ->
    Actions = proplists:get_value(actions, EventProps, []),
    Id = proplists:get_value(id, EventProps),
    Stay = zp_convert:to_bool(proplists:get_value(stay, EventProps, false)),
    File = zp_context:get_q_validated("upload_file", Context),
    ContextUpload = case File of
        #upload{filename=OriginalFilename, tmpfile=TmpFile} ->
            case Id of
                %% Create a new media page
                undefined ->
                    SubjectId = proplists:get_value(subject_id, EventProps),
                    Predicate = proplists:get_value(predicate, EventProps, depiction),
                    Title   = zp_context:get_q_validated("new_media_title", Context),
                    GroupId = list_to_integer(zp_context:get_q("group_id", Context)),

                    Props = [{title, Title}, {original_filename, OriginalFilename}, {group_id, GroupId}],
                    F = fun(Ctx) ->
                        case m_media:insert_file(TmpFile, Props, Ctx) of
                            {ok, MediaRscId} ->
                                case SubjectId of
                                    undefined -> 
                                        ok;
                                    _ ->
                                        m_edge:insert(SubjectId, Predicate, MediaRscId, Ctx)
                                end,
                                {ok, MediaRscId};
                            Error -> 
                                Error
                        end
                    end,
                    Result = zp_db:transaction(F, Context),
            
                    case Result of
                        {ok, MediaId} ->
                            ContextRedirect = case SubjectId of
                                undefined -> 
                                    case Stay of
                                        true -> Context;
                                        false -> zp_render:wire({redirect, [{dispatch, "admin_edit_rsc"}, {id, MediaId}]}, Context)
                                    end;
                                _ -> Context
                            end,
                            zp_render:wire([{growl, [{text, "Uploaded the file."}]} | Actions], ContextRedirect);
                        {error, _Error} ->
                            zp_render:growl_error("Error uploading the file.", Context)
                    end;
                
                %% Replace attached medium with the uploaded file (skip any edge requests)
                N when is_integer(N) -> 
                    Props = [ {original_filename, OriginalFilename} ],
                    case m_media:replace_file(TmpFile, Id, Props, Context) of
                        {ok, _} ->
                            Context1 = zp_render:wire(Actions, Context),
                            zp_render:growl("Uploaded the file.", Context1);
                        {error, eacces} ->
                            zp_render:growl_error("You don't have permission to change this page.", Context);
                        {error, _} ->
                            zp_render:growl_error("Error uploading the file.", Context)
                    end
            end;
        _ ->
            zp_render:growl("No file specified.", Context)
    end,

    % Close the dialog and optionally perform the post upload actions
    zp_render:wire({dialog_close, []}, ContextUpload).

