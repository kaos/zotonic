%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-27
%%
%% @doc Open a dialog with some fields to upload a new media.

-module(action_dialog_media_upload).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Title = proplists:get_value(title, Args),
    RscId = proplists:get_value(rsc_id, Args),
    GroupId = proplists:get_value(group_id, Args),
    Actions = proplists:get_all_values(action, Args),
    Postback = {media_upload_dialog, Title, RscId, GroupId, Actions},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {media_upload_dialog, Title, RscId, GroupId, Actions}, _TriggerId, _TargetId}, Context) ->
    DTitle = "Add a new media file",
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {rsc_id, RscId },
        {group_id, GroupId},
        {title, Title},
        {actions, Actions}
    ],
    Html = zp_template:render("_action_dialog_media_upload.tpl", Vars, Context),
    {Html1, Context1} = zp_render:render_to_string(Html, Context),
    zp_render:wire({dialog, [{title, DTitle}, {text, Html1}]}, Context1);


event({submit, {media_upload, EventProps}, _TriggerId, _TargetId}, Context) ->
    Actions = proplists:get_value(actions, EventProps, []),
    Title   = zp_context:get_q("new_media_title", Context),
    GroupId = list_to_integer(zp_context:get_q("group_id", Context)),
    RscId   = zp_convert:to_integer(zp_context:get_q("rsc_id", Context)),
    File    = zp_context:get_q("file", Context),
    Context1 = case File of
        {OriginalFilename, TmpFile} ->
            Props = [{title, Title}, {original_filename, OriginalFilename}, {group_id, GroupId}],
            Result = case RscId of
                undefined -> m_media:insert_file(TmpFile, Props, Context);
                _ -> m_media:insert_file_rsc(TmpFile, RscId, Props, Context)
            end,
            case Result of
                {ok, _MediaId} ->
                    ?DEBUG(Actions),
                    zp_render:wire([{growl, [{text, "Uploaded the file."}]} | Actions], Context);
                {error, _Error} ->
                    zp_render:wire({growl, [{text, "Error uploading the file."}, {type, "error"}]}, Context)
            end;
        _ ->
            zp_render:wire({growl, [{text, "No file specified."}]}, Context)
    end,

    % Close the dialog and optionally perform the post upload actions
    zp_render:wire({dialog_close, []}, Context1).

