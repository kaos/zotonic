%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-28
%%
%% @doc Open a dialog that asks confirmation to delete a media.

-module(action_admin_dialog_delete_media).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {delete_media_dialog, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the media
%% @spec event(Event, Context1) -> Context2
event({postback, {delete_media_dialog, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case z_acl:media_editable(Id, Context) of
        true ->
            Vars = [
                {on_success, OnSuccess},
                {id, Id}
            ],
            z_render:dialog("Confirm delete", "_action_dialog_delete_media.tpl", Vars, Context);
        false ->
            z_render:growl_error("You are not allowed to delete this media.", Context)
    end.
