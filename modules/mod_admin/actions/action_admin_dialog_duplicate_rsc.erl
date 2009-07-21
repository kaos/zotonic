%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-21
%%
%% @doc Duplicate a resource, replace the title with the one entered in a dialog.

-module(action_admin_dialog_duplicate_rsc).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zotonic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = z_convert:to_integer(proplists:get_value(id, Args)),
    Postback = {duplicate_rsc_dialog, Id},
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the duplicate page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {duplicate_rsc_dialog, Id}, _TriggerId, _TargetId}, Context) ->
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {id, Id}
    ],
    z_render:dialog("Duplicate page.", "_action_dialog_duplicate_rsc.tpl", Vars, Context);


event({submit, {duplicate_page, ActionProps}, _TriggerId, _TargetId}, Context) ->
    Id = proplists:get_value(id, ActionProps),
    Title   = z_context:get_q("new_rsc_title", Context),
    IsPublished = z_context:get_q("is_published", Context),

    Props = [
        {title, Title},
        {is_published, IsPublished}
    ],
    {ok, NewId} = m_rsc_update:duplicate(Id, Props, Context),

    % Close the dialog and redirect to the edit page of the new resource
    Context1 = z_render:wire({dialog_close, []}, Context),
    Location = z_dispatcher:url_for(admin_edit_rsc, [{id, NewId}], Context1),
    z_render:wire({redirect, [{location, Location}]}, Context1).
