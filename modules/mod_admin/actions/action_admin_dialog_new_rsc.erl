%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-27
%%
%% @doc Open a dialog with some fields to make a new page/resource.

-module(action_admin_dialog_new_rsc).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Title = proplists:get_value(title, Args),
    Redirect = proplists:get_value(redirect, Args, true),
    SubjectId = proplists:get_value(subject_id, Args),
    Predicate = proplists:get_value(predicate, Args),
    Postback = {new_rsc_dialog, Title, Redirect, SubjectId, Predicate},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {new_rsc_dialog, Title, Redirect, SubjectId, Predicate}, _TriggerId, _TargetId}, Context) ->
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {redirect, Redirect },
        {subject_id, SubjectId},
        {predicate, Predicate},
        {title, Title}
    ],
    zp_render:dialog("Make a new page", "_action_dialog_new_rsc.tpl", Vars, Context);


event({submit, new_page, _TriggerId, _TargetId}, Context) ->
    Title   = zp_context:get_q("new_rsc_title", Context),
    GroupId = list_to_integer(zp_context:get_q("group_id", Context)),
    CatId   = list_to_integer(zp_context:get_q("category_id", Context)),
    Redirect = zp_context:get_q("redirect", Context),
    SubjectId = zp_context:get_q("subject_id", Context),
    Predicate = zp_context:get_q("predicate", Context),
    IsPublished = zp_context:get_q("is_published", Context),

    Props = [
        {category_id, CatId},
        {group_id, GroupId},
        {title, Title},
        {is_published, IsPublished}
    ],
    {ok, Id} = m_rsc_update:insert(Props, Context),

    % Optionally add an edge from the subject to this new resource
    Context1 = case SubjectId of
        [] -> 
            Context;
        L when is_list(L) ->
            action_admin_link:do_link(zp_convert:to_integer(SubjectId), Predicate, Id, Context);
        _ ->
            Context
    end,
    
    % Close the dialog and optionally redirect to the edit page of the new resource
    Context2 = zp_render:wire({dialog_close, []}, Context1),
    case zp_convert:to_bool(Redirect) of
        false ->
            Context2;
        true ->
            Location = zp_dispatcher:url_for(admin_edit_rsc, [{id, Id}], Context2),
            zp_render:wire({redirect, [{location, Location}]}, Context2)
    end.

