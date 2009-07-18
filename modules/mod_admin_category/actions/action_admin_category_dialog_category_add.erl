%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-13
%%
%% @doc Open a dialog to let the user add a new category. Allows to select the parent category, refreshes the category overview.

-module(action_admin_category_dialog_category_add).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {dialog_category_add, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the category.
%% @spec event(Event, Context1) -> Context2
event({postback, {dialog_category_add, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            Vars = [ {on_success, OnSuccess} ],
            zp_render:dialog("Add category", "_action_dialog_category_add.tpl", Vars, Context);
        false ->
            zp_render:growl_error("Only administrators can add categories.", Context)
    end;

%% @doc Handle the form postback. Optionally renaming existing categories.
event({submit, {category_add, Options}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            Title    = zp_context:get_q_validated("title", Context),
            Name     = zp_context:get_q_validated("name", Context),
            GroupId  = zp_convert:to_integer(zp_context:get_q("group_id", Context, undefined)),
            ParentId = zp_convert:to_integer(zp_context:get_q("category_id", Context, undefined)),
            
            Props = [
                {is_published, true},
                {group_id, GroupId},
                {category, category},
                {name, Name},
                {title, Title}
            ],
            
            case m_rsc:insert(Props, Context) of
                {ok, Id} ->
                    case ParentId of
                        PId when is_integer(PId) ->  m_category:move_below(Id, PId, Context);
                        undefined -> nop
                    end,
                    zp_render:wire(proplists:get_all_values(on_success, Options), Context);
                {error, duplicate_name} ->
                    zp_render:growl_error("This category exists already. Please use another name.", Context);
                {error, Reason} ->
                    Error = io_lib:format("Could not insert the categorie (~p)", [Reason]),
                    zp_render:growl_error(Error, Context)
            end;
        false ->
            zp_render:growl_error("Only administrators can delete categories.", Context)
    end.
