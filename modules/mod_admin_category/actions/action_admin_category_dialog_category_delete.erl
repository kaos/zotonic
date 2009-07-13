%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-10
%%
%% @doc Open a dialog that asks confirmation to delete a category. Optionaly move pages to another category.

-module(action_admin_category_dialog_category_delete).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    Id = zp_convert:to_integer(proplists:get_value(id, Args)),
    OnSuccess = proplists:get_all_values(on_success, Args),
    Postback = {delete_category_dialog, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the category.
%% @spec event(Event, Context1) -> Context2
event({postback, {delete_category_dialog, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            Count = m_category:get_page_count(Id, Context),
            Vars = [ {on_success, OnSuccess}, {id, Id}, {page_count, Count} ],
            zp_render:dialog("Confirm delete", "_action_dialog_category_delete.tpl", Vars, Context);
        false ->
            zp_render:growl_error("Only administrators can delete categories.", Context)
    end;

%% @doc Handle the form postback. Optionally renaming existing categories.
event({submit, {delete_category, _Props}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            Id = list_to_integer(zp_context:get_q("id", Context)),
            TransferId = case zp_context:get_q("transfer_id") of
                [] -> undefined;
                undefined -> undefined;
                T -> list_to_integer(T)
            end,
            case m_category:delete(Id, TransferId, Context) of
                ok ->
                    %% Page refresh, good enough for the current usage.
                    zp_render:wire({reload, []}, Context);
                {error, Reason} ->
                    Error = io_lib:format("Could not delete the categorie (~p)", [Reason]),
                    zp_render:growl_error(Error, Context)
            end;
        false ->
            zp_render:growl_error("Only administrators can delete categories.", Context)
    end.
