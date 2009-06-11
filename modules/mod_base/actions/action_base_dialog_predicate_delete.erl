%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-28
%%
%% @doc Open a dialog that asks confirmation to delete a predicate.

-module(action_base_dialog_predicate_delete).
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
    Postback = {delete_predicate_dialog, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the delete confirmation template. The next step will ask to delete the predicate.
%% @todo Check if the predicate is in use, if so show text that the predicate is in use and can't be deleted
%% @spec event(Event, Context1) -> Context2
event({postback, {delete_predicate_dialog, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            DTitle = "Confirm delete",
            Vars = [
                {on_success, OnSuccess},
                {id, Id}
            ],
            Html = zp_template:render("_action_dialog_predicate_delete.tpl", Vars, Context),
            {Html1, Context1} = zp_render:render_to_string(Html, Context),
            zp_render:wire({dialog, [{title, DTitle}, {text, Html1}]}, Context1);
        false ->
            zp_render:wire({growl, [{text, "You are not allowed to delete predicates."}, {type, "error"}]}, Context)
    end.
