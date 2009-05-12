%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-05-12
%%
%% @doc Open a dialog with some fields to make a new predicate.

-module(action_dialog_predicate_new).
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
    Postback = {predicate_new_dialog, Title, Redirect},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @doc Fill the dialog with the new page form. The form will be posted back to this module.
%% @spec event(Event, Context1) -> Context2
event({postback, {predicate_new_dialog, Title, Redirect}, _TriggerId, _TargetId}, Context) ->
    DTitle = "Make a new page",
    Vars = [
        {delegate, atom_to_list(?MODULE)},
        {redirect, Redirect },
        {title, Title}
    ],
    Html = zp_template:render("_action_dialog_predicate_new.tpl", Vars, Context),
    {Html1, Context1} = zp_render:render_to_string(Html, Context),
    zp_render:wire({dialog, [{title, DTitle}, {text, Html1}]}, Context1);


event({submit, predicate_new, _TriggerId, _TargetId}, Context) ->
    Title   = zp_context:get_q("new_predicate_title", Context),
    Redirect = zp_context:get_q("redirect", Context),

    Props = [
        {title, Title},
        {name, zp_string:to_name(Title)}
    ],
    {ok, Id} = m_predicate:insert(Props, Context),

    % Close the dialog and optionally redirect to the edit page of the new resource
    Context2 = zp_render:wire({dialog_close, []}, Context),
    case zp_convert:to_bool(Redirect) of
        false ->
            Context2;
        true ->
            Location = zp_dispatcher:url_for(admin_predicate_edit, [{id, Id}], Context2),
            zp_render:wire({redirect, [{location, Location}]}, Context2)
    end.

