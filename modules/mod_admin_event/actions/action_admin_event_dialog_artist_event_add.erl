%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-18
%%
%% @doc Easy dialog to add an event for an artist.

-module(action_admin_event_dialog_artist_event_add).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) ->
    OnSuccess = proplists:get_all_values(on_success, Args),
    Id = zp_convert:to_integer(proplists:get_value(id, Args)),
    Postback = {dialog_artist_event_add, Id, OnSuccess},
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback(Postback, click, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


event({postback, {dialog_artist_event_add, Id, OnSuccess}, _TriggerId, _TargetId}, Context) ->
    Vars = [
        {id, Id},
        {on_success, OnSuccess}
    ],
    zp_render:dialog("Add an event for the artist.", "_action_dialog_artist_event_add.tpl", Vars, Context);


%% @doc Delete an username from an user.
%% @spec event(Event, Context1) -> Context2
event({submit, {event_add, Props}, _TriggerId, _TargetId}, Context) ->
    ArtistId = proplists:get_value(id, Props),
    Title = zp_context:get_q_validated("title", Context),
    Venue = zp_context:get_q("venue", Context),
    Genres = zp_context:get_q_all("genre", Context),
    GroupId = list_to_integer(zp_context:get_q("group_id", Context)),

    EventProps = [
        {is_published, true},
        {visible_for, 0},
        {category, event},
        {title, lists:flatten(Title)},
        {group_id, GroupId}
    ],
    
    F = fun(Ctx) ->
        case m_rsc:insert(EventProps, Ctx) of
            {ok, EventId} ->
                m_edge:insert(EventId, performer, ArtistId, Ctx),
                case Venue of
                    undefined -> nop;
                    _ -> m_edge:insert(EventId, atvenue, list_to_integer(Venue), Ctx)
                end,
                [ m_edge:insert(EventId, relation, list_to_integer(Genre), Ctx) || Genre <- Genres, Genre /= [] ],
                {ok, EventId};
            {error, InsReason} ->
                throw({error, InsReason})
        end
    end,
    
    case zp_db:transaction(F, Context) of
        {ok, EventId} ->
            Context1 = zp_render:growl(["Created the event ",zp_html:escape(Title), "."], Context),
            zp_render:wire([
                    {dialog_close, []},
                    {redirect, [{dispatch, admin_edit_rsc}, {id, EventId}]}
                    | proplists:get_all_values(on_success, Props) ], Context1);
        {rollback, {Error, _CallStack}} ->
            case Error of
                _OtherError ->
                    ?DEBUG(Error),
                    zp_render:growl_error("Could not create the event. Sorry.", Context)
            end
    end.
