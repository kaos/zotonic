%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(resource_helloworld).
-export([event/2, periodic/2]).

-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->
    {Incr, Context2} = zp_context:incr_session(helloworld_counter, 1, Context),
    Context3         = zp_context:set_context(helloworld_counter, Incr, Context2),
    
    Html = zp_template:render("helloworld.tpl", Context3),
    % Html = zp_template:render("idtest.tpl", Context2),
    _Pid = zp_context:spawn_link_page(?MODULE, periodic, [], Context3),
    zp_context:output(Html, Context3).


event({postback, show_confirm, _TriggerId, _TargetId}, Context) ->
    zp_render:wire({confirm, [{text,"This is a Javascript Confirm"},{postback,confirm_ok}]}, Context);
event({postback, show_alert, _TriggerId, _TargetId}, Context) ->
    zp_render:wire({alert, [{text,"This is a Javascript Alert"}]}, Context);
event({postback, confirm_ok, _TriggerId, _TargetId}, Context) ->
    zp_render:wire({alert, [{text,"You confirmed"}]}, Context);
event({postback, show_growl, _TriggerId, _TargetId}, Context) ->
    zp_render:wire({growl, [{text,"This is a Growl Alert that disappears automatically after some time."}]}, Context);
event({postback, show_growl_stay, _TriggerId, _TargetId}, Context) ->
    zp_render:wire({growl, [{text,"This is a Growl Alert that stays until you close it."},{stay,1}]}, Context);

event({postback, fill_content, _TriggerId, _TargetId}, Context) ->
    zp_render:update("content", "Hello World<br/>", Context);
event({postback, insert_top, _TriggerId, _TargetId}, Context) ->
    zp_render:insert_top("content", "At the top<br/>", Context);
event({postback, insert_bottom, _TriggerId, _TargetId}, Context) ->
    zp_render:insert_bottom("content", "At the bottom<br/>", Context);

event({drop, Drag, Drop}, Context) ->
    zp_render:wire({growl, [{text,["You dropped ",Drag#dragdrop.tag," on ",Drop#dragdrop.tag]}]}, Context);
event({drag, Drag, Drop}, Context) ->
    zp_render:wire({growl, [{text,["You dragged ",Drag#dragdrop.tag," to ",Drop#dragdrop.tag]}]}, Context);

event({sort, Drags, Drop}, Context) ->
    DragIds = [ Id || #dragdrop{id=Id} <- Drags],
    Msg = io_lib:format("Result ~p on ~p",[DragIds,Drop#dragdrop.id]),
    zp_render:wire({growl, [{text,Msg}]}, Context);

event({submit, _Tag, _FormId, _TargetId}, Context) ->
    Email = zp_context:get_q_validated("email", Context),
    zp_render:wire({growl, [{text,["You posted a valid email address \"",Email,"\""]}]}, Context);
    
event(Event, Context) ->
    Error = io_lib:format("~p: unknown event ~p", [?MODULE,Event]),
    zp_render:wire({growl, [{text,Error},{stay,1}]}, Context).



periodic(_Args, Context) ->
    Date = httpd_util:rfc1123_date(),
    zp_context:add_script_session([<<"zp_growl_add('According to the server, the Universal Sprout Time is now<br/><strong>">>,Date,<<"</strong>', 0);">>], Context),
    timer:sleep(10000),
    ?MODULE:periodic([], Context).


    