%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_home).
-export([event/2]).
-author("Tim Benniks <tim@timbenniks.com>").
-include_lib("resource_html.hrl").
 
html(_ReqProps, Context) ->
    Html = zp_template:render("home.tpl", Context),
    zp_context:output(Html, Context).    

event({postback, show_growl_search, _TriggerId, _TargetId}, Context) ->
    zp_render:wire({growl, [{text,"You clicked on search! Sweet..."}]}, Context);

event({postback, show_growl_newsletter, _TriggerId, _TargetId}, Context) ->
    zp_render:wire({growl, [{text,"YÅou have clicked on newsletter! Sweeter!"}]}, Context);

event(Event, Context) ->
    Error = io_lib:format("~p: unknown event ~p", [?MODULE,Event]),
    zp_render:wire({growl, [{text,Error},{stay,1}]}, Context).