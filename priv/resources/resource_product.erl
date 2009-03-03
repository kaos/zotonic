%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_product).
-author("Tim Benniks <tim@timbenniks.com>").
-export([event/2]).
-include_lib("resource_html.hrl").

html(_ReqProps, Context) ->
	MenuList = [
				[{title, "home"}, {uri, "/"}], 
				[{title, "fietsen"}, {uri, "/page/fietsen"}],
				[{title, "Product page"}, {uri, "/product/shimano/105-ST-5600"}]
			],
	Context1 = zp_context:set_context(menu_list, MenuList, Context),

    Html = zp_template:render("product.tpl", Context1),
	zp_context:output(Html, Context1).
	
event({postback, show_basket_notice, _TriggerId, _TargetId}, Context1) ->
    %%zp_render:wire({fade_in, [{speed,300}, {target, "product-notice"}]}, Context1),
    zp_render:wire({growl, [{text,"Shimano 105 ST-5600 toegevoegd aan de winkelmand."},{stay,0}, {type, "notice"}]}, Context1);

event(Event, Context1) ->
    Error = io_lib:format("~p: unknown event ~p", [?MODULE,Event]),
    zp_render:wire({growl, [{text,Error},{stay,1}]}, Context1).