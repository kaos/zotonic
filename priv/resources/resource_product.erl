%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_product).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    resource_exists/2,
    event/2
]).

-include_lib("resource_html.hrl").

resource_exists(_ReqProps, Context) ->
    ContextQs = zp_context:ensure_qs(Context),
    try
        Id = list_to_integer(zp_context:get_q("id", ContextQs)),
        Ctx = zp_context:set(id, Id, ContextQs),
        {m_rsc:exists(Id, Ctx), Ctx}
    catch
        _:_ -> {false, ContextQs}
    end.

html(_ReqProps, Context) ->
	Id = zp_context:get(id, Context),
	CatId = m_rsc:p(Id, category_id, Context),
	CatBrand = shop:category_brands(CatId, Context),
    RscCount = shop:category_rsc_count(CatId, Context),
	Vars = [
        {rsc_id, Id},
	    {cat_brand, CatBrand},
	    {prod_count, RscCount},
	    {cart_count, shop_cart:in_cart(Id, Context)}
	],
    Html = zp_template:render("product.tpl", Vars, Context),
	zp_context:output(Html, Context).

event({postback, show_basket_notice, _TriggerId, _TargetId}, Context1) ->
    %%zp_render:wire({fade_in, [{speed,300}, {target, "product-notice"}]}, Context1),
    zp_render:wire({growl, [{text,"Shimano 105 ST-5600 toegevoegd aan de winkelmand."},{stay,0}, {type, "notice"}]}, Context1);

event(Event, Context1) ->
    Error = io_lib:format("~p: unknown event ~p", [?MODULE,Event]),
    zp_render:wire({growl, [{text,Error},{stay,1}]}, Context1).