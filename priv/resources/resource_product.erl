%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_product).
-author("Tim Benniks <tim@timbenniks.com>").

-export([
    resource_exists/2
]).

-include_lib("resource_html.hrl").

resource_exists(ReqData, Context) ->
    Context1  = ?WM_REQ(ReqData, Context),
    ContextQs = zp_context:ensure_qs(Context1),
    try
        Id = list_to_integer(zp_context:get_q("id", ContextQs)),
        Ctx = zp_context:set(id, Id, ContextQs),
        ?WM_REPLY(m_rsc:exists(Id, Ctx), Ctx)
    catch
        _:_ -> ?WM_REPLY(false, ContextQs)
    end.

html(Context) ->
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

