%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Edit an sku

-module(resource_admin_shop_sku_edit).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    resource_exists/2,
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

%% @todo Change this into "visible" and add a view instead of edit template.
is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(true, visible, "id", ReqData, Context).


resource_exists(ReqData, Context) ->
    Context1 = ?WM_REQ(ReqData, Context),
    Context2 = zp_context:ensure_all(Context1),
    Id = zp_context:get_q("id", Context2),
    try
        IdN = list_to_integer(Id),
        Context3 = zp_context:set(id, IdN, Context2),
        case m_shop_product:get_sku(IdN, Context3) of
            undefined -> 
                ?WM_REPLY(false, Context3);
            Sku -> 
                Context4 = zp_context:set(sku, Sku, Context3),
                ?WM_REPLY(true, Context4)
        end
    catch
        M:E ->
            ?WM_REPLY(false, Context2)
    end.


html(Context) ->
    Id =  zp_context:get(id, Context),
    Vars = [
        {id, Id},
        {sku, zp_context:get(sku, Context)}
    ],
    Html = zp_template:render("admin_shop_sku_edit.tpl", Vars, Context),
	zp_context:output(Html, Context).


%% @doc Handle the submit of the resource edit form
event({submit, skuform, _FormId, _TargetId}, Context) ->
    Id = zp_context:get_q("id", Context),
    Props = [
        {variant, zp_context:get_q("variant", Context)}
    ],
    m_shop_product:update_sku(zp_convert:to_integer(Id), Props, Context),
    zp_render:wire({growl, [{text,"Saved Sku."}]}, Context).

