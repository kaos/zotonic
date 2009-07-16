%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-22
%%
%% @doc Callback to set the html area with the variant select and price. 

-module(action_shop_shop_select_variant).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    render_action/4,
    event/2
]).

-include_lib("zophrenic.hrl").

render_action(TriggerId, TargetId, Args, Context) -> 
    Id      = proplists:get_value(id, Args),
    ?ASSERT(is_integer(Id), {id_must_be_integer, Id}),
	{PostbackMsgJS, _PickledPostback} = zp_render:make_postback({shop_select_variant, Id}, postback, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.

event({postback, {shop_select_variant, Id}, _TriggerId, _TargetId}, Context) ->
    Variant = zp_convert:to_binary(zp_context:get_q("triggervalue", Context)),
    Vs = m_shop_product:get_variants_as_proplist(Id, Context),
    Vars = [
        {rsc_id, Id},
        {variant, Variant},
        {variants, Vs}
    ],
    Html = zp_template:render("_product_price.tpl", Vars, Context),
    Context1 = zp_render:update("product-price", Html, Context),
    MediaId = case find_variant(Vs, Variant) of
        undefined -> undefined;
        V -> proplists:get_value(media_id, V)
    end,
    Props = case MediaId of
        undefined -> m_media:depiction(Id, Context);
        _ -> m_media:get(MediaId, Context)
    end,
    case Props of
        undefined -> Context1;
        _ ->
            Filename = proplists:get_value(filename, Props),
            {ok, Tag} = zp_media_tag:tag(Filename, [{alt,""}, {crop,true}, {width,300}]),
            zp_render:update("variant-image", Tag, Context1)
    end.


find_variant([], _Variant) ->
    undefined;
find_variant([H|T], Variant) ->
    case proplists:get_value(variant, H) of
        Variant -> H;
        _ -> find_variant(T, Variant)
    end.
