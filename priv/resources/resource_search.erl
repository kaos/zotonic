%% @author Tim Benniks <tim@timbenniks.com>
%% @copyright 2009 Tim Benniks.
%% @doc Home Page webmachine_resource.

-module(resource_search).
-author("Tim Benniks <tim@timbenniks.com>").

-export([resource_exists/2]).
-include_lib("resource_html.hrl").

resource_exists(_ReqProps, Context) ->
    ContextQs = zp_context:ensure_qs(Context),
    Cat = zp_context:get_q("cat", ContextQs, product),
    case m_category:name_to_id(Cat, ContextQs) of
         {ok, CatId} ->
             {true, zp_context:set([{cat_id, CatId}], ContextQs)};
         {error, _} -> 
             {false, ContextQs}
     end.
    
html(_ReqProps, Context) ->
    Context1 = zp_context:ensure_all(Context),
	MenuList = [
				[{title, "home"}, {uri, "/"}], 
				[{title, "Basic page"}, {uri, "/page/basic"}],
				[{title, "Product page"}, {uri, "/product/shimano/105-ST-5600"}],
				[{title, "Bike page"}, {uri, "/bike/trek/urban"}]
			],
	CatId  = zp_context:get(cat_id, Context1),
	Qs     = zp_context:get_q("qs", Context1),
    Result = zp_search:search({fulltext, [{cat,CatId},{text,Qs}]}, {1,100}, Context1),
    Vars   = [
        {menu_list, MenuList},
        {cat_id, CatId},
        {text, Qs},
        {result, Result}
    ],
    Html = zp_template:render("search.tpl", Vars, Context1),
	zp_context:output(Html, Context1).