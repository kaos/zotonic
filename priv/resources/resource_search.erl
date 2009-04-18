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
	CatId  = zp_context:get(cat_id, Context1),
	Qs     = zp_context:get_q("qs", Context1),
	Page   = try list_to_integer(zp_context:get_q("page", Context1, "1")) catch _:_ -> 1 end,
    Result = zp_search:search_pager({fulltext, [{cat,CatId},{text,Qs}]}, Page, Context1),
    Vars   = [
        {cat_id, CatId},
        {page, Page},
        {text, Qs},
        {result, Result}
    ],
    Html = zp_template:render("search.tpl", Vars, Context1),
	zp_context:output(Html, Context1).