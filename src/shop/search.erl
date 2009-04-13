%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-12
%%
%% @doc 

-module(search).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    search/2,
    search/3,
    search_media/2,
    search_media/3
]).

-define(OFFSET_LIMIT, {1,10}).


search(S, Context) ->
    search(S, ?OFFSET_LIMIT, Context).
    
%% @doc Return a list of featured resource ids inside a category
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({category_featured, CatId}, {Offset,Limit}, Context) ->
    Rows = zp_db:q("
            select r.id
            from rsc r, category rc, category ic
            where r.category_id = rc.id
              and r.is_published
              and r.publication_start <= now()
              and r.publication_end >= now()
              and rc.nr >= ic.lft
              and rc.nr <= ic.rght
              and ic.id = $1
            order by r.is_featured desc, r.id desc
            limit $3
            offset $2
        ", [CatId, Offset-1, Limit], Context),
    [ Col || {Col} <- Rows ].


search_media(S, Context) ->
    search_media(S, ?OFFSET_LIMIT, Context).
    
search_media({category_image, CatId}, {Offset,Limit}, Context) ->
    Rows = zp_db:q("
            select m.filename 
            from rsc r, category rc, category ic, rsc_media rm, media m
            where r.category_id = rc.id
              and r.is_published
              and r.publication_start <= now()
              and r.publication_end >= now()
              and rc.nr >= ic.lft
              and rc.nr <= ic.rght
              and ic.id = $1
              and rm.rsc_id = r.id
              and rm.media_id = m.id
            order by r.is_featured desc
            limit $3
            offset $2
        ", [CatId, Offset-1, Limit], Context),
    [ Col || {Col} <- Rows ].
    

    


