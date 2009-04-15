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
    search/3
]).

-define(OFFSET_LIMIT, {1,10}).

-include_lib("zophrenic.hrl").

search(S, Context) ->
    search(S, ?OFFSET_LIMIT, Context).


%% @doc Return a list of resource ids, featured ones first
%% @spec search(SearchSpec, Range, Context) -> {ok, IdList, Total} | {error, Reason}
search({featured, []}, {Offset,Limit}, Context) ->
    Rows = zp_db:q("
            select r.id
            from rsc r
            where r.is_published
              and r.publication_start <= now()
              and r.publication_end >= now()
            order by r.is_featured desc, r.id desc
            limit $2
            offset $1
        ", [Offset-1, Limit], Context),
    #search_result{result=[ Col || {Col} <- Rows ]};

%% @doc Return a list of resource ids inside a category, featured ones first
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({featured, [{cat, Cat}]}, {Offset,Limit}, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
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
    #search_result{result=[ Col || {Col} <- Rows ]};

%% @doc Return a list of featured resource ids inside a category having a object_id as predicate
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({featured, [{cat,Cat},{object,ObjectId},{predicate,Predicate}]}, {Offset,Limit}, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    PredId = m_predicate:name_to_id_check(Predicate, Context),
    Rows = zp_db:q("
            select r.id
            from rsc r, category rc, category ic, edge e
            where r.category_id = rc.id
              and r.is_published
              and r.publication_start <= now()
              and r.publication_end >= now()
              and rc.nr >= ic.lft
              and rc.nr <= ic.rght
              and ic.id = $1
              and r.id = e.subject_id
              and e.predicate_id = $4
              and e.object_id = $5
            order by r.is_featured desc, r.id desc
            limit $3
            offset $2
        ", [CatId, Offset-1, Limit, PredId, ObjectId], Context),
    #search_result{result=[ Col || {Col} <- Rows ]};

search({media_category_image, CatId}, {Offset,Limit}, Context) ->
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
    #search_result{result=[ Col || {Col} <- Rows ]}.
    
