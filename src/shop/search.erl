%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-12
%%
%% @doc 

-module(search).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    search/3
]).

-include_lib("zophrenic.hrl").


%% @doc Return a list of resource ids, featured ones first
%% @spec search(SearchSpec, Range, Context) -> #search_sql{}
search({featured, []}, _OffsetLimit, _Context) ->
    #search_sql{
        select="r.id",
        from="rsc r",
        order="r.is_featured desc, r.id desc",
        tables=[{rsc,"r"}]
    };

%% @doc Return a list of resource ids inside a category, featured ones first
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({featured, [{cat, Cat}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    #search_sql{
        select="r.id",
        from="rsc r, category rc, category ic",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1",
        order="r.is_featured desc, r.id desc",
        args=[CatId],
        tables=[{rsc,"r"}]
    };

%% @doc Return a list of featured resource ids inside a category having a object_id as predicate
%% @spec search(SearchSpec, Range, Context) -> IdList | {error, Reason}
search({featured, [{cat,Cat},{object,ObjectId},{predicate,Predicate}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    PredId = m_predicate:name_to_id_check(Predicate, Context),
    #search_sql{
        select="r.id",
        from="rsc r, category rc, category ic, edge e",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1
                and r.id = e.subject_id and e.predicate_id = $2 and e.object_id = $3",
        order="r.is_featured desc, r.id desc",
        args=[CatId, PredId, ObjectId],
        tables=[{rsc,"r"}]
    };

search({latest, [{cat, Cat}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    #search_sql{
        select="r.id",
        from="rsc r, category rc, category ic",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1",
        order="r.modified desc, r.id desc",
        args=[CatId],
        tables=[{rsc,"r"}]
    };

search({media_category_image, [{cat,Cat}]}, _OffsetLimit, Context) ->
    CatId = m_category:name_to_id_check(Cat, Context),
    #search_sql{
        select="m.filename",
        from="rsc r, category rc, category ic, rsc_media rm, media m",
        where="r.category_id = rc.id and rc.nr >= ic.lft and rc.nr <= ic.rght and ic.id = $1
                and rm.rsc_id = r.id and rm.media_id = m.id",
        tables=[{rsc,"r"}, {media, "m"}],
        args=[CatId]
    }.

