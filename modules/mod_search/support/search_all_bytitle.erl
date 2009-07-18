%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-04
%%
%% @doc Return a list of ids of a category, sorted by title.  This needs to fetch and sort all 
%% resources of that category, so use this with care for situations where you know that the number
%% of returned resources is relatively low.

-module(search_all_bytitle).
-author("Marc Worrell <marc@worrell.nl>").

%% interface functions
-export([
    search/2
]).

-include_lib("zophrenic.hrl").

search(Cat, Context) ->
    case m_category:name_to_id(Cat, Context) of
        {ok, CatId} ->
            {Left,Right} = m_category:get_range(CatId, Context),
            Ids = zp_db:q("select id from rsc where pivot_category_nr >= $1 and pivot_category_nr <= $2", [Left,Right], Context),
            IdTitles = [ {?TR(m_rsc:p(Id, title, Context), Context), Id} || {Id} <- Ids, m_rsc:is_visible(Id, Context) ],
            Sorted = lists:sort(IdTitles),
            #search_result{result=Sorted, all=Sorted, total=length(Sorted)};
        {error, _Reason} ->
            #search_result{}
    end.
