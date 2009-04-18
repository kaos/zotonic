%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-15
%%
%% @doc Search model, used as an interface to the search functions of modules etc.
%% A search question is represented by:
%%      {search_name, [PropList]}
%% The search result is represented by:  
%%      {search_result, [Results], [PropList], PagingInfo}
%% The search options are always sorted before the search is done.
%%
%% The model allows the following indices:
%%  page        current page number
%%  next        next page number (undefined if no next page)
%%  prev        previous page number (undefined if no previous page)
%%  pages       total number of pages
%%  total       total number of found items
%%  q_args      the query args for url building (all properties except paging)
%%  props       the search properties
%%  name        the name of the search
%%  result      list of ids for this page, can be a #rsc_list{}
%%
%% {% for id in m.search[{featured cat="accessoiries"}] %}
%%
%% Paging is done by fetching the first ?SEARCH_LIMIT rows and then return a slice from those rows.
%% This result set can be cached for a short while (depending on writes by the person_id associated with the visitor).


-module(m_search).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    
    search/2,
    get_result/3
]).

-include_lib("zophrenic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(SearchProps, #m{value=undefined} = M, Context) ->
    M#m{value=search(SearchProps, Context)};
m_find_value(Key, #m{value=#m_search_result{}} = M, Context) ->
    get_result(Key, M#m.value, Context).

%% @doc Transform a model value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=#m_search_result{result=undefined}}, _Context) ->
    [];
m_to_list(#m{value=#m_search_result{result=Result}}, _Context) ->
    Result#search_result.result;
m_to_list(#m{}, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined;
m_value(#m{value=#m_search_result{result=Result}}, _Context) ->
    Result#search_result.result.


%% @doc Perform a search, wrap the result in a m_search_result record
%% @spec search(Search, Context) -> #m_search_result{}
search({SearchName, Props}=Search, Context) ->
    {Page, PageLen, Props1} = get_paging_props(Props),
    Result = zp_search:search(Search, Context),
    Total1 = case Result#search_result.total of
        undefined -> length(Result);
        Total -> Total
    end,
    #m_search_result{result=Result, total=Total1, search_name=SearchName, search_props=Props1};
search(SearchName, Context) ->
    search({zp_convert:to_atom(SearchName), []}, Context).

get_result(N, Result, _Context) when is_integer(N) ->
    try
        lists:nth(N, Result#m_search_result.result)
    catch
        _:_ -> undefined
    end;
get_result(name, Result, _Context) ->
    Result#m_search_result.search_name;
get_result(props, Result, _Context) ->
    Result#m_search_result.search_props;
get_result(total, Result, _Context) ->
    Result#m_search_result.total;
get_result(_Key, _Result, _Context) ->
    undefined.
    


get_paging_props(Props) ->
    Page = proplists:get_value(page, Props, 1),
    PageLen = proplists:get_value(pagelen, Props, ?SEARCH_PAGELEN),
    P1 = proplists:delete(page, Props),
    P2 = proplists:delete(pagelen, P1),
    {Page, PageLen, lists:keysort(1, P2)}.
