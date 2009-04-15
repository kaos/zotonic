%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-15
%%
%% @doc Search the database, interfaces to specific search routines.

-module(zp_search).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    search/2
]).


search({SearchName, Props}, Context) ->
    % todo: fetch paging information from props
    PropsSorted = lists:keysort(1, Props),
    search:search({SearchName, PropsSorted}, Context);
search(Name, Context) ->
    search({zp_convert:to_atom(Name), []}, Context).


