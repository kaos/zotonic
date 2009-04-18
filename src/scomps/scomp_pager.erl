%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-18
%%
%% @doc Show the pager for the search result

-module(scomp_pager).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).
-export([test/0]).

-include("zophrenic.hrl").

% Pages before/after the current page
-define(DELTA, 2).
-define(SLIDE, ?DELTA + ?DELTA + 1).


init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, _Vars, Context, _State) ->
    Result       = proplists:get_value(result, Params),
    Dispatch     = proplists:get_value(dispatch, Params, search),
    CleanedArgs  = proplists:delete(dispatch, proplists:delete(result, Params)),
         
    DispatchArgs = case proplists:is_defined(qargs, CleanedArgs) of
        true -> CleanedArgs;
        false -> [{qargs,true}|CleanedArgs]
    end,
     
    case Result of
        #search_result{result=[]} ->
            {ok, ""};
        #search_result{pages=undefined} ->
            {ok, ""};
        #search_result{page=Page, pages=Pages} ->
            Html = build_html(Page, Pages, Dispatch, DispatchArgs, Context),
            {ok, Html};
        _ ->
            {error, "scomp_pager: search result is not a #search_result{}"}
    end.

build_html(Page, Pages, Dispatch, DispatchArgs, Context) ->
    {S,M,E} = pages(Page, Pages),
    Urls = urls(S, M, E, Dispatch, DispatchArgs, Context),
    [
        "\n<ul class=\"pager\">",
            [ url_to_li(Url, N, N == Page) || {N, Url} <- Urls ],
        "\n</ul"
    ].

url_to_li(sep, _, _) ->
    "\n<li class=\"pager-sep\">…</li>";
url_to_li(Url, N, false) ->
    ["\n<li><a href=\"",Url,"\">",integer_to_list(N),"</a></li>"];
url_to_li(Url, N, true) ->
    ["\n<li class=\"current\"><a href=\"",Url,"\">",integer_to_list(N),"</a></li>"].

pages(Page, Pages) ->
    Start = case Page - ?DELTA > 1 of
        true ->
            % Separate "1 ... 3"
            [1];
        false ->
            % Together "1 .. "
            seq(1, min(?SLIDE, Pages))
    end,
    Middle = case Page - ?DELTA > 1 of
        true ->
            seq(max(1,Page-?DELTA), min(Pages-1,Page+?DELTA));
        false ->
            []
    end,
    End = case Pages > 1 of
        true ->
            [Pages];
        false ->
            []
    end,
    {Start, Middle, End}.


urls(Start, Middle, End, Dispatch, DispatchArgs, Context) ->
    UrlStart  = [ {N, zp_dispatcher:url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- Start ],
    UrlMiddle = [ {N, zp_dispatcher:url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- Middle ],
    UrlEnd    = [ {N, zp_dispatcher:url_for(Dispatch, [{page,N}|DispatchArgs], Context)} || N <- End ],
    {Part1,Next} = case Middle of
        [] ->
            {UrlStart, lists:max(Start) + 1};
        [N|_] when N == 2 -> 
            % Now Start is always of the format [1]
            {UrlStart ++ UrlMiddle, lists:max(Middle) + 1};
        _ ->
            {UrlStart ++ [sep|UrlMiddle], lists:max(Middle) + 1}
    end,
    case End of
        [] ->
            Part1;
        [M|_] -> 
            if
                M == Next -> Part1 ++ UrlEnd;
                true -> Part1 ++ [sep, UrlEnd]
            end
    end.


seq(A,B) when B < A -> [];
seq(A,B) -> lists:seq(A,B).


min(A,B) when A < B -> A;
min(_,B) -> B.

max(A,B) when A > B -> A;
max(_,B) -> B.


test() ->
    C = zp_context:new(),
    R = #search_result{result=[a], pages=100, page=10},
    {ok, H} = render([{result,R}], [], C, []),
    list_to_binary(H).
    