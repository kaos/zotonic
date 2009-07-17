%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Render the menu.  Add classes to highlight the current item.

-module(scomp_menu_menu).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zophrenic.hrl").


%%      init(Args) -> {ok, State} | {error, Error}
%%      render(Params, Context, State) -> {ok, NewContext} | {ok, iolist()} | {error, Error}
%%      code_change(OldVsn, State, Extra) -> {ok, NewState}
%%      terminate(Reason) -> ok
%%      
%%      	State = term()
%%      	Params = proplist()
%%      	Context = context()
%%      
%%      varies(Params, Context) -> {EssentialParams, MaxAge, Depends} | undefined
%%      
%%      	Params = proplist()
%%      	MaxAge = integer()
%%          Depends = TermList

% Menu structure is a bit like:
%
% <ul id="navigation">
% 	<li id="nav-item-1" class="first current">
% 		<a href="" class="home-page">home</a>
% 	</li>
% 	<li id="nav-item-2">
% 		<a href="" class="about-page">about</a>
% 	</li>
% 	<li id="nav-item-3" class="last">
% 		<a href="" class="contact-page">contact</a>
% 	</li>
% </ul>


init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, _Vars, Context, _State) ->
    Id = proplists:get_value(id, Params),
    Menu = get_menu(Context),
    ?DEBUG(Menu),
    LIs = build_menu(Menu, Id, 1, [], Context),
    UL = ["<ul id=\"navigation\">", LIs, "</ul>"],
    {ok, list_to_binary(UL)}.


build_menu([], _Id, _Nr, Acc, _Context) ->
    lists:reverse(Acc);
build_menu([{N,[]} | T], Id, Nr, Acc, Context) ->
    LI = menu_item(N, T, Id, Nr, Context),
    build_menu(T, Id, Nr+1, [ [LI,"</li>"] | Acc ], Context);
build_menu([{N,SubMenu} | T], Id, Nr, Acc, Context) ->
    LI = menu_item(N, T, Id, Nr, Context),
    SubLIs = build_menu(SubMenu, Id, 1, [], Context),
    build_menu(T, Id, Nr+1, [ [LI,"<ul>",SubLIs,"</ul></li>"] | Acc ], Context);
build_menu([N | T], Id, Nr, Acc, Context) when is_integer(N) ->
    LI = menu_item(N, T, Id, Nr, Context),
    build_menu(T, Id, Nr+1, [ [LI,"</li>"] | Acc ], Context).


menu_item(N, T, Id, Nr, Context) ->
    First = case Nr of 1 -> " first "; _ -> [] end,
    Last  = case T of [] -> " last "; _ -> [] end,
    Current = case N == Id of true -> " current "; _ -> [] end,
    [
        "<li id=\"nav-item-", integer_to_list(Nr), "\" class=\"",First,Last,Current,"\">",
            "<a href=\"", m_rsc:p(N, page_url, Context), "\" class=\"", m_rsc:p(N, slug, Current), "\">",
                ?TR(m_rsc:p(N, title, Context), Context),
        "</a>"
    ]. 


%% @doc Fetch the menu from the site configuration.
%% @spec get_menu(Context) -> list()
get_menu(Context) ->
    case m_config:get(menu, menu_default, Context) of
        undefined -> [];
        Props -> proplists:get_value(menu, Props, [])
    end.
