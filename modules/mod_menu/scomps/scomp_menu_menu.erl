%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Render the menu.  Add classes to highlight the current item.  The menu is always build as seen by the anonymous user.

-module(scomp_menu_menu).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zotonic.hrl").


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
% <ul id="navigation" class="at-menu">
% 	<li id="nav-item-1" class="first">
% 		<a href="" class="home-page current">home</a>
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
    Menu = get_menu(Context),
    Id = proplists:get_value(id, Params),
    CurrentId = find_id(Id, Menu),
    case z_depcache:get({menu, CurrentId, Context#context.language}, Context) of
        {ok, CachedMenu} ->
            {ok, CachedMenu};
        undefined ->
            {IdAcc, LIs} = build_menu(Menu, CurrentId, 1, [], [], z_acl:anondo(Context)),
            UL = ["<ul id=\"navigation\" class=\"clearfix at-menu do_superfish\">", LIs, "</ul>"],
            NewMenu = iolist_to_binary(UL),
            z_depcache:set({menu, CurrentId, Context#context.language}, NewMenu, ?DAY, [CurrentId, menu | IdAcc], Context),
            {ok, NewMenu}
    end.


build_menu([], _Id, _Nr, IdAcc, Acc, _Context) ->
    {IdAcc, lists:reverse(Acc)};
build_menu([{N,[]} | T], Id, Nr, IdAcc, Acc, Context) ->
    LI = menu_item(N, T, Id, Nr, Context),
    build_menu(T, Id, Nr+1, [N|IdAcc], [ [LI,"</li>"] | Acc ], Context);
build_menu([{N,SubMenu} | T], Id, Nr, IdAcc, Acc, Context) ->
    LI = menu_item(N, T, Id, Nr, Context),
    {IdAcc1, SubLIs} = build_menu(SubMenu, Id, 1, IdAcc, [], Context),
    build_menu(T, Id, Nr+1, [N|IdAcc1], [ [LI,"<ul>",SubLIs,"</ul></li>"] | Acc ], Context);
build_menu([N | T], Id, Nr, IdAcc, Acc, Context) when is_integer(N) ->
    LI = menu_item(N, T, Id, Nr, Context),
    build_menu(T, Id, Nr+1, [N|IdAcc], [ [LI,"</li>"] | Acc ], Context).


%% @doc Check if the id is in the menu. Return undefined when not found, otherwise the Id.
find_id(undefined, _Menu) ->
    undefined;
find_id(_Id, []) ->
    undefined;
find_id(Id, [{Id,_}|_]) ->
    Id;
find_id(Id, [Id|_]) ->
    Id;
find_id(Id, [{_,S}|T]) ->
    case find_id(Id, S) of
        undefined ->
            find_id(Id, T);
        Id ->
            Id
    end;
find_id(Id, [_|T]) ->
    find_id(Id, T).


menu_item(N, T, Id, Nr, Context) ->
    case m_rsc:exists(N, Context) andalso m_rsc:is_visible(N, Context) of
        true ->
            First = case Nr of 1 -> " first "; _ -> [] end,
            Last  = case T of [] -> " last "; _ -> [] end,
            Current = case N == Id of true -> " current "; _ -> [] end,
            [
                "<li id=\"nav-item-", integer_to_list(Nr), "\" class=\"",First,Last,"\">",
                    "<a href=\"", m_rsc:p(N, page_url, Context), "\" class=\"", Current, z_convert:to_binary(m_rsc:p(N, name, Context)), "\">",
                        get_title(N, Context),
                "</a>"
            ];
        false ->
            []
    end.

get_title(Id, Context) ->
	case ?TR(m_rsc:p(Id, short_title, Context), Context) of
		N when N == [] orelse N == <<"">> orelse N == undefined -> ?TR(m_rsc:p(Id, title, Context), Context);
		Title -> Title
	end.

%% @doc Fetch the menu from the site configuration.
%% @spec get_menu(Context) -> list()
get_menu(Context) ->
    case m_config:get(menu, menu_default, Context) of
        undefined -> [];
        Props -> proplists:get_value(menu, Props, [])
    end.
