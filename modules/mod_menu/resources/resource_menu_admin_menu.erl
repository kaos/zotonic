%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Show the current menu, enables editing the menu.

-module(resource_menu_admin_menu).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    is_authorized/2,
    event/2
]).

-include_lib("resource_html.hrl").

is_authorized(ReqData, Context) ->
    zp_auth:wm_is_authorized(ReqData, Context).


html(Context) ->
	Html = zp_template:render("admin_menu.tpl", [{page_admin_menu, true}, {menu, get_menu(Context)}], Context),
	zp_context:output(Html, Context).

event({drop, {dragdrop, DragTag, _, _DragEltId}, {dragdrop, DropTag, _, _DropEltId}}, Context) ->
    Menu  = get_menu(Context),
    Menu1 = handle_drop(Menu, DragTag, DropTag, Context),
    save_menu(Menu1, Context),
    Html = zp_template:render("_admin_menu_menu_view.tpl", [{menu, Menu1}], Context),
    zp_render:update("menu-editor", Html, Context).

event({postback, {delete, Props}, _TriggerId, _TargetId}, Context) ->
    Id = proplists:get_value(id, Props),
    Menu = get_menu(Context),
    Menu1 = case proplists:get_value(item, Props) of
        [Nr] -> 
            remove_nth(Nr, Menu);
        [Nr,SubNr] ->
            SubMenu  = lists:nth(Nr, Menu),
            SubMenu1 = remove_nth(SubNr, SubMenu),
            set_nth(Nr, SubMenu1, Menu)
    end,
    save_menu(Menu1, Context),
    Html = zp_template:render("_admin_menu_menu_view.tpl", [{menu, Menu1}], Context),
    zp_render:update("menu-editor", Html, Context);

event(Event, Context) ->
    ?DEBUG(Event),
    Context.

%% @doc Fetch the menu from the site configuration.
%% @spec get_menu(Context) -> list()
get_menu(Context) ->
    case m_config:get(menu, menu_default, Context) of
        undefined -> [];
        Props -> proplists:get_value(menu, Props, [])
    end.

%% @doc Save the menu to the site configuration.
%% @spec save_menu(list(), Context) -> ok
save_menu(Menu, Context) ->
    m_config:set_prop(menu, menu_default, menu, Menu, Context).


%% @doc Handle the drop of an id on top of a menu item.
handle_drop(Menu, Id, "top", Context) ->
    [ {Id, []} | Menu ];
handle_drop(Menu, Id, [MenuId], Context) ->
    Menu;
handle_drop(Menu, Id, [MenuId, ItemId], Context) ->
    Menu.



remove_nth(Nr, List) ->
    remove_nth(Nr, List, []).

remove_nth(_Nr, [], Acc) ->
    lists:reverse(Acc);
remove_nth(1, [_H|T], Acc) ->
    lists:reverse(Acc, T);
remove_nth(N, [H|T], Acc) ->
    remove_nth(N-1, T, [H|Acc]).



set_nth(Nr, Value, List) ->
    set_nth(Nr, Value, List, []).

set_nth(_Nr, Value, [], Acc) ->
    lists:reverse(Acc);
set_nth(1, Value, [_H|T], Acc) ->
    lists:reverse([Value|Acc], T);
set_nth(N, Value, [H|T], Acc) ->
    set_nth(N-1, Value, T, [H|Acc]).

