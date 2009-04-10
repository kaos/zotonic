%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-10
%%
%% @doc 

-module(zp_access_control).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    default/2,
    person/1,
    add_defaults/2,
    add_person/3
]).


default(group, Context) -> 
    1;
default(visible_for, Context) ->
    0.

person(Context) ->
    1.

add_defaults(PropList, Context) ->
    PropGroup = case proplists:get_value(group_id, PropList) of
        undefined -> [{group_id,default(group,Context)}|PropList];
        _ -> PropList
    end,
    PropVis = case proplists:get_value(visible_for, PropGroup) of
        undefined -> [{visible_for,default(visible_for,Context)}|PropGroup];
        _ -> PropGroup
    end,
    PropVis.

add_person(Prop, PropList, Context) ->
    case proplists:get_value(Prop, PropList) of
        undefined -> [{Prop,person(Context)}|PropList];
        _ -> PropList
    end.
    
    