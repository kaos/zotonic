%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-10
%%
%% @doc Access Control for zophrenic. Defines what a person can do on the site.

-module(zp_access_control).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    default/2,
    can_see/1,
    has_role/2,
    publish_level/1,
    publish_level/2,
    person/1,
    add_defaults/2,
    add_person/3
]).

-include_lib("zophrenic.hrl").

default(group, Context) -> 
    1;
default(visible_for, Context) ->
    publish_level(Context).

%% @doc Maximum visible_for value for content that is not in the user's groups
can_see(Context) -> 
    ?ACL_VIS_GROUP.

has_role(admin, Context) ->
    true;
has_role(supervisor, Context) ->
    true;
has_role(editor, Context) ->
    true;
has_role(community_publisher, Context) ->
    true;
has_role(public_publisher, Context) ->
    true.
    
publish_level(Context) ->
    publish_level(0, Context).
publish_level(Requested, Context) ->
    Max = case has_role(public_publisher, Context) of
        true -> 
            ?ACL_VIS_PUBLIC;
        false ->
            case has_role(community_publisher, Context) of
                true -> ?ACL_VIS_COMMUNITY;
                false -> ?ACL_VIS_GROUP  % group only
            end
    end,
    if 
        Max >  Requested -> Max;
        Max =< Requested -> Requested
    end.

person(Context) ->
    1.

add_defaults(PropList, Context) ->
    PropGroup = case proplists:get_value(group_id, PropList) of
        undefined -> [{group_id,default(group,Context)}|PropList];
        _ -> PropList
    end,
    PropVis = case proplists:get_value(visible_for, PropGroup) of
        undefined -> [{visible_for,publish_level(Context)}|PropGroup];
        Vis -> 
            Max = publish_level(Context),
            if 
                Max > Vis -> zp_utils:prop_replace(visible_for, Max, PropGroup);
                true -> PropGroup
            end
    end,
    PropVis.

add_person(Prop, PropList, Context) ->
    case proplists:get_value(Prop, PropList) of
        undefined -> [{Prop,person(Context)}|PropList];
        _ -> PropList
    end.
    
    