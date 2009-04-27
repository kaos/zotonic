%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-10
%%
%% @doc Access Control for zophrenic. Defines what a user can do on the site.
%% The access control is organized around people working together in groups. When you are
%% member of a group then you can see all content belonging to that group, regardless of publication state.
%% Other content must be published and is either visible to all or to authenticated users (community).


-module(zp_acl).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    rsc_visible/2,
    rsc_editable/2,
    rsc_ingroup/2,
    media_visible/2,
    media_editable/2,
    sudo/2,
    logon/2,
    logoff/1,
    default/2,
    can_see/1,
    groups_member/1,
    groups_observer/1,
    groups_leader/1,
    has_role/2,
    publish_level/1,
    publish_level/2,
    user/1,
    user_check/1,
    has_user/1,
    add_defaults/2,
    add_user/3
]).

-include_lib("zophrenic.hrl").

-record(acl, {
    is_admin = false,
    is_supervisor = false,
    is_community_publisher = false,
    is_public_publisher = false
}).


%% @doc Check if the resource is visible for the current user
rsc_visible(Id, #context{user_id=UserId}) when Id == UserId andalso is_integer(UserId) ->
    % Can always see myself
    true;
rsc_visible(Id, Context) ->
    acl_visible(m_rsc:get_acl_props(Id, Context), Context).


%% @doc Check if the resource is editable by the current user
rsc_editable(_Id, #context{user_id=undefined}) ->
    % Anonymous visitors can't edit anything
    false;
rsc_editable(Id, #context{user_id=UserId}) when Id == UserId andalso is_integer(UserId) ->
    % Can always edit myself
    true;
rsc_editable(Id, Context) ->
    acl_editable(m_rsc:get_acl_props(Id, Context), Context).

%% @doc Check if the group of the resource is in one of the groups of the user
rsc_ingroup(_Id, #context{user_id=undefined}) ->
    false;
rsc_ingroup(Id, Context) ->
    Gs = groups_member(Context),
    Props = m_rsc:get_acl_props(Id, Context),
    lists:member(Props#acl_props.group_id, Gs).


%% @doc Check if the media record is visible for the current user
media_visible(Id, Context) ->
    acl_visible(m_media:get_acl_props(Id, Context), Context).

%% @doc Check if the media record is editable by the current user
media_editable(_Id, #context{user_id=undefined}) ->
    % Anonymous visitors can't edit anything
    false;
media_editable(Id, Context) ->
    acl_editable(m_media:get_acl_props(Id, Context), Context).


%% @doc Call a function with admin privileges.
%% @spec sudo(FuncDef, #context) -> FuncResult
sudo({M,F}, Context) ->
    erlang:apply(M, F, [set_admin(Context)]);
sudo({M,F,A}, Context) ->
    ContextSu = set_admin(Context),
    erlang:apply(M, F, A ++ [ContextSu]);
sudo(F, Context) when is_function(F, 1) ->
    F(set_admin(Context)).

    set_admin(#context{acl=undefined} = Context) ->
        Context#context{acl=#acl{is_admin=true}, user_id=1};
    set_admin(Context) ->
        Acl  = Context#context.acl,
        Acl1 = Acl#acl{is_admin=true},
        Context#context{acl=Acl1}.


%% @doc Log the user with the id on, fill the acl field of the context
logon(Id, Context) ->
    Roles = m_group:roles(Id, Context),
    Acl = #acl{
        is_admin=lists:member(admin, Roles),
        is_supervisor=lists:member(supervisor, Roles),
        is_community_publisher=lists:member(community_publisher, Roles),
        is_public_publisher=lists:member(public_publisher, Roles)
    },
    Context#context{user_id=Id, acl=Acl}.


%% @doc Log off, reset the acl field of the context
logoff(Context) ->
    Context#context{user_id=undefined, acl=#acl{}}.


%% @doc Maximum visible_for value for content that is not in the user's groups
%% @spec can_see(#context) -> int()
can_see(#context{user_id=UserId, acl=Acl}) when is_integer(UserId) ->
    case Acl#acl.is_admin orelse Acl#acl.is_supervisor of
        true -> ?ACL_VIS_GROUP;
        false -> ?ACL_VIS_COMMUNITY
    end;
can_see(_) ->
    ?ACL_VIS_PUBLIC.


%% @doc Return the groups the current user is full member of
%% @spec groups(#context) -> list()
groups_member(#context{user_id=undefined}) ->
    [];
groups_member(#context{user_id=UserId} = Context) ->
    m_group:groups_member(UserId, Context).


%% @doc Return the groups the current user is full member or observer
%% @spec groups(#context) -> list()
groups_observer(#context{user_id=undefined}) ->
    [];
groups_observer(#context{user_id=UserId} = Context) ->
    m_group:groups_leader(UserId, Context).


%% @doc Return the groups the current user is full member or observer
%% @spec groups(#context) -> list()
groups_leader(#context{user_id=undefined}) ->
    [];
groups_leader(#context{user_id=UserId} = Context) ->
    m_group:groups_leader(UserId, Context).
    

has_role(admin, Context) ->
    case Context#context.acl of
        undefined -> false;
        Acl -> Acl#acl.is_admin
    end;
has_role(supervisor, Context) ->
    case Context#context.acl of
        undefined -> false;
        Acl -> Acl#acl.is_supervisor
    end;
has_role(community_publisher, Context) ->
    case Context#context.acl of
        undefined -> false;
        Acl -> Acl#acl.is_community_publisher orelse Acl#acl.is_admin
    end;
has_role(public_publisher, Context) ->
    case Context#context.acl of
        undefined -> false;
        Acl -> Acl#acl.is_public_publisher orelse Acl#acl.is_admin
    end.


%% @doc Maximum publish level that an user can give to his edited resources
%% @spec publish_level(#context) -> int()
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


%% @doc Return the current user id that has been authenticated.  This is the rsc id of a person record.
%% @spec user(#context) -> int() | undefined
user(Context) ->
    Context#context.user_id.


%% @doc Return the current user id, fail when no user logged on.
user_check(#context{user_id=UserId}) when UserId /= undefined ->
    UserId.


%% @doc Check if the current context has a user attached
%% @spec has_user(#context) -> bool()
has_user(#context{user_id=undefined}) ->
    false;
has_user(#context{user_id=UserId}) when is_integer(UserId) ->
    true.
    

%% @doc Return the default group for resource creation by the current user
default(group, Context) -> 
    1;
default(visible_for, Context) ->
    publish_level(Context).


%% @doc Add default ACL settings to a resource or media definition.  Adds missing group_id and visible_for.
add_defaults(PropList, Context) ->
    PropGroup = case proplists:get_value(group_id, PropList) of
        undefined -> 
            [{group_id,default(group,Context)}|PropList];
        _ ->
            %% @todo Check if group is a group of this user
            PropList
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


%% @doc Add the current user id as the prop, when the prop is not set.
%% @spec add_user(atom(), PropList1, #context) -> PropList2
add_user(Prop, PropList, Context) ->
    case proplists:get_value(Prop, PropList) of
        undefined ->
            [{Prop,user(Context)}|PropList];
        _ ->
            PropList
    end.


%% @doc Check if something with the given #acl fields is visible.
%% @spec acl_editable(#acl_props, #context) -> bool()
acl_visible(Acl, #context{user_id=undefined}) ->
    case Acl#acl_props.is_published of
        false -> 
            false;
        true ->
            case Acl#acl_props.visible_for == 0 of
                false ->
                    false;
                true ->
                    Date = calendar:universal_time(),
                    Acl#acl_props.publication_start =< Date andalso Acl#acl_props.publication_end >= Date
            end
    end;
acl_visible(Acl, Context) ->
    case has_role(admin, Context) orelse has_role(supervisor, Context) of
        true ->
            true;
        false ->
            Visible = case Acl#acl_props.visible_for of
                0 -> acl_is_published(Acl);
                1 -> acl_is_published(Acl);
                2 -> false
            end,

            case Visible of
                true ->
                    true;
                false ->
                    %% Must be in one of our groups
                    Gs = groups_observer(Context),
                    lists:member(Acl#acl_props.group_id, Gs)
            end
    end.

    acl_is_published(Acl) ->
        case Acl#acl_props.is_published of
            true ->
                Date = calendar:universal_time(),
                Acl#acl_props.publication_start =< Date andalso Acl#acl_props.publication_end >= Date;
            false ->
                false
        end.

%% @doc Check if something with the given #acl fields is editable.
%% @spec acl_editable(#acl_props, #context) -> bool()
acl_editable(Acl, Context) ->
    case has_role(admin, Context) of
        true ->
            true;
        false ->
            % Must be in one of our groups
            Gs = groups_member(Context),
            lists:member(Acl#acl_props.group_id, Gs)
    end.
