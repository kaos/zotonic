%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-10
%%
%% @doc DB model group interface

-module(m_group).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    get/2,
    insert/2,
    add_member/3,
    add_observer/3,
    add_leader/3,
    delete_member/2,
    delete_member/3,
    groups_member/1,
    groups_member/2,
    groups_observer/1,
    groups_observer/2,
    groups_visible/1,
    groups_visible/2,
    groups_leader/1,
    groups_leader/2,
    roles/1,
    roles/2
]).

-include_lib("zophrenic.hrl").


%% @doc Get the group
%% @spec get(Id, Context) -> PropList
get(Id, Context) ->
    case zp_depcache:get({group, Id}) of
        {ok, Group} -> Group;
        undefined ->
            Group = zp_db:assoc_props_row("select * from \"group\" where id = $1", [Id], Context),
            zp_depcache:set({group, Id}, Group, ?WEEK),
            Group
    end.


%% @doc Insert a new group, make sure that the roles are only set by the admin
%% @spec insert(PropList, Context) -> int()
insert(Props, Context) ->
    PropsSafe = case zp_access_control:has_role(admin, Context) of
        true -> Props;
        false ->
            zp_utils:prop_delete(is_admin, 
                zp_utils:prop_delete(is_editor, 
                    zp_utils:prop_delete(is_supervisor, 
                        zp_utils:prop_delete(is_community_publisher, 
                            zp_utils:prop_delete(is_public_publisher, Props)))))
    end,
    PropsType = case proplists:get_value(grouptype_id, PropsSafe) of
        undefined -> [ {grouptype_id, m_grouptype:default(Context)} | PropsSafe];
        _ -> PropsSafe
    end,
    {ok, Id} = zp_db:insert(group, PropsType, Context),
    Id.



add_member(Id, RscId, Context) ->
    F = fun(Ctx) ->
        case zp_db:q1("select id from rsc_group where group_id = $1 and rsc_id = $2", [Id, RscId], Ctx) of
            undefined ->
                {ok, Id} = zp_db:insert(rsc_group, [{group_id, Id},{rsc_id, RscId}], Ctx),
                Id;
            RscGroupId ->
                zp_db:q("update rsc_group set is_observer = false, is_leader = false where id = $1", [RscGroupId], Ctx),
                RscGroupId
        end
    end,
    zp_db:transaction(F, Context).


add_observer(Id, RscId, Context) ->
    F = fun(Ctx) ->
        case zp_db:q1("select id from rsc_group where group_id = $1 and rsc_id = $2", [Id, RscId], Ctx) of
            undefined ->
                {ok, Id} = zp_db:insert(rsc_group, [{group_id, Id},{rsc_id, RscId},{is_observer, true}], Ctx),
                Id;
            RscGroupId ->
                zp_db:q("update rsc_group set is_observer = true, is_leader = false where id = $1", [RscGroupId], Ctx),
                RscGroupId
        end
    end,
    zp_db:transaction(F, Context).


add_leader(Id, RscId, Context) ->
    F = fun(Ctx) ->
        case zp_db:q1("select id from rsc_group where group_id = $1 and rsc_id = $2", [Id, RscId], Ctx) of
            undefined ->
                {ok, Id} = zp_db:insert(rsc_group, [{group_id, Id},{rsc_id, RscId},{is_leader, true}], Ctx),
                Id;
            RscGroupId ->
                zp_db:q("update rsc_group set is_observer = false, is_leader = true where id = $1", [RscGroupId], Ctx),
                RscGroupId
        end
    end,
    zp_db:transaction(F, Context).


delete_member(RscGroupId, Context) ->
    zp_db:delete(rsc_group, RscGroupId, Context).

delete_member(Id, RscId, Context) ->
    zp_db:q("delete from rsc_group where group_id = $1 and rsc_id = $2",[Id, RscId], Context).
    
    
%% @doc Return the group ids the current person is member of
%% @spec person_groups(Context) -> List
groups_member(Context) ->
    groups_member(zp_access_control:person(Context), Context).

%% @doc Return the group ids the person is member of
%% @spec person_groups(PersonId, Context) -> List
groups_member(PersonId, Context) ->
    Groups = zp_db:q("select group_id from rsc_group where rsc_id = $1 and is_observer = false", [PersonId], Context),
    [ G || {G} <- Groups ].

%% @doc Return the group ids the current person can only view
%% @spec person_groups(Context) -> List
groups_observer(Context) ->
    groups_observer(zp_access_control:person(Context), Context).

%% @doc Return the group ids the person can only view
%% @spec person_groups(PersonId, Context) -> List
groups_observer(PersonId, Context) ->
    Groups = zp_db:q("select group_id from rsc_group where rsc_id = $1 and is_observer = true", [PersonId], Context),
    [ G || {G} <- Groups ].


%% @doc Return the group ids the current person can view
%% @spec person_groups(Context) -> List
groups_visible(Context) ->
    groups_visible(zp_access_control:person(Context), Context).

%% @doc Return the group ids the person can view
%% @spec person_groups(PersonId, Context) -> List
groups_visible(PersonId, Context) ->
    Groups = zp_db:q("select group_id from rsc_group where rsc_id = $1", [PersonId], Context),
    [ G || {G} <- Groups ].

%% @doc Return the group ids the current person is leader of
%% @spec person_groups(Context) -> List
groups_leader(Context) ->
    groups_leader(zp_access_control:person(Context), Context).

%% @doc Return the group ids the person leads
%% @spec person_groups(PersonId, Context) -> List
groups_leader(PersonId, Context) ->
    Groups = zp_db:q("select group_id from rsc_group where rsc_id = $1 and is_leader = true", [PersonId], Context),
    [ G || {G} <- Groups ].


%% @doc Return the roles the current person has
%% @spec roles(Context) -> [atom(), ..]
roles(Context) ->
    roles(zp_access_control:person(Context), Context).

%% @doc Return the roles the person has
%% @spec roles(Context) -> [atom(), ..]
roles(PersonId, Context) ->
    Roles = case zp_db:q("
        select  max(g.is_admin::int), max(g.is_editor::int), max(g.is_supervisor::int), 
                max(g.is_community_publisher::int), max(g.is_public_publisher::int)
        from \"group\" g join rsc_group rg on rg.group_id = g.id
        where rg.rsc_id = $1
          and rg.is_observer = false", [PersonId], Context) of
          [Row] -> Row;
          [] -> {0, 0, 0, 0, 0}
    end,
    P = lists:zip([admin, editor, supervisor, community_publisher, public_publisher], tuple_to_list(Roles)),
    [ R || {R,M} <- P, M =/= 0 ].
