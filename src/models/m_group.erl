%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-10
%%
%% @doc DB model group interface

-module(m_group).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,

    name_to_id/2,
    name_to_id_check/2,
    
    get/2,
    insert/2,
    update_noflush/3,
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


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(member, #m{value=undefined}, Context) ->
    groups_member(Context);
m_find_value(leader, #m{value=undefined}, Context) ->
    groups_leader(Context);
m_find_value(observer, #m{value=undefined}, Context) ->
    groups_observer(Context);
m_find_value(Key, #m{value=undefined}, Context) when is_integer(Key) ->
    get(Key, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.


%% @doc Get the group
%% @spec get(Id, Context) -> PropList
get(Id, Context) ->
    F = fun() ->
        zp_db:assoc_row("select * from \"group\" where id = $1", [Id], Context)
    end,
    zp_depcache:memo(F, {group, Id}, ?WEEK, [#rsc{id=Id}]).


name_to_id(Name, Context) ->
    m_rsc:name_to_id_cat(Name, group, Context).

name_to_id_check(Name, Context) ->
    m_rsc:name_to_id_cat_check(Name, group, Context).
    

%% @doc Insert a new group.
%% @spec insert(Title, Context) -> {ok, int()} | {error, Reason}
insert(Title, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            Props = [
                {title, Title},
                {category, group},
                {group, admins},
                {is_published, true},
                {visible_for, 1}
            ],
            m_rsc:insert(Props, Context);
        false ->
            {error, eacces}
    end.


update_noflush(Id, Props, Context) ->
    case zp_acl:has_role(admin, Context) of
        true ->
            GroupProps = [
                {is_admin, proplists:get_value(group_is_admin, Props, false)},
                {is_supervisor, proplists:get_value(group_is_supervisor, Props, false)},
                {is_community_publisher, proplists:get_value(group_is_community_publisher, Props, false)},
                {is_public_publisher, proplists:get_value(group_is_public_publisher, Props, false)}
            ],
            case zp_db:update(group, Id, GroupProps, Context) of
                {ok, 1} ->
                    {ok, Id};
                {ok, 0} ->
                    zp_db:insert(group, [ {id, Id} | GroupProps ], Context)
            end;
        false ->
            {error, eacces}
    end.
    

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
    zp_db:transaction(F, Context),
    zp_depcache:flush({groups, RscId}).


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
    Result = zp_db:transaction(F, Context),
    zp_depcache:flush({groups, RscId}),
    Result.


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
    Result = zp_db:transaction(F, Context),
    zp_depcache:flush({groups, RscId}),
    Result.


delete_member(RscGroupId, Context) ->
    RscId = zp_db:q("select rsc_id from rsc_group where id = $1", [RscGroupId], Context),
    Result = zp_db:delete(rsc_group, RscGroupId, Context),
    zp_depcache:flush({groups, RscId}),
    Result.

delete_member(Id, RscId, Context) ->
    Result = zp_db:q("delete from rsc_group where group_id = $1 and rsc_id = $2",[Id, RscId], Context),
    zp_depcache:flush({groups, RscId}),
    Result.
    
    
%% @doc Return the group ids the current person is member of
%% @spec person_groups(Context) -> List
groups_member(Context) ->
    groups_member(zp_acl:user(Context), Context).


%% @doc Return the group ids the person is member or leader of
%% @spec person_groups(PersonId, Context) -> List
groups_member(PersonId, Context) ->
    F = fun() ->
        Groups = zp_db:q("select group_id from rsc_group where rsc_id = $1 and is_observer = false", [PersonId], Context),
        [ G || {G} <- Groups ]
    end,
    zp_depcache:memo(F, {groups_member, PersonId}, ?DAY, [{groups, PersonId}]).


%% @doc Return the group ids the current person can only view
%% @spec person_groups(Context) -> List
groups_observer(Context) ->
    groups_observer(zp_acl:user(Context), Context).


%% @doc Return the group ids the person can only view
%% @spec person_groups(PersonId, Context) -> List
groups_observer(PersonId, Context) ->
    F = fun() ->
        Groups = zp_db:q("select group_id from rsc_group where rsc_id = $1 and is_observer = true", [PersonId], Context),
        [ G || {G} <- Groups ]
    end,
    zp_depcache:memo(F, {groups_observer, PersonId}, ?DAY, [{groups, PersonId}]).


%% @doc Return the group ids the current person can view
%% @spec person_groups(Context) -> List
groups_visible(Context) ->
    groups_visible(zp_acl:user(Context), Context).

%% @doc Return the group ids the person can view
%% @spec person_groups(PersonId, Context) -> List
groups_visible(PersonId, Context) ->
    F = fun() ->
        Groups = zp_db:q("select group_id from rsc_group where rsc_id = $1", [PersonId], Context),
        [ G || {G} <- Groups ]
    end,
    zp_depcache:memo(F, {groups_visible, PersonId}, ?DAY, [{groups, PersonId}]).


%% @doc Return the group ids the current person is leader of
%% @spec person_groups(Context) -> List
groups_leader(Context) ->
    groups_leader(zp_acl:user(Context), Context).

%% @doc Return the group ids the person leads
%% @spec person_groups(PersonId, Context) -> List
groups_leader(PersonId, Context) ->
    F = fun() ->
        Groups = zp_db:q("select group_id from rsc_group where rsc_id = $1 and is_leader = true", [PersonId], Context),
        [ G || {G} <- Groups ]
    end,
    zp_depcache:memo(F, {groups_leader, PersonId}, ?DAY, [{groups, PersonId}]).


%% @doc Return the roles the current person has
%% @spec roles(Context) -> [atom(), ..]
roles(Context) ->
    roles(zp_acl:user(Context), Context).

%% @doc Return the roles the person has
%% @spec roles(Context) -> [atom(), ..]
roles(PersonId, Context) ->
    Roles = case zp_db:q("
        select  max(g.is_admin::int), max(g.is_supervisor::int), 
                max(g.is_community_publisher::int), max(g.is_public_publisher::int)
        from \"group\" g join rsc_group rg on rg.group_id = g.id
        where rg.rsc_id = $1
          and rg.is_observer = false", [PersonId], Context) of
          [Row] -> Row;
          [] -> {0, 0, 0, 0, 0}
    end,
    P = lists:zip([admin, supervisor, community_publisher, public_publisher], tuple_to_list(Roles)),
    [ R || {R,M} <- P, M =/= 0 ].
