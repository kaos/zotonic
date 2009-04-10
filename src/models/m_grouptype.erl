%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-10
%%
%% @doc 

-module(m_grouptype).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    get/2,
    get_all/1,
    default/1,
    insert/2,
    delete/3,
    update/3
]).

-include_lib("zophrenic.hrl").

%% @doc Get the group
%% @spec get(Id, Context) -> PropList
get(Id, Context) ->
    zp_db:select(grouptype, Id, Context).

%% @doc Get all groups
%% @spec get_all(Context) -> [PropList]
get_all(Context) ->
    zp_db:assoc_props("select * from grouptype order by name", Context).


%% @doc Return the default grouptype id
%% @spec default(Context) -> int()
default(Context) ->
    [{Id}] = zp_db:q("select id from grouptype where name = $1", [general], Context),
    Id.


%% @doc Insert a new grouptype, only the admin can do this
%% @spec insert(Props, Context) -> Id
insert(Props, Context) ->
    {ok, Id} = zp_db:insert(grouptype, Props, Context),
    Id.

%% @doc Delete a grouptype, move all groups with this type to another type
%% @spec delete(TypeId, NewType, Context) -> void()
delete(Id, NewId, Context) ->
    ?ASSERT(Id /= NewId, illegal_id),
    F = fun(Ctx) ->
        zp_db:q("update \"group\" set grouptype_id = $1 where grouptype_id = $2", [NewId, Id], Ctx),
        zp_db:delete(grouptype, Id, Ctx)
    end,
    zp_db:transaction(F, Context).


update(Id, Props, Context) ->
    zp_db:update(grouptype, Id, Props, Context).

