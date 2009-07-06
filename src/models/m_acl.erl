%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-27
%%
%% @doc Template access for access control functions and state

-module(m_acl).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

-include_lib("zophrenic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(user, #m{value=undefined}, Context) ->
    zp_acl:user(Context);
m_find_value(is_admin, #m{value=undefined}, Context) ->
    zp_acl:has_role(admin, Context);
m_find_value(is_supervisor, #m{value=undefined}, Context) ->
    zp_acl:has_role(supervisor, Context);
m_find_value(is_community_publisher, #m{value=undefined}, Context) ->
    zp_acl:has_role(community_publisher, Context);
m_find_value(is_public_publisher, #m{value=undefined}, Context) ->
    zp_acl:has_role(public_publisher, Context);
m_find_value(observer, #m{value=undefined}, Context) ->
    zp_acl:groups_observer(Context);
m_find_value(member, #m{value=undefined}, Context) ->
    zp_acl:groups_member(Context);
m_find_value(leader, #m{value=undefined}, Context) ->
    zp_acl:groups_leader(Context);
m_find_value(_Key, #m{value=undefined}, _Context) ->
   undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(_, _Context) ->
    [].

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
    undefined.
