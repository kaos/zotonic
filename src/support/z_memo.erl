%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-11-05
%%
%% @doc Simple memo functions.  Stores much used values in the process dictionary. Especially useful for
%% ACL lookups.

-module(z_memo).
-author("Marc Worrell <marc@worrell.nl>").

-export([
	enable/0,
	disable/0,
	is_enabled/1,
	set_userid/1,
	set/2,
	set/3,
	get/1,
	get/2
]).

-include_lib("include/zotonic.hrl").

%% @doc Enable memoization for this process. You need to call set_userid/1 before memoization is effective.
enable() ->
	erlang:put(is_memo, true).

%% @doc Disable memoization for this process.
disable() ->
	erlang:erase(is_memo),
	erlang:erase(memo_userid).

%% @doc Set the user id for which we memo values.  Called by z_auth on session initialization.
set_userid(AuthUserId) ->
	case erlang:get(is_memo) of
		true -> erlang:put(memo_userid, {ok, AuthUserId});
		_ -> error
	end.

%% @doc Check if memoization is enabled for the current user/process.
is_enabled(#context{user_id=UserId}) ->
	case erlang:get(memo_userid) of
		{ok, UserId} -> true;
		_ -> false
	end.

%% @doc Check if the key is stored.
get(Key) ->
	erlang:get(Key).

get(Key, Context) ->
	case is_enabled(Context) of
		true -> erlang:get(Key);
		false -> undefined
	end.

%% @doc Store a key if memoization is set.
set(Key, Value) ->
	erlang:put(Key, Value).

set(Key, Value, Context) ->
	case is_enabled(Context) of
		true -> erlang:put(Key, Value);
		false -> error
	end.




