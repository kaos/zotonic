%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-08
%%
%% @doc Some easy shortcut functions.

-module(zp).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    n/2,
    n1/2,
    m/0,
    flush/0,
    restart/0
]).

-include_lib("zotonic.hrl").

%% @doc Send a notification
n(Msg, Context) ->
    z_notifier:notify(Msg, Context).

%% @doc Send a notification to the first observer
n1(Msg, Context) ->
    z_notifier:notify1(Msg, Context).

%% @doc (Re)make all erlang source modules and reset the caches.
m() -> 
    make:all([load]), 
    flush().

%% @doc Reset all caches, reload the dispatch rules and rescan all modules.
flush() ->
    C = z_context:new(),
    z_depcache:flush(),
    z_dispatcher:reload(C),
    n({module_ready}, C).

%% @doc Full restart of Zophrenic
restart() ->
    zotonic:stop(),
    zotonic:start().

