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

-include_lib("zophrenic.hrl").

n(Msg, Context) ->
    zp_notifier:notify(Msg, Context).

n1(Msg, Context) ->
    zp_notifier:notify1(Msg, Context).
    
m() -> 
    make:all([load]), 
    flush().

flush() ->
    C = zp_context:new(),
    zp_depcache:flush(),
    zp_dispatcher:reload(C),
    n({module_ready}, C).

restart() ->
    zophrenic:stop(),
    zophrenic:start().

