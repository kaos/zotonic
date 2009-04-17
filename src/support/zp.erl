%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-08
%%
%% @doc Some easy shortcut functions.

-module(zp).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    m/0,
    flush/0,
    restart/0
]).

-include_lib("zophrenic.hrl").

m() -> 
    make:all([load]), 
    flush().

flush() ->
    zp_depcache:flush(),
    zp_dispatcher:reload().

restart() ->
    zophrenic:stop(),
    zophrenic:start().

