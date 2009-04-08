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
    install/0,
    install/1
]).


m() -> make:all([load]).

install() -> zp_install:install(dbdefault).
install(Db) -> zp_install:install(Db).

