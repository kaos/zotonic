%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-17
%%
%% @doc This server will install the database when started. It will always return ignore to the supervisor.
%% This server should be started after the database pool but before any database queries will be done.

-module(z_installer).
-author("Marc Worrell <marc@worrell.nl").

%% gen_server exports
-export([start_link/1]).

-include_lib("zotonic.hrl").

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Install zotonic on the databases in the PoolOpts, skips when already installed.
start_link(Opts) when is_list(Opts) ->
    [ install_check(Name, PoolOpts) || {Name, _Size, PoolOpts} <- Opts ],
    ignore.

install_check(Name, PoolOpts) ->
    % Check if the config table exists, if so then assume that all is ok
    Database = proplists:get_value(database, PoolOpts),
    {ok, C} = pgsql_pool:get_connection(Name),
    {ok, HasConfig} = pgsql:equery1(C, "
            select count(*) 
            from information_schema.tables 
            where table_catalog = $1 
              and table_name = 'config' 
              and table_type = 'BASE TABLE'", [Database]),
    pgsql_pool:return_connection(Name, C),

    case HasConfig of
        0 ->
            ?LOG("Installing database ~p@~p:~p ~p", [
                        proplists:get_value(user, PoolOpts),
                        proplists:get_value(host, PoolOpts),
                        proplists:get_value(port, PoolOpts),
                        Database
                        ]),
            z_install:install(Name);
        1 -> 
            ok
    end.

    