%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Supervisor for the zophrenic application.

-module(zophrenic_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
	    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    % Listen to IP address and Port
    WebIp      = case os:getenv("ZP_IP")   of false -> "0.0.0.0"; Anyip -> Anyip end,   
    WebPort    = case os:getenv("ZP_PORT") of false -> 8000; Anyport -> Anyport end,   

    % Default connection for database
    DbHost     = case os:getenv("ZP_DBHOST")     of false -> "localhost"; AnyDbHost -> AnyDbHost end,
    DbPort     = case os:getenv("ZP_DBPORT")     of false -> 5432; AnyDbPort -> list_to_integer(AnyDbPort) end,
    DbUser     = case os:getenv("ZP_DBUSER")     of false -> "zophrenic"; AnyDbUser -> AnyDbUser end,
    DbPassword = case os:getenv("ZP_DBPASSWORD") of false -> ""; AnyDbPassword -> AnyDbPassword end,
    DbDatabase = case os:getenv("ZP_DB")         of false -> "zophrenic"; AnyDbDatabase -> AnyDbDatabase end,

    WebConfig = [
		 {ip, WebIp},
		 {port, WebPort},
		 {error_handler, zp_webmachine_error_handler},
         {log_dir, "priv/log"},
		 {dispatch, []}
	],

    MochiWeb = {webmachine_mochiweb,
	            {webmachine_mochiweb, start, [WebConfig]}, 
	            permanent, 5000, worker, dynamic},

    DbPoolConfig = [
        {dbdefault, 10, [{host, DbHost}, {port, DbPort}, {user, DbUser}, {password, DbPassword}, {database, DbDatabase}]}
    ],
    
    Postgres = {epgsql_pool,
                {epgsql_pool, start_link, [DbPoolConfig]},
                permanent, 5000, worker, dynamic},
                
    Depcache = {zp_depcache,
                {zp_depcache, start_link, []}, 
                permanent, 5000, worker, dynamic},

    Dispatcher = {zp_dispatcher,
	            {zp_dispatcher, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Notifier = {zp_notifier,
	            {zp_notifier, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Session = {zp_session_manager,
	            {zp_session_manager, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Person = {zp_person_manager,
	            {zp_person_manager, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Ids     = {zp_ids,
	            {zp_ids, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Template = {zp_template,
	            {zp_template, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Scomp = {zp_scomp,
	            {zp_scomp, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    DropBox = {zp_dropbox,
                {zp_dropbox, start_link, []}, 
                permanent, 5000, worker, dynamic},

    Processes = [MochiWeb, Postgres, Depcache, Ids, Dispatcher, Notifier, Session, Person, Template, Scomp, DropBox],
    {ok, {{one_for_one, 1000, 10}, Processes}}.

