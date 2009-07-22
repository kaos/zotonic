%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Supervisor for the zotonic application.

-module(zotonic_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
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
    WebIp      = case os:getenv("ZOTONIC_IP")   of false -> any; Anyip -> Anyip end,   
    WebPort    = case os:getenv("ZOTONIC_PORT") of false -> 8000; Anyport -> list_to_integer(Anyport) end,   

    % Default connection for database
    DbHost     = case os:getenv("ZOTONIC_DBHOST")     of false -> "localhost"; AnyDbHost -> AnyDbHost end,
    DbPort     = case os:getenv("ZOTONIC_DBPORT")     of false -> 5432; AnyDbPort -> list_to_integer(AnyDbPort) end,
    DbUser     = case os:getenv("ZOTONIC_DBUSER")     of false -> "zotonic"; AnyDbUser -> AnyDbUser end,
    DbPassword = case os:getenv("ZOTONIC_DBPASSWORD") of false -> ""; AnyDbPassword -> AnyDbPassword end,
    DbDatabase = case os:getenv("ZOTONIC_DB")         of false -> "zotonic"; AnyDbDatabase -> AnyDbDatabase end,

    WebConfig = [
		 {ip, WebIp},
		 {port, WebPort},
		 {error_handler, z_webmachine_error_handler},
         {log_dir, filename:join([code:lib_dir(zotonic, priv), "log"])},
		 {dispatch, []},
		 {backlog, 500}
	],

    MochiWeb = {webmachine_mochiweb,
	            {webmachine_mochiweb, start, [WebConfig]}, 
	            permanent, 5000, worker, dynamic},

    DbPoolConfig = [
        {default, 10, [{host, DbHost}, {port, DbPort}, {username, DbUser}, {password, DbPassword}, {database, DbDatabase}]}
    ],
    

    Ids     = {z_ids,
	            {z_ids, start_link, []}, 
	            permanent, 5000, worker, dynamic},
    
    Postgres = {epgsql_pool,
                {epgsql_pool, start_link, [DbPoolConfig]},
                permanent, 5000, worker, dynamic},
                
    Depcache = {z_depcache,
                {z_depcache, start_link, []}, 
                permanent, 5000, worker, dynamic},

    Notifier = {z_notifier,
	            {z_notifier, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Installer = {z_installer,
                {z_installer, start_link, [DbPoolConfig]},
                permanent, 1, worker, dynamic},

    Session = {z_session_manager,
	            {z_session_manager, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Visitor = {z_visitor_manager,
	            {z_visitor_manager, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Dispatcher = {z_dispatcher,
	            {z_dispatcher, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Template = {z_template,
	            {z_template, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    Scomp = {z_scomp,
	            {z_scomp, start_link, []}, 
	            permanent, 5000, worker, dynamic},

    DropBox = {z_dropbox,
                {z_dropbox, start_link, []}, 
                permanent, 5000, worker, dynamic},

    Pivot = {z_pivot_rsc,
                {z_pivot_rsc, start_link, []}, 
                permanent, 5000, worker, dynamic},

    PreviewServer = {z_media_preview_server,
                {z_media_preview_server, start_link, []}, 
                permanent, 5000, worker, dynamic},

    ModuleIndexer = {z_module_indexer,
                {z_module_indexer, start_link, []},
                permanent, 5000, worker, dynamic},

    Modules = {z_module_sup,
                {z_module_sup, start_link, []},
                permanent, 5000, worker, dynamic},

    Processes = [
            MochiWeb, Ids, Postgres, Depcache, Notifier, Installer, Session, Visitor, 
            Dispatcher, Template, Scomp, DropBox, Pivot, PreviewServer,
            ModuleIndexer, Modules
    ],
    {ok, {{one_for_one, 1000, 10}, Processes}}.

