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
    Ip   = case os:getenv("WEBMACHINE_IP")   of false -> "0.0.0.0"; Anyip -> Anyip end,   
    Port = case os:getenv("WEBMACHINE_PORT") of false -> 8000; Anyport -> Anyport end,   

    WebConfig = [
		 {ip, Ip},
		 {port, Port},
		 {error_handler, zp_webmachine_error_handler},
         {log_dir, "priv/log"},
		 {dispatch, []}],

    MochiWeb = {webmachine_mochiweb,
	            {webmachine_mochiweb, start, [WebConfig]}, 
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

    Processes = [MochiWeb, Depcache, Ids, Dispatcher, Notifier, Session, Person, Template, Scomp, DropBox],
    {ok, {{one_for_one, 1000, 10}, Processes}}.

