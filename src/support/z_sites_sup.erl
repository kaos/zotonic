%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Supervisor for all sites running inside Zotonic.  Starts the sites according to the config files in the sites subdirectories.

-module(z_sites_sup).
-author('Marc Worrell <marc@worrell.nl>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0, update_dispatchinfo/0]).

%% supervisor callbacks
-export([init/1]).

-include_lib("zotonic.hrl").
-include_lib("wm_host_dispatch_list.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @doc Update the webmachine dispatch information. Collects dispatch information from all sites and sends it
%% to webmachine for updating its dispatch lists and host information.
update_dispatchinfo() ->
    Sites = [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)],
    DispatchList = [ fetch_dispatchinfo(Site) || Site <- Sites ],
    application:set_env(webmachine, dispatch_list, DispatchList).

    fetch_dispatchinfo(Site) ->
        Name = z_utils:name_for_host(z_dispatcher, Site),
        {Host, Hostname, Hostalias, DispatchList} = z_dispatcher:dispatchinfo(Name),
        WMList = [list_to_tuple(tl(tuple_to_list(Disp))) || Disp <- DispatchList],
        #wm_host_dispatch_list{host=Host, hostname=Hostname, hostalias=Hostalias, redirect=true, dispatch_list=WMList}.


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
    Sites = scan_sites(),
    {ok, {{one_for_one, 1000, 10}, sites_to_spec(Sites, [])}}.

    %% @doc Define a site supervisor for all enabled sites.
    sites_to_spec([], Acc) ->
        Acc;
    sites_to_spec([SiteProps|Rest], Acc) ->
        Enabled = proplists:get_value(enabled, SiteProps, false),
        case Enabled of
            true ->
                {host, Name} = proplists:lookup(host, SiteProps),
                Spec = {
                    Name,
                    {z_site_sup, start_link, [SiteProps]},
                    permanent, 5000, worker, dynamic
                },
                sites_to_spec(Rest, [Spec|Acc]);
            false ->
                sites_to_spec(Rest, Acc)
        end.


%% @doc Scan all sites subdirectories for the site configurations.
%% @spec scan_sites -> [ SiteProps ]
scan_sites() ->
    SitesDir = filename:join([code:lib_dir(zotonic, priv), "sites", "*", "config"]),
    Configs = [ parse_config(C) || C <- filelib:wildcard(SitesDir) ],
    [ C || C <- Configs, is_list(C) ].


    parse_config(C) ->
        case file:consult(C) of
            {ok, [H|_]} -> 
                H;
            {error, Reason} = Error ->
                ?ERROR("Could not consult site config ~p: error ~p", [C, Reason]),
                Error
        end.

