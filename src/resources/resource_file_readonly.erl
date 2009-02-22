%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Serve static files from a configured list of directories.  Caches files in the local depcache.
%% @todo gzip files before caching them in the depcache, serve gzip'ed file when requested

-module(resource_file_readonly).
-export([init/1]).
-export([allowed_methods/2,
	 resource_exists/2,
	 last_modified/2,
	 expires/2,
	 content_types_provided/2,
	 charsets_provided/2,
	 provide_content/2,
	 finish_request/2
	 ]).

%-include_lib("kernel/include/file.hrl").
-include_lib("webmachine_resource.hrl").
-include_lib("zophrenic.hrl").

%% These are used for file serving (move to metadata)
-record(state, {
        root=undefined,
        fullpath=undefined,
        is_cached=false,
        path=undefined,
        mime=undefined,
        last_modified=undefined,
        body=undefined
    }).

-record(cache, {
    path=undefined,
    fullpath=undefined,
    mime=undefined,
    last_modified=undefined,
    body=undefined
    }).

init(ConfigProps) ->
    {root, Root} = proplists:lookup(root, ConfigProps),
    {ok, #state{root=Root}}.
    
allowed_methods(_ReqProps, State) ->
    {['HEAD', 'GET'], State}.

content_types_provided(ReqProps, State) ->
    case State#state.mime of
        undefined ->
            Path = ?PATH(ReqProps),
            CT = webmachine_util:guess_mime(Path),
            {[{CT, provide_content}], State#state{mime=CT}};
        Mime -> 
            {[{Mime, provide_content}], State}
    end.

resource_exists(ReqProps, State) ->
    Path = ?PATH(ReqProps),
    case zp_depcache:get(cache_key(Path)) of
        undefined ->
            case file_exists(State, Path) of 
            	{true, FullPath} -> 
            	    {true, State#state{path=Path, fullpath=FullPath}};
            	_ -> 
            	    {false, State}
            end;
        {ok, Cache} ->
            {true, State#state{
                            is_cached=true,
                            path=Cache#cache.path,
                            fullpath=Cache#cache.fullpath,
                            mime=Cache#cache.mime,
                            last_modified=Cache#cache.last_modified,
                            body=Cache#cache.body
                        }}
    end.

charsets_provided(_ReqProps, State) ->
    case is_text(State#state.mime) of
        true -> {[{"utf-8", fun(X) -> X end}], State};
        _ -> {no_charset, State}
    end.
    
last_modified(_ReqProps, State) ->
    case State#state.last_modified of
        undefined -> 
            LMod = filelib:last_modified(State#state.fullpath),
            {LMod, State#state{last_modified=LMod}};
        LMod ->
            {LMod, State}
    end.

expires(_ReqProps, State) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    OneWeek = 7*24*60*60,
    {calendar:gregorian_seconds_to_datetime(NowSecs + OneWeek), State}.

provide_content(_ReqProps, State) ->
    case State#state.body of
        undefined ->
            {ok, Body} = file:read_file(State#state.fullpath),
            {Body, State#state{body=Body}};
        Body -> 
            {Body, State}
    end.

finish_request(_ReqProps, State) ->
    case State#state.is_cached of
        false ->
            case State#state.body of
                undefined -> 
                    {ok, State};
                _ ->
                    % Cache the served file in the depcache.  Cache it for 3600 secs.
                    Cache = #cache{
                                path=State#state.path,
                                fullpath=State#state.fullpath,
                                mime=State#state.mime,
                                last_modified=State#state.last_modified,
                                body=State#state.body
                            },
                    zp_depcache:set(cache_key(State#state.path), Cache),
                    {ok, State}
            end;
        true ->
            {ok, State}
    end.


%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%
    
cache_key(Path) ->
    {resource_file,Path}.

file_paths(State, Name) ->
    RelName = case hd(Name) of
        "/" -> tl(Name);
        _ -> Name
    end,
    lists:map(fun(R) -> filename:join([R,RelName]) end, State#state.root).

file_exists(_State, []) ->
    false;
file_exists(State, Name) ->
    RelName = case hd(Name) of
        "/" -> tl(Name);
        _ -> Name
    end,
    case mochiweb_util:safe_relative_path(RelName) of
        undefined -> false;
        SafePath ->
            NamePaths = file_paths(State, SafePath),
            file_exists1(NamePaths)
    end.

file_exists1([]) ->
    false;
file_exists1([NamePath|T]) ->
    case filelib:is_regular(NamePath) of 
	true ->
	    {true, NamePath};
	false ->
	    file_exists1(T)
    end.


%% @spec is_text(Mime) -> bool()
%% @doc Check if a mime type is textual
is_text("text/" ++ _) -> true;
is_text("application/x-javascript") -> true;
is_text("application/xhtml+xml") -> true;
is_text("application/xml") -> true;
is_text(_Mime) -> false.


