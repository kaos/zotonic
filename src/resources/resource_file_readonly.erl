%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Serve static files from a configured list of directories.  Caches files in the local depcache.
%% Is also able to generate previews (if configured to do so).
%% @todo gzip files before caching them in the depcache, serve gzip'ed file when requested
%%
%% Serves files like:
%% 
%% /lib/<filepath>
%% /image/2007/03/31/wedding.jpg(300x300)(crop-center)(709a-a3ab6605e5c8ce801ac77eb76289ac12).jpg
%% /media/inline/<filepath>
%% /media/attachment/<filepath>


-module(resource_file_readonly).
-export([init/1]).
-export([allowed_methods/2,
	 resource_exists/2,
	 last_modified/2,
	 expires/2,
	 content_types_provided/2,
	 charsets_provided/2,
	 encodings_provided/2,
	 provide_content/2,
	 finish_request/2
	 ]).

%-include_lib("kernel/include/file.hrl").
-include_lib("webmachine_resource.hrl").
-include_lib("zophrenic.hrl").

%% These are used for file serving (move to metadata)
-record(state, {
        root=undefined,
        media_path=undefined,
        is_media_preview=false,
        content_disposition=undefined,
        use_cache=false,
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

-define(MAX_AGE, 315360000).
-define(MEDIA_PATH,   "priv/files/archive/").


init(ConfigProps) ->
    UseCache       = proplists:get_value(use_cache, ConfigProps, false),
    {root, Root}   = proplists:lookup(root, ConfigProps),
    IsMediaPreview = proplists:get_value(is_media_preview, ConfigProps, false),
    MediaPath      = proplists:get_value(media_path, ConfigProps, ?MEDIA_PATH),
    ContentDisposition = proplists:get_value(content_disposition, ConfigProps),
    {ok, #state{root=Root, use_cache=UseCache, is_media_preview=IsMediaPreview, media_path=MediaPath, content_disposition=ContentDisposition}}.
    
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

encodings_provided(_ReqProps, State) ->
    Encodings = case State#state.use_cache of
                    true ->
                        case State#state.mime of
                            "image/jpeg" -> 
                                    [{"identity", fun(Data) -> Data end}];
                            _ -> 
                                    [{"identity", fun(Data) -> Data end},
                                     {"gzip",     fun(Data) -> Data end}]
                        end;
                    false ->
                        [{"identity", fun(Data) -> Data end}]
                end,
    {Encodings,State}.


resource_exists(ReqProps, State) ->
    Path   = ?PATH(ReqProps),
    Cached = case State#state.use_cache of
                true -> zp_depcache:get(cache_key(Path));
                _    -> undefined
            end,
    case Cached of
        undefined ->
            case file_exists(State, Path) of 
            	{true, FullPath} -> 
            	    {true, State#state{path=Path, fullpath=FullPath}};
            	_ -> 
            	    %% We might be able to generate a new preview
            	    case State#state.is_media_preview of
            	        true ->
            	            % Generate a preview, recurse on success
            	            ensure_preview(ReqProps, Path, State);
            	        false ->
                    	    {false, State}
            	    end
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
    
last_modified(ReqProps, State) ->
    Req = ?REQ(ReqProps),
    Req:add_response_header("Cache-Control", "public, max-age="++integer_to_list(?MAX_AGE)),
    case State#state.last_modified of
        undefined -> 
            LMod = filelib:last_modified(State#state.fullpath),
            {LMod, State#state{last_modified=LMod}};
        LMod ->
            {LMod, State}
    end.

expires(_ReqProps, State) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + ?MAX_AGE), State}.

provide_content(ReqProps, State) ->
    Req = ?REQ(ReqProps),
    case State#state.content_disposition of
        inline ->     Req:add_response_header("Content-Disposition", "inline");
        attachment -> Req:add_response_header("Content-Disposition", "attachment");
        undefined ->  ok
    end,
    {Choices, State1} = case State#state.body of
                            undefined ->
                                {ok, Data} = file:read_file(State#state.fullpath),
                                Body = case State#state.use_cache of
                                          true  -> [{"identity",Data}, {"gzip",zlib:gzip(Data)}];
                                          false -> [{"identity",Data}]
                                       end,
                                {Body, State#state{body=Body}};
                            Body -> 
                                {Body, State}
                        end,
    CE = Req:get_metadata('content-encoding'),
    {proplists:get_value(CE, Choices), State1}.

finish_request(_ReqProps, State) ->
    case State#state.is_cached of
        false ->
            case State#state.body of
                undefined ->  
                    {ok, State};
                _ ->
                    case State#state.use_cache of
                        true ->
                            % Cache the served file in the depcache.  Cache it for 3600 secs.
                            Cache = #cache{
                                        path=State#state.path,
                                        fullpath=State#state.fullpath,
                                        mime=State#state.mime,
                                        last_modified=State#state.last_modified,
                                        body=State#state.body
                                    },
                            zp_depcache:set(cache_key(State#state.path), Cache),
                            {ok, State};
                        _ ->
                            {ok, State}
                    end
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


%% @spec ensure_preview(ReqProps, Path, State) -> {Boolean, State}
%% @doc Generate the file on the path from an archived media file.
%% The path is like: 2007/03/31/wedding.jpg(300x300)(crop-center)(709a-a3ab6605e5c8ce801ac77eb76289ac12).jpg
%% The original media should be in State#media_path
%% The generated image should be created in State#root
ensure_preview(ReqProps, Path, State) ->
    {Filepath, PreviewPropList, _Checksum, _ChecksumBaseString} = zp_media_tag:url2props(Path),
    case mochiweb_util:safe_relative_path(Filepath) of
        undefined ->
            {false, State};
        Safepath  ->
            MediaFile = filename:join(State#state.media_path, Safepath),
            case filelib:is_regular(MediaFile) of
                true ->
                    % Media file exists, perform the resize
                    % @todo make use of a resize server, so that we do not resize too many files at the same time.
                    [Root|_] = State#state.root,
                    PreviewFile = filename:join(Root,Path),
                    case zp_media_preview:convert(MediaFile, PreviewFile, PreviewPropList) of
                        ok -> resource_exists(ReqProps,State);
                        {error, Reason} -> throw(Reason)
                    end;
                false ->
                    {false, State}
            end
    end.


