%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Serve static files from a configured list of directories.  Caches files in the local depcache.
%% Is also able to generate previews (if configured to do so).
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
-include_lib("zotonic.hrl").

%% These are used for file serving (move to metadata)
-record(state, {
        root=undefined,
        media_path=undefined,
        is_media_preview=false,
        content_disposition=undefined,
        use_cache=false,
        encode_data=false,
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
-define(MEDIA_PATH,   filename:join([code:lib_dir(zotonic, priv), "sites", "default", "files", "archive"])).


init(ConfigProps) ->
    UseCache       = proplists:get_value(use_cache, ConfigProps, false),
    {root, Root}   = proplists:lookup(root, ConfigProps),
    IsMediaPreview = proplists:get_value(is_media_preview, ConfigProps, false),
    MediaPath      = proplists:get_value(media_path, ConfigProps, ?MEDIA_PATH),
    ContentDisposition = proplists:get_value(content_disposition, ConfigProps),
    {ok, #state{root=Root, use_cache=UseCache, is_media_preview=IsMediaPreview, media_path=MediaPath, content_disposition=ContentDisposition}}.
    
allowed_methods(ReqData, State) ->
    {['HEAD', 'GET'], ReqData, State}.

content_types_provided(ReqData, State) ->
    case State#state.mime of
        undefined ->
            Path = wrq:disp_path(ReqData),
            CT = z_utils:guess_mime(Path),
            {[{CT, provide_content}], ReqData, State#state{mime=CT}};
        Mime -> 
            {[{Mime, provide_content}], ReqData, State}
    end.

encodings_provided(ReqData, State) ->
    Encodings = case State#state.mime of
        "image/jpeg" -> 
            [{"identity", fun(Data) -> Data end}];
        _ -> 
            [{"identity", fun(Data) -> decode_data(identity, Data) end},
             {"gzip",     fun(Data) -> decode_data(gzip, Data) end}]
    end,
    EncodeData = length(Encodings) > 1,
    {Encodings, ReqData, State#state{encode_data=EncodeData}}.


resource_exists(ReqData, State) ->
    Context = z_context:new(ReqData, ?MODULE),
    Path   = wrq:disp_path(ReqData),
    Cached = case State#state.use_cache of
        true -> z_depcache:get(cache_key(Path));
        _    -> undefined
    end,
    case Cached of
        undefined ->
            case file_exists(State, Path, Context) of 
            	{true, FullPath} -> 
            	    {true, ReqData, State#state{path=Path, fullpath=FullPath}};
            	_ -> 
            	    %% We might be able to generate a new preview
            	    case State#state.is_media_preview of
            	        true ->
            	            % Generate a preview, recurse on success
            	            ensure_preview(ReqData, Path, State);
            	        false ->
                    	    {false, ReqData, State}
            	    end
            end;
        {ok, Cache} ->
            {true, ReqData, State#state{
                            is_cached=true,
                            path=Cache#cache.path,
                            fullpath=Cache#cache.fullpath,
                            mime=Cache#cache.mime,
                            last_modified=Cache#cache.last_modified,
                            body=Cache#cache.body
                        }}
    end.

charsets_provided(ReqData, State) ->
    case is_text(State#state.mime) of
        true -> {[{"utf-8", fun(X) -> X end}], ReqData, State};
        _ -> {no_charset, ReqData, State}
    end.
    
last_modified(ReqData, State) ->
    RD1 = wrq:set_resp_header("Cache-Control", "public, max-age="++integer_to_list(?MAX_AGE), ReqData),
    case State#state.last_modified of
        undefined -> 
            LMod = filelib:last_modified(State#state.fullpath),
            {LMod, RD1, State#state{last_modified=LMod}};
        LMod ->
            {LMod, RD1, State}
    end.

expires(ReqData, State) ->
    NowSecs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    {calendar:gregorian_seconds_to_datetime(NowSecs + ?MAX_AGE), ReqData, State}.

provide_content(ReqData, State) ->
    RD1 = case State#state.content_disposition of
        inline ->     wrq:set_resp_header("Content-Disposition", "inline", ReqData);
        attachment -> wrq:set_resp_header("Content-Disposition", "attachment", ReqData);
        undefined ->  ReqData
    end,
    {Content, State1} = case State#state.body of
        undefined ->
            {ok, Data} = file:read_file(State#state.fullpath),
            Body = case State#state.encode_data of 
                true -> encode_data(Data);
                false -> Data
            end,
            {Body, State#state{body=Body}};
        Body -> 
            {Body, State}
    end,
    {Content, RD1, State1}.
    
    
finish_request(ReqData, State) ->
    case State#state.is_cached of
        false ->
            case State#state.body of
                undefined ->  
                    {ok, ReqData, State};
                _ ->
                    case State#state.use_cache andalso State#state.encode_data of
                        true ->
                            % Cache the served file in the depcache.  Cache it for 3600 secs.
                            Cache = #cache{
                                        path=State#state.path,
                                        fullpath=State#state.fullpath,
                                        mime=State#state.mime,
                                        last_modified=State#state.last_modified,
                                        body=State#state.body
                                    },
                            z_depcache:set(cache_key(State#state.path), Cache),
                            {ok, ReqData, State};
                        _ ->
                            % No cache or no gzip'ed version (file system cache is fast enough for image serving)
                            {ok, ReqData, State}
                    end
            end;
        true ->
            {ok, ReqData, State}
    end.


%%%%%%%%%%%%%% Helper functions %%%%%%%%%%%%%%
    
cache_key(Path) ->
    {resource_file, Path}.

file_exists(_State, [], _Context) ->
    false;
file_exists(State, Name, Context) ->
    RelName = case hd(Name) of
        "/" -> tl(Name);
        _ -> Name
    end,
    case mochiweb_util:safe_relative_path(RelName) of
        undefined -> false;
        SafePath ->
            RelName = case hd(SafePath) of
                "/" -> tl(SafePath);
                _ -> SafePath
            end,
            file_exists1(State#state.root, RelName, Context)
    end.

file_exists1([], _RelName, _Context) ->
    false;
file_exists1([ModuleIndex|T], RelName, Context) when is_atom(ModuleIndex) ->
    case z_module_indexer:find(ModuleIndex, RelName, Context) of
        {ok, File} -> {true, File};
        {error, _} -> file_exists1(T, RelName, Context)
    end;
file_exists1([DirName|T], RelName, Context) ->
    NamePath = filename:join([DirName,RelName]),
    case filelib:is_regular(NamePath) of 
	true ->
	    {true, NamePath};
	false ->
	    file_exists1(T, RelName, Context)
    end.


%% @spec is_text(Mime) -> bool()
%% @doc Check if a mime type is textual
is_text("text/" ++ _) -> true;
is_text("application/x-javascript") -> true;
is_text("application/xhtml+xml") -> true;
is_text("application/xml") -> true;
is_text(_Mime) -> false.


%% @spec ensure_preview(ReqData, Path, State) -> {Boolean, State}
%% @doc Generate the file on the path from an archived media file.
%% The path is like: 2007/03/31/wedding.jpg(300x300)(crop-center)(709a-a3ab6605e5c8ce801ac77eb76289ac12).jpg
%% The original media should be in State#media_path
%% The generated image should be created in State#root
ensure_preview(ReqData, Path, State) ->
    {Filepath, PreviewPropList, _Checksum, _ChecksumBaseString} = z_media_tag:url2props(Path),
    case mochiweb_util:safe_relative_path(Filepath) of
        undefined ->
            {false, ReqData, State};
        Safepath  ->
            Context = z_context:new(ReqData, ?MODULE),
            MediaFile = case Safepath of
                "lib/" ++ LibPath -> 
                    case z_module_indexer:find(lib, LibPath, Context) of
                        {ok, ModuleFilename} -> ModuleFilename;
                        {error, _} -> filename:join(State#state.media_path, Safepath)
                    end;
                _ ->
                    filename:join(State#state.media_path, Safepath)
            end,
            case filelib:is_regular(MediaFile) of
                true ->
                    % Media file exists, perform the resize
                    % @todo make use of a resize server, so that we do not resize too many files at the same time.
                    [Root|_] = State#state.root,
                    PreviewFile = filename:join(Root, Path),
                    case z_media_preview:convert(MediaFile, PreviewFile, PreviewPropList, Context) of
                        ok -> resource_exists(ReqData, State);
                        {error, Reason} -> throw(Reason)
                    end;
                false ->
                    {false, ReqData, State}
            end
    end.



%% Encode the data so that the identity variant comes first and then the gzip'ed variant
encode_data(Data) when is_binary(Data) ->
    Gzip = zlib:gzip(Data),
    Size = size(Data),
    <<Size:32, Data/binary, Gzip/binary>>.

decode_data(identity, Data) ->
    <<Size:32, _Rest/binary>> = Data,
    <<Size:32, Identity:Size/binary, _Gzip/binary>> = Data,
    Identity;
decode_data(gzip, Data) ->
    <<Size:32, _Rest/binary>> = Data,
    <<Size:32, _Identity:Size/binary, Gzip/binary>> = Data,
    Gzip.
