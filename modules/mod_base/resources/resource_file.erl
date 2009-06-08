%% @author Bryan Fink <bryan@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2008 Basho Technologies, Inc.

-module(resource_file).
-export([init/1]).
-export([allowed_methods/2,
	 resource_exists/2,
	 last_modified/2,
	 content_types_provided/2,
	 content_types_accepted/2,
     delete_resource/2,
     post_is_create/2,
     create_path/2,
	 provide_content/2,
	 accept_content/2
	 %%generate_etag/2
	 ]).


-include_lib("kernel/include/file.hrl").
-include_lib("webmachine_resource.hrl").

-record(state, {root=undefined, response_body=undefined, fullpath=undefined, metadata=[]}).

init(ConfigProps) ->
    {root, Root} = proplists:lookup(root, ConfigProps),
    {ok, #state{root=Root}}.
    
allowed_methods(ReqData, State) ->
    {['HEAD', 'GET', 'PUT', 'DELETE', 'POST'], ReqData, State}.

file_path(State, Name) ->
    RelName = case hd(Name) of
        "/" -> tl(Name);
        _ -> Name
    end,
    case mochiweb_util:safe_relative_path(RelName) of
        undefined ->
            undefined;
        SafePath ->
            filename:join([State#state.root, SafePath])
    end.

file_exists(State, Name) ->
    case file_path(State, Name) of
        undefined ->
            false;
        NamePath ->
            case filelib:is_regular(NamePath) of 
        	true ->  {true, NamePath};
        	false -> false
            end
    end.

resource_exists(ReqData, State) ->
    Path = wrq:disp_path(ReqData),
    case file_exists(State, Path) of 
    	{true, _} ->
    	    {true, ReqData, State};
    	_ ->
            case Path of
                "p" -> {true, ReqData, State};
                _ -> {false, ReqData, State}
            end
    end.

maybe_fetch_object(State, Path) ->
    % if returns {true, NewState} then NewState has response_body
    case State#state.response_body of
    	undefined ->
    	    case file_exists(State, Path) of 
        		{true, FullPath} ->
        		    {ok, Value} = file:read_file(FullPath),
        		    {true, State#state{response_body=Value}};
        		false ->
        		    {false, State}
    	    end;
    	_Body ->
    	    {true, State}
    end.

content_types_provided(ReqData, State) ->
    CT = zp_utils:guess_mime(wrq:disp_path(ReqData)),
    {[{CT, provide_content}],
     ReqData,
     State#state{metadata=[{'content-type', CT}|State#state.metadata]}}.
     
content_types_accepted(ReqData, State) ->
    CT = wrq:get_req_header("content-type", ReqData),
    {[{CT, accept_content}],
     ReqData,
     State#state{metadata=[{'content-type', CT}|State#state.metadata]}}.

accept_content(ReqData, State) ->
    Path = wrq:disp_path(ReqData),
    FP = file_path(State, Path),
    ok = filelib:ensure_dir(filename:dirname(FP)),
    RD1 = case file_exists(State, Path) of 
    	{true, _} ->
                ReqData;
    	_ ->
                LOC = "http://" ++ wrq:get_req_header("host") ++ "/fs/" ++ Path,
                wrq:set_resp_header("Location", LOC, ReqData)
    end,
    Value = wrq:req_body(RD1),
    case file:write_file(FP, Value) of
        ok ->
            RD2 = wrq:append_to_response_body(Value, RD1),
            {true, RD2, State};
        _ ->
            {false, RD1, State}
    end.    

post_is_create(ReqData, State) ->
    {true, ReqData, State}.

create_path(ReqData, State) ->
    case wrq:get_req_header("slug", ReqData) of
        undefined -> {undefined, ReqData, State};
        Slug ->
            case file_exists(State, Slug) of
                {true, _} -> {undefined, ReqData, State};
                _ -> {Slug, ReqData, State}
            end
    end.

delete_resource(ReqData, State) ->
    Path = wrq:disp_path(ReqData),
    case file:delete(file_path(State, Path)) of
        ok -> {true, ReqData, State};
        _ -> {false, ReqData, State}
    end.

provide_content(ReqData, State) ->
    Path = wrq:disp_path(ReqData),
    case maybe_fetch_object(State, Path) of 
	{true, NewState} ->
	    Body = NewState#state.response_body,
	    {Body, ReqData, State};
	{false, NewState} ->
	    {error, ReqData, NewState}
    end.

last_modified(ReqData, State) ->
    Path = wrq:disp_path(ReqData),
    {true, FullPath} = file_exists(State, Path),
    LMod = filelib:last_modified(FullPath),
    {LMod, ReqData, State#state{metadata=[{'last-modified',
                    httpd_util:rfc1123_date(LMod)}|State#state.metadata]}}.

