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
    
allowed_methods(_ReqProps, State) ->
    {['HEAD', 'GET', 'PUT', 'DELETE', 'POST'], State}.

file_path(State, Name) ->
    RelName = case hd(Name) of
        "/" -> tl(Name);
        _ -> Name
    end,
    case mochiweb_util:safe_relative_path(RelName) of
        undefined -> undefined;
        SafePath -> filename:join([State#state.root, SafePath])
    end.

file_exists(State, Name) ->
    case file_path(State, Name) of
        undefined -> false;
        NamePath ->
            case filelib:is_regular(NamePath) of 
        	true ->  {true, NamePath};
        	false -> false
            end
    end.

resource_exists(ReqProps, State) ->
    Path = ?PATH(ReqProps),
    case file_exists(State, Path) of 
	{true, _} ->
	    {true, State};
	_ ->
            case Path of
                "p" -> {true, State};
                _ -> {false, State}
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

content_types_provided(ReqProps, State) ->
    CT = webmachine_util:guess_mime(?PATH(ReqProps)),
    {[{CT, provide_content}],
     State#state{metadata=[{'content-type', CT}|State#state.metadata]}}.

content_types_accepted(ReqProps, State) ->
    Req = ?REQ(ReqProps),
    CT = Req:get_header_value("content-type"),
    {[{CT, accept_content}],
     State#state{metadata=[{'content-type', CT}|State#state.metadata]}}.

accept_content(ReqProps, State) ->
    Req = ?REQ(ReqProps),
    Path = ?PATH(ReqProps),
    FP = file_path(State, Path),
    ok = filelib:ensure_dir(filename:dirname(FP)),
    case file_exists(State, Path) of 
	{true, _} ->
            nop;
	_ ->
            LOC = "http://" ++ Req:get_header_value("host") ++ "/fs/" ++ Path,
            Req:add_response_header("Location", LOC)
    end,
    Value = Req:recv_body(),
    case file:write_file(FP, Value) of
        ok ->
            Req:append_to_response_body(Value),
            {true, State};
        _ ->
            false
    end.    

post_is_create(_ReqProps, State) ->
    {true, State}.

create_path(ReqProps, State) ->
    Req = ?REQ(ReqProps),
    case Req:get_header_value("slug") of
        undefined -> {undefined, State};
        Slug ->
            case file_exists(State, Slug) of
                {true, _} -> {undefined, State};
                _ -> {Slug, State}
            end
    end.

delete_resource(ReqProps, State) ->
    Path = ?PATH(ReqProps),
    case file:delete(file_path(State, Path)) of
        ok -> {true, State};
        _ -> {false, State}
    end.

provide_content(ReqProps, State) ->
    Path = ?PATH(ReqProps),
    case maybe_fetch_object(State, Path) of 
	{true, NewState} ->
	    Body = NewState#state.response_body,
	    {Body, State};
	{false, NewState} ->
	    {error, NewState}
    end.

last_modified(ReqProps, State) ->
    {true, FullPath} = file_exists(State,
                                   proplists:get_value(path, ReqProps)),
    LMod = filelib:last_modified(FullPath),
    {LMod, State#state{metadata=[{'last-modified',
                    httpd_util:rfc1123_date(LMod)}|State#state.metadata]}}.

%hash_body(Body) ->
%    mochihex:to_hex(binary_to_list(crypto:sha(Body))).

%generate_etag(ReqProps, State) ->
%    case maybe_fetch_object(State, proplists:get_value(path, ReqProps)) of
%        {true, BodyState} ->
%            ETag = hash_body(BodyState#state.response_body),
%            {ETag,
%             BodyState#state{metadata=[{etag,ETag}|
%                                           BodyState#state.metadata]}};
%        _ ->
%           {undefined, State}
%    end.
