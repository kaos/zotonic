%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009  Marc Worrell

%% @doc Request context for zophenic request evaluation.

-module(z_context).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    new/2,
    new/0,
    new_for_host/1,

    prune_for_async/1,
    prune_for_template/1,
    prune_for_database/1,
    prune_for_scomp/2,
    output/2,

    pickle/1,
    depickle/1,
    
    combine_results/2,

    ensure_all/1,
    ensure_session/1,
    ensure_visitor/1,
    ensure_visitor_id/1,
    ensure_page_session/1,
    ensure_qs/1,
    
    get_reqdata/1,
    set_reqdata/2,
    get_resource_module/1,
    set_resource_module/2,

    get_q/2,
    get_q/3,
    get_q_all/1,
    get_q_all/2,
    get_q_validated/2,

    add_script_session/2,
    add_script_page/2,

    spawn_link_session/4,
    spawn_link_page/4,
    
    get_value/2,
    
    set_session/3,
    get_session/2,
    incr_session/3,

    set_page/3,
    get_page/2,
    incr_page/3,

    set_visitor/3,
    get_visitor/2,

    set/3,
    set/2,
    get/2,
    get/3,
    incr/3,
    get_all/1,
    
    language/1,
    
    merge_scripts/2,
    copy_scripts/2
]).

-include_lib("zotonic.hrl").


%% @doc Create a new context record for the current request.
new(ReqData, Module) ->
    Context = #context{wm_reqdata=ReqData, resource_module=Module},
    Context#context{language=z_trans:default_language(Context)}.

%% @doc Return a new empty context
new() -> 
    Context = #context{},
    Context#context{language=z_trans:default_language(Context)}.

%% @doc Return an almost empty context for the database connection only, nothing else is initialised
new_for_host(#context{host=Host}) ->
    Context = new(),
    Context#context{host=Host};
new_for_host(Host) ->
    Context = new(),
    Context#context{host=Host}.

%% @doc Make the context safe to use in a async message
prune_for_async(#context{wm_reqdata=ReqData, host=Host, acl=Acl, props=Props}) ->
    #context{wm_reqdata=ReqData, host=Host, acl=Acl, props=Props}.


%% @doc Cleanup a context for the output stream
prune_for_template(#context{}=Context) ->
    #context{
        wm_reqdata=undefined,
        props=undefined,
        updates=Context#context.updates,
        actions=Context#context.actions,
        content_scripts=Context#context.content_scripts,
        scripts=Context#context.scripts,
        wire=Context#context.wire,
        validators=Context#context.validators,
        render=Context#context.render
    };
prune_for_template(Output) -> Output.


%% @doc Cleanup a context so that it can be used exclusively for database connections
prune_for_database(Context) ->
    #context{host=Context#context.host, dbc=Context#context.dbc}.

%% @doc Cleanup a context for cacheable scomp handling.  Resets most of the accumulators to prevent duplicating
%% between different (cached) renderings.
prune_for_scomp(VisibleFor, Context) ->
    z_acl:set_visible_for(VisibleFor, Context#context{
        dbc=undefined,
	    wm_reqdata=undefined,
		updates=[],
		actions=[],
		content_scripts=[],
		scripts=[],
		wire=[],
		validators=[],
		render=[]
    }).


%% @doc Pickle a context for storing in the database
%% @todo pickle/depickle the visitor id (when any)
%% @spec pickle(Context) -> tuple()
pickle(Context) ->
    {pickled_context, Context#context.host, Context#context.user_id, Context#context.language, undefined}.

%% @doc Depickle a context for restoring from a database
%% @todo pickle/depickle the visitor id (when any)
depickle({pickled_context, Host, UserId, Language, _VisitorId}) ->
    Context = #context{host=Host, language=Language},
    case UserId of
        undefined -> Context;
        _ -> z_acl:logon(UserId, Context)
    end.

%% @spec output(list(), Context) -> {io_list(), Context}
%% @doc Replace the contexts in the output with their rendered content and collect all scripts
output(<<>>, Context) ->
    {[], Context};
output(List, Context) ->
    output1(List, Context, []).

%% @doc Recursively walk through the output, replacing all context placeholders with their rendered output
output1([], Context, Acc) ->
    {lists:reverse(Acc), Context};
output1([#context{}=C|Rest], Context, Acc) ->
    {Rendered, Context1} = output1(C#context.render, Context, []),
    output1(Rest, merge_scripts(C, Context1), [Rendered|Acc]);
output1([{script}|Rest], Context, Acc) ->
    DefaultFormPostback = z_render:make_postback_info("", "submit", undefined, undefined, undefined, Context),
    Script = [
            <<"\n\n<script type='text/javascript'>\n$(function() {\n">>,
                z_script:get_page_startup_script(Context),
                z_script:get_script(Context), 
                <<"z_init_postback_forms();\nz_default_form_postback = \"">>, DefaultFormPostback, $", $;,
            <<"\n});\n</script>\n">>
           ],
    output1(Rest, Context, [Script|Acc]);
output1([List|Rest], Context, Acc) when is_list(List) ->
    {Rendered, Context1} = output1(List, Context, []),
    output1(Rest, Context1, [Rendered|Acc]);
output1([undefined|Rest], Context, Acc) ->
    output1(Rest, Context, Acc);
output1([C|Rest], Context, Acc) ->
    output1(Rest, Context, [C|Acc]).

%% @spec combine_results(Context1, Context2) -> Context
%% @doc Merge the scripts and the rendered content of two contexts into Context1
combine_results(C1, C2) ->
	Merged = merge_scripts(C2, C1),
    Merged#context{
        render=combine(C1#context.render, C2#context.render)
    }.

%% @spec merge_scripts(Context, ContextAcc) -> Context
%% @doc Merge the scripts from context C into the context accumulator, used when collecting all scripts in an output stream
merge_scripts(C, Acc) ->
    Acc#context{
        updates=combine(Acc#context.updates, C#context.updates),
        actions=combine(Acc#context.actions, C#context.actions),
        content_scripts=combine(Acc#context.content_scripts, C#context.content_scripts),
        scripts=combine(Acc#context.scripts, C#context.scripts),
        wire=combine(Acc#context.wire, C#context.wire),
        validators=combine(Acc#context.validators, C#context.validators)
    }.
    
combine([],X) -> X;
combine(X,[]) -> X;
combine(X,Y) -> [X++Y].


%% @doc Overwrite the scripts in Context with the scripts in From
%% @spec set_scripts(From, Context) -> Context
copy_scripts(From, Context) ->
    Context#context{
        updates=From#context.updates,
        actions=From#context.actions,
        content_scripts=From#context.content_scripts,
        scripts=From#context.scripts,
        wire=From#context.wire,
        validators=From#context.validators
    }.

%% @doc Ensure session and page session and fetch&parse the query string
ensure_all(Context) ->
    Context1 = ensure_session(Context),
    Context2 = ensure_visitor(Context1),
    Context3 = ensure_page_session(Context2),
    ensure_qs(Context3).

%% @doc Ensure that we have a session, start a new session process when needed
ensure_session(Context) ->
    case Context#context.session_pid of
        undefined ->
            Context1 = z_session_manager:ensure_session(Context),
            z_visitor:associate_session(Context1#context.visitor_pid, Context1#context.session_pid),
            Context2 = z_auth:logon_from_session(Context1),
            add_nocache_headers(Context2);
        _ ->
            Context    
    end.
    
%% @doc Ensure that we have a visitor, start a new visitor process when needed
ensure_visitor(Context) ->
    case Context#context.visitor_pid of
        undefined ->
            z_visitor_manager:ensure_visitor(Context);
        _ ->
            Context    
    end.

%% @doc Ensure that we have a page session, used for comet and postback requests
ensure_page_session(Context) ->
    case Context#context.page_pid of
        undefined ->
            Context1 = ensure_session(Context),
            z_session:ensure_page_session(Context1, Context#context.session_pid);
        _ ->
            Context
    end.
    
%% @doc Ensure that we have parsed the query string, fetch body if necessary
ensure_qs(Context) ->
    case proplists:lookup('q', Context#context.props) of
        {'q', _Qs} ->
            Context;
        none ->
            ReqData  = Context#context.wm_reqdata,
            PathDict = wrq:path_info(ReqData),
            PathArgs = lists:map(
                            fun ({T,V}) -> {atom_to_list(T),mochiweb_util:unquote(V)} end, 
                            dict:to_list(PathDict)),
            {Body, ContextParsed} = parse_form_urlencoded(Context),
            Query    = wrq:req_qs(ReqData),
            Combined = PathArgs ++ Body ++ Query,
            QProps = z_utils:prop_replace('q', Combined, ContextParsed#context.props),
            ContextParsed#context{props=QProps}
    end.


%% @spec get_reqdata(Context) -> #wm_reqdata
%% @doc Return the webmachine request data of the context
get_reqdata(Context) ->
    Context#context.wm_reqdata.

%% @spec set_reqdata(ReqData, Context) -> #wm_reqdata
%% @doc Set the webmachine request data of the context
set_reqdata(ReqData = #wm_reqdata{}, Context) ->
    Context#context{wm_reqdata=ReqData}.


%% @spec get_resource_module(Context) -> term()
%% @doc Get the resource module handling the request.
get_resource_module(Context) ->
    Context#context.resource_module.

%% @spec set_resource_module(Context) -> NewContext
set_resource_module(Module, Context) ->
    Context#context{resource_module=Module}.


%% @spec get_q(Key, Context) -> Value | undefined
%%       Value -> string()
%%       Key -> string()
%% @doc Get a request parameter, either from the query string or the post body.  Post body has precedence over the query string.
get_q(Key, Context) ->
    case proplists:lookup('q', Context#context.props) of
        {'q', Qs} -> proplists:get_value(z_convert:to_list(Key), Qs);
        none -> undefined
    end.


%% @spec get_q(Key, Context, Default) -> Value
%%       Value -> string()
%%       Key -> string()
%% @doc Get a request parameter, either from the query string or the post body.  Post body has precedence over the query string.
get_q(Key, Context, Default) ->
    case proplists:lookup('q', Context#context.props) of
        {'q', Qs} -> proplists:get_value(z_convert:to_list(Key), Qs, Default);
        none -> undefined
    end.


%% @spec get_q_all(Context) -> Values
%%        Key -> string()
%%        Values -> list()
%% @doc Get all parameters.
get_q_all(Context) ->
    {'q', Qs} = proplists:lookup('q', Context#context.props),
    Qs.


%% @spec get_q_all(Key, Context) -> Values
%%        Key -> string()
%%        Values -> list()
%% @doc Get the all the parameters with the same name, returns the empty list when non found.
get_q_all(Key, Context) ->
    {'q', Qs} = proplists:lookup('q', Context#context.props),
    proplists:get_all_values(z_convert:to_list(Key), Qs).
    

%% @spec get_q_validated(Key, Context) -> Value
%% @doc Fetch a query parameter and perform the validation connected to the parameter. An exception {not_validated, Key}
%%      is thrown when there was no validator, when the validator is invalid or when the validation failed.
get_q_validated(Key, Context) ->
    case proplists:lookup('q_validated', Context#context.props) of
        {'q_validated', Qs} ->
            case proplists:lookup(z_convert:to_list(Key), Qs) of
                {_Key, Value} -> Value;
                none -> throw({not_validated, Key})
            end
    end.


%% ------------------------------------------------------------------------------------
%% Communicate with pages, session and user processes
%% ------------------------------------------------------------------------------------

%% @doc Add a script to the all pages of the session. Used for comet feeds.
add_script_session(Script, Context) ->
    z_session:add_script(Script, Context#context.session_pid).


%% @doc Add a script to the page in the user agent.  Used for comet feeds.
add_script_page(Script, Context) ->
    z_session_page:add_script(Script, Context#context.page_pid).


%% @doc Spawn a new process, link it to the session process.
spawn_link_session(Module, Func, Args, Context) ->
    LinkContext = #context{
                    visitor_pid  = Context#context.visitor_pid,
                    session_pid = Context#context.session_pid,
                    page_pid    = Context#context.page_pid
                },
    z_session:spawn_link(Module, Func, Args, LinkContext).

%% @doc Spawn a new process, link it to the page process.  Used for comet feeds.
spawn_link_page(Module, Func, Args, Context) ->
    LinkContext = #context{
                    visitor_pid  = Context#context.visitor_pid,
                    session_pid = Context#context.session_pid,
                    page_pid    = Context#context.page_pid
                },
    z_session_page:spawn_link(Module, Func, Args, LinkContext).


%% @doc Ensure that we have an id for the visitor
ensure_visitor_id(Context) ->
    Context1 = ensure_visitor(Context),
    {z_visitor:ensure_visitor_id(Context1#context.visitor_pid), Context1}.


%% ------------------------------------------------------------------------------------
%% Set/get/modify state properties
%% ------------------------------------------------------------------------------------


%% @spec get_value(Key::string(), Context) -> Value | undefined
%% @spec Find a key in the context, page, session or visitor state.
%% @todo Add page and user lookup
get_value(Key, Context) ->
    case get(Key, Context) of
        undefined ->
            case get_page(Key, Context) of
                undefined -> 
                    case get_session(Key, Context) of
                        undefined -> get_visitor(Key, Context);
                        Value -> Value
                    end;
                Value ->
                    Value
            end;
        Value ->
            Value
    end.


%% @spec set_visitor(Key, Value, Context) -> Context
%% @doc Set the value of the visitor variable Key to Value
set_visitor(Key, Value, Context) ->
    z_visitor:set(Key, Value, Context#context.visitor_pid),
    Context.

%% @spec get_visitor(Key, Context) -> Value
%% @doc Fetch the value of the visitor variable Key
get_visitor(_Key, #context{visitor_pid=undefined}) ->
    undefined;
get_visitor(Key, Context) ->
    z_visitor:get(Key, Context#context.visitor_pid).


%% @spec set_session(Key, Value, Context) -> Context
%% @doc Set the value of the session variable Key to Value
set_session(Key, Value, Context) ->
    z_session:set(Key, Value, Context#context.session_pid),
    Context.

%% @spec get_session(Key, Context) -> Value
%% @doc Fetch the value of the session variable Key
get_session(_Key, #context{session_pid=undefined}) ->
    undefined;
get_session(Key, Context) ->
    z_session:get(Key, Context#context.session_pid).

%% @spec incr_session(Key, Increment, Context) -> {NewValue, NewContext}
%% @doc Increment the session variable Key
incr_session(Key, Value, Context) ->
    {z_session:incr(Key, Value, Context#context.session_pid), Context}.

%% @spec set_page(Key, Value, Context) -> Context
%% @doc Set the value of the page variable Key to Value
set_page(Key, Value, Context) ->
    z_session_page:set(Key, Value, Context#context.page_pid),
    Context.

%% @spec get_page(Key, Context) -> Value
%% @doc Fetch the value of the page variable Key
get_page(_Key, #context{page_pid=undefined}) ->
    undefined;
get_page(Key, Context) ->
    z_session_page:get(Key, Context#context.page_pid).


%% @spec incr_page(Key, Increment, Context) -> {NewValue, NewContext}
%% @doc Increment the page variable Key
incr_page(Key, Value, Context) ->
    {z_session_page:incr(Key, Value, Context#context.session_pid), Context}.


%% @spec set(Key, Value, Context) -> Context
%% @doc Set the value of the context variable Key to Value
set(Key, Value, Context) ->
    Props = z_utils:prop_replace(Key, Value, Context#context.props),
    Context#context{props = Props}.


%% @spec set(PropList, Context) -> Context
%% @doc Set the value of the context variables to all {Key, Value} properties.
set(PropList, Context) when is_list(PropList) ->
    NewProps = lists:foldl(
        fun ({Key,Value}, Props) -> 
            z_utils:prop_replace(Key, Value, Props)
        end, Context#context.props, PropList),
    Context#context{props = NewProps}.


%% @spec get(Key, Context) -> Value | undefined
%% @doc Fetch the value of the context variable Key, return undefined when Key is not found.
get(Key, Context) ->
    case proplists:lookup(Key, Context#context.props) of
        {Key, Value} -> Value;
        none -> undefined
    end.

%% @spec get(Key, Context, Default) -> Value | Default
%% @doc Fetch the value of the context variable Key, return Default when Key is not found.
get(Key, Context, Default) ->
    case proplists:lookup(Key, Context#context.props) of
        {Key, Value} -> Value;
        none -> Default
    end.


%% @spec get_all(Context) -> PropList
%% @doc Return a proplist with all context variables.
get_all(Context) ->
    Context#context.props.


%% @spec incr_session(Key, Increment, Context) -> {NewValue,NewContext}
%% @doc Increment the context variable Key
incr(Key, Value, Context) ->
    case z_convert:to_integer(get(Key, Context)) of
        undefined ->
            set(Key, Value, Context),
            Value;
        N ->
            R = N+Value, 
            set(Key, R, Context),
            R
    end.


%% @doc Return the selected language of the Context
language(Context) ->
    Context#context.language.

%% ------------------------------------------------------------------------------------
%% Local helper functions
%% ------------------------------------------------------------------------------------

%% @spec parse_form_urlencoded(context()) -> {list(), NewContext}
%% @doc Return the keys in the body of the request, only if the request is application/x-www-form-urlencoded
parse_form_urlencoded(Context) ->
    ReqData = get_reqdata(Context),
    case wrq:get_req_header("content-type", ReqData) of
        "application/x-www-form-urlencoded" ++ _ ->
            case wrq:req_body(ReqData) of
                undefined ->
                     {[], Context};
                Binary ->
                     {mochiweb_util:parse_qs(Binary), Context}
            end;
        "multipart/form-data" ++ _ ->
            {Form, ContextRcv} = z_parse_multipart:recv_parse(Context),
            FileArgs = [ {Name, #upload{filename=Filename, tmpfile=TmpFile}} || {Name, Filename, TmpFile} <- Form#multipart_form.files ],
            {Form#multipart_form.args ++ FileArgs, ContextRcv};
        _Other ->
            {[], Context}
    end.


%% @doc Some user agents have too aggressive client side caching.  These headers prevent
%% the caching of content on the user agent iff the content generated has a session. You can prevent
%% addition of these headers by not calling z_context:ensure_session/1, or z_context:ensure_all/1.
%% @spec add_nocache_headers(#context) -> #context
add_nocache_headers(Context = #context{wm_reqdata=ReqData}) ->
    RD1 = wrq:set_resp_header("Cache-Control", "no-store, no-cache, must-revalidate, post-check=0, pre-check=0", ReqData),
    RD2 = wrq:set_resp_header("Expires", httpd_util:rfc1123_date({{2008,12,10}, {15,30,0}}), RD1),
    % This let IE6 accept our cookies, basically we tell IE6 that our cookies do not contain any private data.
    RD3 = wrq:set_resp_header("P3P", "CP=\"NOI ADM DEV PSAi COM NAV OUR OTRo STP IND DEM\"", RD2),
    Context#context{wm_reqdata=RD3}.
