%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009  Marc Worrell

%% @doc Request context for zophenic request evaluation.

-module(zp_context).
-author("Marc Worrell <marc@worrell.nl>").

-export([
    new/1,
    new/0,

    cleanup_for_template/1,
    cleanup_for_scomp/1,
    output/2,
    
    ensure_all/1,
    ensure_session/1,
    ensure_person/1,
    ensure_page_session/1,
    ensure_qs/1,
    
    get_reqprops/1,
    get_resource_module/1,
    set_resource_module/2,

    get_q/2,
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

    set_context/3,
    set_context/2,
    get_context/2,
    incr_context/3
    
    ]).

-include_lib("webmachine.hrl").
-include_lib("zophrenic.hrl").


%% @doc Create a new context record for the current request.
new(ReqProps) ->
    Req    = ?REQ(ReqProps),
    Module = Req:get_metadata('resource_module'),
    #context{reqprops=ReqProps, dict=dict:new(), resource_module=Module}.

%% @doc Return a new empty context
new() -> #context{dict=dict:new()}.

%% @doc Cleanup a context for the output stream
cleanup_for_template(#context{}=Context) ->
    #context{
        dict=undefined, reqprops=undefined,
        updates=Context#context.updates,
        actions=Context#context.actions,
        content_scripts=Context#context.content_scripts,
        scripts=Context#context.scripts,
        wire=Context#context.wire,
        validators=Context#context.validators,
        render=Context#context.render
    };
cleanup_for_template(Output) -> Output.


%% @doc Cleanup a context for scomp handling.  Resets the dict so that we do not get unneccesary copying during message passing.
%%      The updates/actions etc are not reset as they can accumulate in nested scomp calls.
cleanup_for_scomp(Context) ->
    Context#context{
        dict=undefined
    }.


%% @spec output(list(), Context) -> {io_list(), Context}
%% @doc Replace the contexts in the output with their rendered content and collect all scripts
output(List, Context) ->
    output1(List, Context, []).

%% @doc Recursively walk through the output, replacing all context placeholders with their rendered output
output1([], Context, Acc) ->
    {lists:reverse(Acc), Context};
output1([#context{}=C|Rest], Context, Acc) ->
    {Rendered, Context1} = output1(C#context.render, Context, []),
    output1(Rest, merge_scripts(C, Context1), [Rendered|Acc]);
output1([{script}|Rest], Context, Acc) ->
    DefaultFormPostback = zp_render:make_postback_info("", "submit", undefined, undefined, undefined, Context),
    Script = [
            <<"\n\n<script type='text/javascript'>\n$(function() {\n">>,
                zp_script:get_page_startup_script(Context),
                zp_script:get_script(Context), 
                <<"zp_init_postback_forms();\nzp_default_form_postback = \"">>, DefaultFormPostback, $", $;,
            <<"\n});\n</script>\n">>
           ],
    output1(Rest, Context, [Script|Acc]);
output1([List|Rest], Context, Acc) when is_list(List) ->
    {Rendered, Context1} = output1(List, Context, []),
    output1(Rest, Context1, [Rendered|Acc]);
output1([C|Rest], Context, Acc) ->
    output1(Rest, Context, [C|Acc]).


%% @spec merge_scripts(Context, ContextAcc) -> Context
%% @doc Merge the scripts in contet C into the context accumulator, used when collecting all scripts in an output stream
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


%% @doc Ensure session and page session and fetch&parse the query string
ensure_all(Context) ->
    Context1 = ensure_session(Context),
    Context2 = ensure_person(Context1),
    Context3 = ensure_page_session(Context2),
    ensure_qs(Context3).

%% @doc Ensure that we have a session, start a new session process when needed
ensure_session(Context) ->
    case Context#context.session_pid of
        undefined ->
            Context1 = zp_session_manager:ensure_session(Context),
            zp_person:associate_session(Context1#context.person_pid, Context1#context.session_pid),
            Context1;
        _ ->
            Context    
    end.
    
%% @doc Ensure that we have a person, start a new person process when needed
ensure_person(Context) ->
    case Context#context.person_pid of
        undefined ->
            zp_person_manager:ensure_person(Context);
        _ ->
            Context    
    end.

%% @doc Ensure that we have a page session, used for comet and postback requests
ensure_page_session(Context) ->
    case Context#context.page_pid of
        undefined ->
            Context1 = ensure_session(Context),
            zp_session:ensure_page_session(Context1, Context#context.session_pid);
        _ ->
            Context
    end.
    
%% @doc Ensure that we have parsed the query string, fetch body if necessary
ensure_qs(Context) ->
    case dict:find('q', Context#context.dict) of
        {ok, _Qs} ->
            Context;
        error ->
            ReqProps = Context#context.reqprops,
            Req      = ?REQ(ReqProps),
            PathArgs = lists:map(fun ({T,V}) -> {atom_to_list(T),V} end, Req:get_path_info()),
            Body     = parse_form_urlencoded(Req),
            Query    = Req:parse_qs(),
            Combined = PathArgs ++ Body ++ Query,
            Dict2    = dict:store('q', Combined, Context#context.dict),
            Context#context{dict=Dict2}
    end.


%% @spec get_reqprops(Context) -> ReqProps
%% @doc Return the request properties of the current context
get_reqprops(Context) ->
    Context#context.reqprops.


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
    {ok, Qs} = dict:find('q', Context#context.dict),
    proplists:get_value(Key, Qs).


%% @spec get_q_all(Key, Context) -> Values
%%        Key -> string()
%%        Values -> list()
%% @doc Get the all the parameters with the same name, returns the empty list when non found.
get_q_all(Key, Context) ->
    {ok, Qs} = dict:find('q', Context#context.dict),
    proplists:get_all_values(Key, Qs).
    

%% @spec get_q_validated(Key, Context) -> Value
%% @doc Fetch a query parameter and perform the validation connected to the parameter. An exception {not_validated, Key}
%%      is thrown when there was no validator, when the validator is invalid or when the validation failed.
get_q_validated(Key, Context) ->
    case dict:find('q_validated', Context#context.dict) of
        {ok, Qs} ->
            case dict:find(Key, Qs) of
                {ok, Value} -> Value;
                error -> throw({not_validated, Key})
            end
    end.


%% ------------------------------------------------------------------------------------
%% Communicate with pages, session and user processes
%% ------------------------------------------------------------------------------------

%% @doc Add a script to the all pages of the session. Used for comet feeds.
add_script_session(Script, Context) ->
    zp_session:add_script(Script, Context#context.session_pid).


%% @doc Add a script to the page in the user agent.  Used for comet feeds.
add_script_page(Script, Context) ->
    zp_session_page:add_script(Script, Context#context.page_pid).


%% @doc Spawn a new process, link it to the session process.
spawn_link_session(Module, Func, Args, Context) ->
    LinkContext = #context{
                    person_pid  = Context#context.person_pid,
                    session_pid = Context#context.session_pid,
                    page_pid    = Context#context.page_pid
                },
    zp_session:spawn_link(Module, Func, Args, LinkContext).

%% @doc Spawn a new process, link it to the page process.  Used for comet feeds.
spawn_link_page(Module, Func, Args, Context) ->
    LinkContext = #context{
                    person_pid  = Context#context.person_pid,
                    session_pid = Context#context.session_pid,
                    page_pid    = Context#context.page_pid
                },
    zp_session_page:spawn_link(Module, Func, Args, LinkContext).



%% ------------------------------------------------------------------------------------
%% Set/get/modify state dicts
%% ------------------------------------------------------------------------------------


%% @spec get_value(Key::string(), Context) -> Value | undefined
%% @spec Find a key in the context, page, session or user state.
%% @todo Add page and user lookup
get_value(Key, Context) ->
    case get_context(Key, Context) of
        undefined ->
            case get_page(Key, Context) of
                undefined -> get_session(Key, Context);
                Value -> Value
            end;
        Value ->
            Value
    end.


%% @spec set_session(Key, Value, Context) -> Context
%% @doc Set the value of the session variable Key to Value
set_session(Key, Value, Context) ->
    zp_session:set(Key, Value, Context#context.session_pid),
    Context.

%% @spec get_session(Key, Context) -> Value
%% @doc Fetch the value of the session variable Key
get_session(_Key, #context{session_pid=undefined}) ->
    undefined;
get_session(Key, Context) ->
    zp_session:get(Key, Context#context.session_pid).


%% @spec incr_session(Key, Increment, Context) -> {NewValue, NewContext}
%% @doc Increment the session variable Key
incr_session(Key, Value, Context) ->
    {zp_session:incr(Key, Value, Context#context.session_pid), Context}.


%% @spec set_page(Key, Value, Context) -> Context
%% @doc Set the value of the page variable Key to Value
set_page(Key, Value, Context) ->
    zp_session_page:set(Key, Value, Context#context.page_pid),
    Context.

%% @spec get_page(Key, Context) -> Value
%% @doc Fetch the value of the page variable Key
get_page(_Key, #context{page_pid=undefined}) ->
    undefined;
get_page(Key, Context) ->
    zp_session_page:get(Key, Context#context.page_pid).


%% @spec incr_page(Key, Increment, Context) -> {NewValue, NewContext}
%% @doc Increment the page variable Key
incr_page(Key, Value, Context) ->
    {zp_session_page:incr(Key, Value, Context#context.session_pid), Context}.


%% @spec set(Key, Value, Context) -> Context
%% @doc Set the value of the context variable Key to Value
set_context(Key, Value, Context) ->
    Dict = dict:store(Key, Value, Context#context.dict),
    Context#context{dict = Dict}.


%% @spec set(PropList, Context) -> Context
%% @doc Set the value of the context variables to all {Key, Value} properties.
set_context(PropList, Context) when is_list(PropList) ->
    NewDict = lists:foldl(fun ({Key,Value}, Dict) -> dict:store(Key, Value, Dict) end, Context#context.dict, PropList),
    Context#context{dict = NewDict}.


%% @spec get(Key, Context) -> Value
%% @doc Fetch the value of the context variable Key
get_context(Key, Context) ->
    case dict:find(Key, Context#context.dict) of
        {ok, Value} ->
                Value;
        error ->
                undefined
    end.

%% @spec incr_session(Key, Increment, Context) -> {NewValue,NewContext}
%% @doc Increment the context variable Key
incr_context(Key, Value, Context) ->
    Dict = dict:update_counter(Key, Value, Context#context.dict),
    Context1 = Context#context{dict = Dict},
    get_context(Key, Context1).



%% ------------------------------------------------------------------------------------
%% Local helper functions
%% ------------------------------------------------------------------------------------

%% @spec parse_form_urlencoded(Req) -> list()
%% @doc Return the keys in the body of the request, only if the request is application/x-www-form-urlencoded
parse_form_urlencoded(Req) ->
    case Req:get_header_value("content-type") of
        "application/x-www-form-urlencoded" ++ _ ->
            case Req:recv_body() of
                undefined ->
                     [];
                Binary ->
                     mochiweb_util:parse_qs(Binary)
            end;
        _ ->
            []
    end.

