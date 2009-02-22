%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Session for zophrenic, holds all information for the current session at an user agent.
%%      An agent can have multiple pages open and an user_session can have multiple sessions.
%%      The user agent session also starts up and monitors the page sessions.
%%
%%      The page session for interaction with the page displayed on the user agent. Support for comet polls.
%%      The page session is the switchboard for getting data pushed to the user agent.  The page session
%%      also caches the page dictionary.  Whenever the new page session is loaded it will check the page
%%      dictionary checksum with the one stored.  When they differ then the page will queue a request
%%      for fetching the data from the page in the user agent.  All queued requests can be sent via 
%%      the current request being handled or via a comet poll.


-module(zp_session).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-include_lib("zophrenic.hrl").

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% session exports
-export([
    start_link/0,
    start_link/1,
    stop/1, 
    set/3,
    get/2, 
    incr/3, 
    keepalive/3, 
    ensure_page_session/2,
    add_script/2,
    check_expire/2,
    spawn_link/4
    ]).


%% The session state
-record(session, {
            vars,
            now,
            expire,
            timer_ref,
            pages
            }).

%% The state per page
-record(page, {
            page_id,
            expire,
            page_pid
            }).

-define(PAGEID_OFFSET, 2).


%% Default session expiration in seconds.
%% The first keepalive message must be received before SESSION_EXPIRE_1 seconds
%% Subsequent messages must be received before SESSION_EXPIRE_N
-define(SESSION_EXPIRE_1,   40).
-define(SESSION_EXPIRE_N, 3600).


start_link() ->
    start_link([]).
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

stop(Pid) ->
    try
        gen_server:cast(Pid, stop)
    catch _Class:_Term -> 
        error 
    end.

set(Key, Value, Pid) ->
    gen_server:cast(Pid, {set, Key, Value}).

get(Key, Pid) ->
    gen_server:call(Pid, {get, Key}).

incr(Key, Value, Pid) ->
    gen_server:call(Pid, {incr, Key, Value}).


%% @doc Reset the timeout counter of the page and session according to the current tick
keepalive(Now, PageId, Pid) ->
    gen_server:cast(Pid, {keepalive, PageId, Now}).


%% @spec ensure_page_session(Context::#context, Pid::pid()) -> #context
%% @doc Make sure that the request has a page session, when the page session was alive then
%%      adjust the expiration of the page.  Returns a new context with the page id set.
ensure_page_session(Context, Pid) ->
    gen_server:call(Pid, {ensure_page_session, Context}).


%% @spec add_script(Script::io_list(), PageId::list(), Pid::pid()) -> none()
%% @doc Send a script to all session pages
add_script(Script, Pid) ->
    gen_server:cast(Pid, {add_script, Script}).


%% @spec check_expire(Now::integer(), Pid::pid()) -> none()
%% @doc Check session and page expiration, periodically called by the session manager
check_expire(Now, Pid) ->
    gen_server:cast(Pid, {check_expire, Now}).


%% @doc Spawn a new process, linked to the session pid
spawn_link(Module, Func, Args, Context) ->
    gen_server:call(Context#context.session_pid, {spawn_link, Module, Func, Args, Context}).


%% Gen_server callbacks

init(Args) ->
    Session = new_session(Args),
    {ok, Session}.

handle_cast(stop, Session) ->
    {stop, normal, Session};

%% @doc Reset the timeout counter for the session and, optionally, a specific page
handle_cast({keepalive, PageId, Now}, Session) ->
    Session1 = Session#session{expire=Now + ?SESSION_EXPIRE_N, now=Now},
    Session2 = case find_page(PageId, Session1) of
                undefined -> 
                    Session1;
                P -> 
                    % Update the keepalive timestamp on the record
                    P1 = page_keepalive(P, Now),
                    store_page(PageId, P1, Session1)
               end,
    {noreply, Session2};

%% @doc Check session expiration, throw away all page administrations that didn't receive a keepalive for some time
handle_cast({check_expire, Now}, Session) ->
    if 
        Session#session.expire < Now ->
            {stop, normal, Session};
        true ->
            IsAlive = fun(Page) ->
                        Page#page.expire >= Now
                      end,
            {Alive,Expired} = lists:partition(IsAlive, Session#session.pages),
            {Alive1, _Now}  = lists:foldl(fun try_stop/2, {Alive,Now}, Expired),
            {noreply, Session#session{pages=Alive1, now=Now}}
    end;

%% @doc Add a script to a specific page's script queue
handle_cast({send_script, Script, PageId}, Session) ->
    Session1 = case find_page(PageId, Session) of
                undefined -> 
                    Session;
                P -> 
                    store_page(PageId, add_script(Script, P), Session)
               end,
    {noreply, Session1};

%% @doc Add a script to all page's script queues
handle_cast({add_script, Script}, Session) ->
    F = fun(P) ->
            catch zp_session_page:add_script(Script, P#page.page_pid)
        end,
    lists:foreach(F, Session#session.pages),
    {noreply, Session};

%% @doc Set the session variable, replaces any old value
handle_cast({set, Key, Value}, Session) ->
    Session1 = Session#session{vars = dict:store(Key, Value, Session#session.vars)},
    {noreply, Session1}.

handle_call({get, Key}, _From, Session) ->
    Value = case dict:find(Key, Session#session.vars) of
                {ok, V} -> V;
                error -> undefined
            end,
    {reply, Value, Session};

handle_call({incr, Key, Delta}, _From, Session) ->
    Session1 = Session#session{vars = dict:update_counter(Key, Delta, Session#session.vars)},
    {ok, Value}  = dict:find(Key, Session1#session.vars),
    {reply, Value, Session1};

handle_call({spawn_link, Module, Func, Args, Context}, _From, State) ->
    Pid = spawn_link(Module, Func, [Args, Context]),
    {reply, Pid, State};

handle_call({ensure_page_session, Context}, _From, Session) ->
    Context1  = zp_context:ensure_qs(Context),
    PageId    = zp_context:get_q(?SESSION_PAGE_Q, Context1),
    NewPageId = case PageId of
                    undefined -> zp_ids:id();
                    Id -> Id
                end,
    {NewPage, Session1} = case find_page(NewPageId, Session) of
                            undefined -> 
                                % Make a new page for this pid
                                P = page_start(NewPageId, Session#session.now),
                                {P, store_page(NewPageId, P, Session)};
                            P -> 
                                % Update the keepalive timestamp on the record
                                P1 = page_keepalive(P, Session#session.now),
                                {P1, store_page(NewPageId, P1, Session)}
                          end,
    Context2 = Context1#context{page_id=NewPageId, page_pid=NewPage#page.page_pid},
    {reply, Context2, Session1}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, Session) ->
    FIsUp = fun(Page) -> Page#page.page_pid /= Pid end,
    Pages = lists:filter(FIsUp, Session#session.pages),
    {noreply, Session#session{pages=Pages}};
handle_info(_, Session) ->
    {noreply, Session}.

terminate(_Reason, _Session) ->
    ok.

code_change(_OldVsn, Session, _Extra) ->
    {ok, Session}.


%% @doc Initialize a new session record
new_session(Args) ->
    Now = proplists:get_value(now, Args),
    #session{
            vars=dict:new(),
            now=Now,
            expire=Now + ?SESSION_EXPIRE_1,
            timer_ref=undefined,
            pages=[]
            }.

%% @doc Return a new page record, monitor the started page process because we want to know about normal exits
page_start(PageId, Now) ->
    {ok,PagePid} = zp_session_page:start_link(),
    erlang:monitor(process, PagePid),
    #page{ expire=Now + ?SESSION_PAGE_TIMEOUT, page_pid=PagePid, page_id=PageId }.

%% @doc Find the page record in the list of known pages
find_page(undefined, _Session) ->
    undefined;
find_page(PageId, Session) ->
    case lists:keysearch(PageId, ?PAGEID_OFFSET, Session#session.pages) of
        {value, Page} -> Page;
        false -> undefined
    end.

%% @doc Store the page in the session, replace an old page
store_page(PageId, Page, Session) ->
    Pages = lists:keystore(PageId, ?PAGEID_OFFSET, Session#session.pages, Page),
    Session#session{pages=Pages}.
    
%% @doc Remove the page from the session
delete_page(PageId, Session) ->
    Pages = lists:keydelete(PageId, ?PAGEID_OFFSET, Session#session.pages),
    Session#session{pages=Pages}.

    
%% @doc Reset the page timeout
page_keepalive(Page, Now) ->
    Page#page{expire=Now + ?SESSION_PAGE_TIMEOUT}.

%% @doc Check with the page if it is busy, if so then we shouldn't expire it. Called from lists:foldl/3.
try_stop(Page, {Acc,Now}) ->
    case Page#page.page_pid of
        undefined ->
            {Acc,Now};
        Pid ->
            case zp_session_page:get_attach_state(Pid) of
                attached ->
                    Page1  = Page#page{expire=Now + ?SESSION_PAGE_TIMEOUT},
                    {[Page1|Acc], Now};
                {detached, LastDetach} ->
                    Expire = LastDetach + ?SESSION_PAGE_TIMEOUT,
                    if
                        Expire >= Now ->
                            Page1 = Page#page{expire=Expire},
                            {[Page1|Acc], Now};
                        true ->
                            zp_session_page:stop(Pid),
                            {Acc,Now}
                    end;
                error ->
                    {Acc,Now}
            end
    end.

