%%%-------------------------------------------------------------------
%%% @author Marc Worrell <marc@worrell.nl>
%%% @copyright 2009 Marc Worrell
%%%
%%% @doc Server for rendering and caching scomps.  Scomps can be caching and
%%%      non caching, depending on the result of the render/2 function.
%%% 
%%% @todo Handle code change events for all scomps     
%%%-------------------------------------------------------------------

-module(zp_scomp).

-behaviour(gen_server).
-author("Marc Worrell <marc@worrell.nl>").

%% API
-export([start_link/0, render/3, scomp_ready/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("zophrenic.hrl").


%% @doc The state of the scomp server.  'Cache' holds the cached entries, 'waiting' holds the list
%%      of entries being rendered for the cache.  'Now' is the current time in seconds, updated with 
%%      a timer to prevent too many time lookups.
%% @todo do performance checks if we want to use ets for this, nice with dicts is that the content is not copied
-record(state, {waiting, scomps}).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @spec render(ScompName, Args, Vars) -> {ok, Context} | {ok, io_list} | {error, Reason}
%% @doc Render the names scomp, Args are the scomp arguments and Vars are the variables given to the template
render(ScompName, Args, Context) ->
    CleanedContext = zp_context:cleanup_for_scomp(Context),
    case gen_server:call(?MODULE, {render, ScompName, Args, CleanedContext}) of
        {renderer, M, F, RenderArgs} ->
            erlang:apply(M, F, RenderArgs);
        Result -> Result
    end.


%% @spec scomp_ready(Key, Result, MaxAge, Depends) -> none()
%% @doc Called when a cacheable scomp has been rendered, places the scomp in the cache and
%%      posts the result to all waiting processes
scomp_ready(Key, Result, MaxAge, Depends) ->
    gen_server:cast(?MODULE, {scomp_ready, Key, Result, MaxAge, Depends}).


%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    timer:start(),
    {ok, #state{waiting=dict:new(), scomps=dict:new()}}.


%%--------------------------------------------------------------------
%% Function: handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% @doc Render a scomp. Args are the scomp parameters, Context is the request context
%%      at the moment of the call to the template engine.  The scomp is a module that
%%      must be present in the ebin directory. 
handle_call({render, ScompName, Args, Context}, From, State) ->
    ScompDepends = ScompName:depends(Args, Context),
    {Cached, State1} =  case ScompDepends of
                            undefined -> 
                                {undefined, State};
                            {Essential, Max, Dep} ->
                                case Max of
                                    0 -> {undefined, State};
                                    _ -> {cache_lookup(ScompName, Essential, Dep), State}
                                end
                        end,
    case Cached of 
        undefined ->
            case scomp_state(ScompName, State1) of
                {error, Error} ->
                    Err = io_lib:format("Error initializing scomp ~p: ~p", [ScompName, Error]),
                    {reply, Err, State1};

                {ok, ScompState, State2} ->
                    %% No cached entry, we need to redo the scomp.
                    %% When the scomp is cacheable we record it to prevent parallel rendering
                    %% When the scomp is not cacheable we just return the renderer to be evaluated in the process of the caller
                    case ScompDepends of
                        undefined ->
                            {reply, {renderer, ScompName, render, [Args, Context, ScompState]}, State2};
                        {EssentialParams, MaxAge, Depends} ->
                            StateSpawned = spawn_cacheable_renderer(ScompName, ScompState, EssentialParams, MaxAge, Depends, Context, From, State2),
                            {noreply, StateSpawned}
                    end
            end;
        CachedResult ->
            %% Cached content, return the html and context
            %% The context must be filtered from the returned html before serving
            {reply, CachedResult, State1}
    end.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

%% @desc Store the rendered scomp in the cache, send the result to all waiting processes
handle_cast({scomp_ready, Key, Result, MaxAge, Depends}, State) ->
    case dict:find(Key, State#state.waiting) of
        {ok, List} -> lists:foreach(fun(Pid) -> catch gen_server:reply(Pid, Result) end, List);
        _ -> ok
    end,

    %% If maxage is 0 then there is slam-dunk protection but no additional caching.
    State1 = State#state{ waiting=dict:erase(Key, State#state.waiting) },
    case zp_convert:to_integer(MaxAge) of
        undefined -> 
            {noreply, State1};
        0 -> 
            {noreply, State1};
        Max ->
            zp_depcache:set(Key, Result, Max, Depends),
            {noreply, State1}
    end;
    
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({From, Tag, get_modules}, State) ->
    From ! {element(2,Tag), scomp_list(State)},
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

cache_lookup(_ScompName, undefined, _Depends) ->
    undefined;
cache_lookup(ScompName, EssentialParams, _Depends) ->
    case zp_depcache:get({ScompName,EssentialParams}) of
        {ok, Result} -> Result;
        _ -> undefined
    end.


%% @doc Render a scomp where we can cache the result.  The scomp is rendered and reported
%%      back to this scomp server, which then replies all callee processes that are waiting and
%%      caches the result.
spawn_cacheable_renderer(ScompName, ScompState, EssentialParams, MaxAge, Depends, Context, From, State) ->
    Key = {ScompName, EssentialParams},
    Waiting = try
                dict:append(Key, From, State#state.waiting)
             catch
                _M:_E -> dict:store(Key, [From], State#state.waiting)
             end,
    Renderer = fun() ->
                     Result = ScompName:render(EssentialParams, Context, ScompState),
                     zp_scomp:scomp_ready(Key, Result, MaxAge, Depends)
                end,
    spawn(Renderer),
    State#state{waiting=Waiting}.
 
 
%% @doc Make sure that the scomp module is registered with the list of modules for which we want to receive code change events
scomp_state(ScompName, State) ->
    case dict:find(ScompName, State#state.scomps) of
        {ok, ScompState} ->  
            {ok, ScompState, State};
        _ ->
            case ScompName:init([]) of
                {ok, ScompState} ->
                    Dict2 = dict:store(ScompName, ScompState, State#state.scomps),
                    {ok, ScompState, State#state{scomps=Dict2}};
                {error, Error} ->
                    {error, Error}
            end
    end.

%% @doc Return the list of loaded scomps
scomp_list(State) ->
    dict:fetch_keys(State#state.scomps).

    