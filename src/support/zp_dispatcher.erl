%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% @doc Manage dispatch lists (aka definitions for url patterns). Constructs named urls from dispatch lists.

-module(zp_dispatcher).
-author("Marc Worrell <marc@worrell.nl>").

-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% zp_dispatch exports
-export([url_for/1, url_for/2, url_for/3, reload/0, test/0]).

-record(state, {dispatchlist=undefined, lookup=undefined}).

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the dispatch server
start_link() ->
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).


%% @spec uri_for(atom()) -> iolist()
%% @doc Construct an uri from a named dispatch, assuming no parameters
url_for(Name) ->
    gen_server:call(?MODULE, {'url_for', Name, [], html}).


%% @spec uri_for(atom(), Args) -> iolist()
%%        type Args = PropList
%% @doc Construct an uri from a named dispatch and the parameters
url_for(Name, Args) ->
    gen_server:call(?MODULE, {'url_for', Name, Args, html}).


%% @spec uri_for(atom(), Args) -> iolist()
%%        type Args = PropList
%% @doc Construct an uri from a named dispatch and the parameters
url_for(Name, Args, Escape) ->
    gen_server:call(?MODULE, {'url_for', Name, Args, Escape}).


%% @doc Reload all dispatch lists.  Finds new dispatch lists and adds them to the dispatcher
reload() ->
    gen_server:cast(?MODULE, 'reload').


%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server, loads the dispatch list into the webmachine dispatcher
init(_Args) ->
    State  = #state{dispatchlist=[], lookup=dict:new()},
    State1 = reload_dispatch_list(State),
    {ok, State1}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
handle_call({'url_for', Name, Args, Escape}, _From, State) ->
    Uri = make_url_for(Name, Args, Escape, State#state.lookup),
    {reply, Uri, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Handling cast messages
handle_cast('reload', State) ->
    State1 = reload_dispatch_list(State),
    {noreply, State1}.


%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.


%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


%% @doc Reload the dispatch list and send it to the webmachine dispatcher.
reload_dispatch_list(State) ->
    DispatchList = collect_dispatch_lists(),
    LookupDict   = dispatch_for_uri_lookup(DispatchList),
    dispatch_webmachine(DispatchList),
    State#state{dispatchlist=DispatchList, lookup=LookupDict}.


%% @doc Collect all dispatch lists.  Checks priv/dispatch for all dispatch list definitions.
collect_dispatch_lists() ->
    Files1     = filelib:wildcard("default/dispatch/*"),
    Files2     = filelib:wildcard("priv/dispatch/*"),
    Resources1 = filelib:wildcard("src/resources/resource_*.erl"),
    Resources2 = filelib:wildcard("priv/resources/resource_*.erl"),
    Modules    = [list_to_atom(filename:basename(X, ".erl")) || X <- Resources1 ++ Resources2],
    Dispatch   = [
                    lists:map(fun get_module_dispatch/1, Modules),
                    lists:map(fun get_file_dispatch/1, Files2++Files1)
                 ],
    lists:flatten(Dispatch).
    

%% @doc Set the dispatch list of the webmachine dispatcher.
dispatch_webmachine(DispatchList) ->
    WMList = [list_to_tuple(tl(tuple_to_list(Disp))) || Disp <- DispatchList],
    webmachine_dispatcher:set_dispatch_list(WMList).
   

%% @doc Fetch a dispatch list from a module (if the module exports dispatch/0)
get_module_dispatch(Mod) ->
    Exports = Mod:module_info(exports),
    case proplists:is_defined(dispatch, Exports) of
        true -> Mod:dispatch();
        false -> []
    end.

%% @doc Read a dispatch file, the file should contain a valid Erlang dispatch datastructure.
get_file_dispatch(File) ->
    case filelib:is_regular(File) of
        true ->
            Basename = filename:basename(File),
            case Basename of
                "." ++ _ -> 
                    [];
                _Other  ->
                    {ok, Disp} = file:consult(File),
                    Disp
            end;
        false -> 
            []
    end.


%% @doc Transform the dispatchlist into a datastructure for building uris from name/vars
%% Datastructure needed is:   name -> [vars, pattern]
dispatch_for_uri_lookup(DispatchList) ->
    dispatch_for_uri_lookup1(DispatchList, dict:new()).
    
dispatch_for_uri_lookup1([], Dict) ->
    Dict;
dispatch_for_uri_lookup1([{Name, Pattern, _Resource, _Args}|T], Dict) ->
    Vars  = lists:filter(fun erlang:is_atom/1, Pattern),
    Dict1 = case dict:is_key(Name, Dict) of
                true  -> dict:append(Name, {length(Vars), Vars, Pattern}, Dict);
                false -> dict:store(Name, [{length(Vars), Vars, Pattern}], Dict)
            end,
    dispatch_for_uri_lookup1(T, Dict1).



%% @doc Make an uri for the named dispatch with the given parameters
make_url_for(Name, Args, Escape, UriLookup) ->
    Name1 = zp_convert:to_atom(Name),
    Args1 = lists:filter(fun
            ({_, <<>>}) -> false;
            ({_, []}) -> false;
            ({_, undefined}) -> false;
            (_) -> true
        end, Args),
    case dict:find(Name1, UriLookup) of
        {ok, Patterns} -> make_url_for1(Args1, Patterns, Escape, undefined);
        error -> undefined
    end.


%% @doc Try to match all patterns with the arguments
make_url_for1(_Args, [], _Escape, undefined) ->
    undefined;
make_url_for1(Args, [], Escape, {QueryStringArgs, Pattern}) -> 
    ReplArgs =  fun 
                    (V) when is_atom(V) -> mochiweb_util:quote_plus(proplists:get_value(V, Args));
                    (S) -> S
                end,
    UriParts = lists:map(ReplArgs, Pattern), 
    Uri      = [$/ | zp_utils:combine($/, UriParts)],
    case QueryStringArgs of
        [] -> Uri;
        _  ->
            Sep = case Escape of
                    xml  -> "&amp;";
                    html -> "&amp;";
                    _    -> $&
                  end,
            [Uri, $?, urlencode(QueryStringArgs, Sep)]
    end;
make_url_for1(Args, [Pattern|T], Escape, Best) ->
    Best1 = select_best_pattern(Args, Pattern, Best),
    make_url_for1(Args, T, Escape, Best1).


select_best_pattern(Args, {PCount, PArgs, Pattern}, Best) ->
    if 
        length(Args) >= PCount ->
            %% Check if all PArgs are part of Args
            {PathArgs, QueryStringArgs} = lists:partition(fun({A,_}) -> lists:member(A, PArgs) end, Args),
            case length(PathArgs) of
                PCount ->
                    % Could fill all path args, this match satisfies
                    select_best_pattern1({QueryStringArgs,Pattern}, Best);
                _ ->
                    % Could not fill all path args, try other patterns
                    Best
            end;
        true ->
            Best
    end.

select_best_pattern1(A, undefined) -> 
    A;
select_best_pattern1({AQS, _APat}=A, {BQS, _BPat}=B) ->
    if 
        length(BQS) > length(AQS) -> A;
        true -> B
    end.


%% @spec urlencode([{Key, Value}]) -> string()
%% @doc URL encode the property list.
urlencode(Props, Join) ->
    RevPairs = lists:foldl(fun ({K, V}, Acc) ->
                                   [[mochiweb_util:quote_plus(K), $=, mochiweb_util:quote_plus(V)] | Acc]
                           end, [], Props),
    lists:flatten(revjoin(RevPairs, Join, [])).

revjoin([], _Separator, Acc) ->
    Acc;
revjoin([S | Rest], Separator, []) ->
    revjoin(Rest, Separator, [S]);
revjoin([S | Rest], Separator, Acc) ->
    revjoin(Rest, Separator, [S, Separator | Acc]).



% {ok, X, _}  = erl_scan:string("[{hello, blaat}].").
% {ok, Y} = erl_parse:parse_exprs(X).
% {value, Value, _NewBindings} = erl_eval:exprs(Y, [])}.

test() ->
    List = collect_dispatch_lists(),
    Dict = dispatch_for_uri_lookup(List),
    dict:to_list(Dict),

    Uri = make_url_for1(
                [{a,"A"},{b,"B"},{c,"hello&plop"},{d,"bla"}], 
                [
                {1, [a],   [a,"a"]},
                {2, [a,b], [a,"ab",b]}, 
                {2, [b,a], [b,"ba",a]} 
                ], 
                html,
                undefined),
    lists:flatten(Uri).
