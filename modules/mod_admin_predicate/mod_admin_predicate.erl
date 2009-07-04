%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-07-02
%%
%% @doc Support for editing predicates in the admin module.  Also hooks into the rsc update function to
%% save the specific fields for predicates

-module(mod_admin_predicate).
-author("Marc Worrell <marc@worrell.nl>").
-behaviour(gen_server).

-mod_title("Zophrenic Predicate Administration").
-mod_description("Adds support for editing predicates to the admin.").
-mod_prio(600).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    rsc_update/3,
    predicate_flush/2
]).

-include_lib("zophrenic.hrl").

-record(state, {context}).


%% @doc Check if the update contains information for a predicate.  If so then update
%% the predicate information in the db and remove it from the update props.
%% @spec rsc_update({rsc_update, ResourceId, OldResourceProps}, UpdateProps, Context) -> NewUpdateProps
rsc_update({rsc_update, Id, _OldProps}, Props, Context) ->
    case       proplists:is_defined(predicate_subject, Props) 
        orelse proplists:is_defined(predicate_object, Props) of

        true ->
            Subjects = proplists:get_all_values(predicate_subject, Props),
            Objects  = proplists:get_all_values(predicate_object, Props),
            m_predicate:update_noflush(Id, Subjects, Objects, Context),

            proplists:delete(predicate_subject, 
                proplists:delete(predicate_object, Props));
        false ->
            Props
    end.

%% @doc Whenever a predicate has been updated we have to flush the predicate cache.
predicate_flush({rsc_update_done, _UpdateAction, _Id, BeforeCatList, CatList}, Context) ->
    case lists:member(predicate, CatList) orelse lists:member(predicate, BeforeCatList) of
        true -> m_predicate:flush(Context);
        false -> ok
    end.


%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() -> 
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    process_flag(trap_exit, true),
    {context, Context} = proplists:lookup(context, Args),
    zp_notifier:observe(rsc_update,      {?MODULE, rsc_update},      Context),
    zp_notifier:observe(rsc_update_done, {?MODULE, predicate_flush}, Context),
    {ok, #state{context=zp_context:new_for_host(Context)}}.


%% @spec handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%% @doc Trap unknown calls
handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @doc Trap unknown casts
handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.



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
terminate(_Reason, State) ->
    zp_notifier:detach(rsc_update,      {?MODULE, rsc_update},      State#state.context),
    zp_notifier:detach(rsc_update_done, {?MODULE, rsc_update_done}, State#state.context),
    zp_notifier:detach(rsc_insert_done, {?MODULE, rsc_update_done}, State#state.context),
    zp_notifier:detach(rsc_delete_done, {?MODULE, rsc_update_done}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

