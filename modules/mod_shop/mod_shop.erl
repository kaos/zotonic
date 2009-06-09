%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-06-08
%%
%% @doc A complete shop in Zophrenic

-module(mod_shop).
-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_server).

-mod_title("Shop").
-mod_description("A complete shop. Payments are done via a PSP.").
-mod_prio(100).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    observe/2,
    category_brands/2,
    category_subcat_bybrand/3,
    category_rsc_count/2
]).

-record(state, {context}).

%%====================================================================
%% API
%%====================================================================


observe({search_query, Req, OffsetLimit}, Context) ->
    shop_queries:search(Req, OffsetLimit, Context).


%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
start_link() -> 
    start_link([]).
start_link(Args) when is_list(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).



%% @doc Return the list of brands in a certain category, and with each brand the number of resources attached to that brand
%% @spec category_brands(CatId, Context) -> [ {Id,Count} ]
category_brands(Cat, Context) ->
    Id = m_category:name_to_id_check(Cat, Context),
    BrandPred = m_predicate:name_to_id_check(brand, Context),
    {Left, Right} = m_category:get_range(Id, Context),
    zp_db:q("
        select b.id, b.name, count(r.id)
        from category c
         join rsc r on r.category_id = c.id
         join edge e on e.subject_id = r.id
         join rsc b on e.object_id = b.id
        where r.is_published = true
          and r.visible_for = 0
          and c.nr >= $1
          and c.nr <= $2
          and e.predicate_id = $3
        group by b.id, b.name
        order by b.name
    ", [Left, Right, BrandPred], Context).


%% @doc Return the list of subcategories that have a product in the main category, and where the product has a certain brand
%% @spec category_brands_subcat(CatId, BrandId, Context) -> [ {Id,Count} ]
category_subcat_bybrand(CatId, undefined, Context) ->
    m_category:get_by_parent(CatId, Context);
category_subcat_bybrand(CatId, BrandId, Context) ->
    BrandPred = m_predicate:name_to_id_check(brand, Context),
    {Left, Right} = m_category:get_range(CatId, Context),
    Nrs = zp_db:q("
        select distinct c.nr
        from category c
         join rsc r on r.category_id = c.id
         join edge e on e.subject_id = r.id
        where r.is_published = true
          and r.visible_for = 0
          and c.nr >= $1
          and c.nr <= $2
          and e.predicate_id = $3
          and e.object_id = $4
    ", [Left, Right, BrandPred, BrandId], Context),
    % Select all subcats that have a lft/rght around any of the Nrs
    Nrs1 = [ N || {N} <- Nrs ],
    Sub = m_category:get_by_parent(CatId, Context),
    InRange = fun(L, R) ->
        lists:any(fun(N) -> L =< N andalso R >= N end, Nrs1)
    end,
    lists:filter(fun(Cat) -> InRange(proplists:get_value(lft, Cat), proplists:get_value(rght, Cat)) end, Sub).


%% @doc Count the number of visible resources in the category
%% @spec category_brands(CatId, Context) -> [ {Id,Count} ]
category_rsc_count(Cat, Context) ->
    Id = m_category:name_to_id_check(Cat, Context),
    {Left, Right} = m_category:get_range(Id, Context),
    zp_db:q1("
        select count(r.id)
        from category c
         join rsc r on r.category_id = c.id
        where r.is_published = true
          and r.visible_for = 0
          and c.nr >= $1
          and c.nr <= $2
    ", [Left, Right], Context).



%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore               |
%%                     {stop, Reason}
%% @doc Initiates the server.
init(Args) ->
    {context, Context} = proplists:lookup(context, Args),
    zp_notifier:observe(search_query, {?MODULE, observe}, Context),

    % Every 10 minutes a periodic check of Adyen logs and old orders
    timer:send_interval(60 * 1000 * 10, periodic),
    {ok, #state{context=zp_context:prune_for_database(Context)}}.

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

%% @doc Periodic checks on unhandled payment notifications, also expire orders that have not been paid.
handle_info(periodic, State) ->
    % Check if there are any unhandled payment notifications
    shop_adyen:periodic_log_check(State#state.context),
    % Check if there are expired orders
    expire_orders(State#state.context),
    {noreply, State};

%% @doc Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, State) ->
    zp_notifier:detach(search_query, {?MODULE, observe}, State#state.context),
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================


%% @doc Deallocate all orders that are past their expire time
expire_orders(Context) ->
    Ids = zp_db:q("select id from shop_order where expires > now() and status = 'new'", Context),
    F = fun(Ctx) ->
        [ shop_order:set_status(Id, canceled, Ctx) || {Id} <- Ids ]
    end,
    zp_db:transaction(F, Context).
    
