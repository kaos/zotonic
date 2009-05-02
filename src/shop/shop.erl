%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-11
%%
%% @doc Basic routines for shop functionality



-module(shop).
-author("Marc Worrell <marc@worrell.nl").

-author("Marc Worrell <marc@worrell.nl").
-behaviour(gen_server).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, start_link/1]).

%% interface functions
-export([
    category_brands/2,
    category_subcat_bybrand/3,
    category_rsc_count/2
]).

-include_lib("zophrenic.hrl").

%%====================================================================
%% API
%%====================================================================
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server

%% @doc Start the shop processes. Only a periodic Adyen notification checker is needed (should make that into a server...)
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
init(_Args) ->
    % Every 10 minutes a periodic check of Adyen logs and old orders
    timer:send_interval(60 * 1000 * 10, periodic),
    {ok, []}.

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
handle_info(periodic, State) ->
    Context = zp_context:new(),
    % Check if there are any unhandled payment notifications
    shop_adyen:periodic_log_check(Context),
    % Check if there are expired orders
    expire_orders(Context),
    {noreply, State};
    
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


%% @doc Deallocate all orders that are past their expire time
expire_orders(Context) ->
    Ids = zp_db:q("select id from shop_order where expire > now() and status = 'new'", Context),
    F = fun(Ctx) ->
        [ shop_order:set_status(Id, canceled, Ctx) || {Id} <- Ids ]
    end,
    zp_db:transaction(F, Context).
    
