%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-13
%%
%% @doc Fill the small shopping cart scomp, including the total nr of products and the total costs

-module(scomp_shop_shop_cart_small).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zophrenic.hrl").

%%      init(Args) -> {ok, State} | {error, Error}
%%      render(Params, Context, State) -> {ok, NewContext} | {error, Error}
%%      code_change(OldVsn, State, Extra) -> {ok, NewState}
%%      terminate(Reason) -> ok
%%      
%%      	State = term()
%%      	Params = proplist()
%%      	Context = context()
%%      
%%      varies(Params, Context) -> {EssentialParams, MaxAge, Depends} | undefined
%%      
%%      	Params = proplist()
%%      	MaxAge = integer()
%%          Depends = TermList

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, _Vars, Context, _State) ->
    Total = proplists:get_value(total, Params),
    Count = proplists:get_value(count, Params),
    
    Context1 = case is_float(Total) andalso is_integer(Count) of
        true ->  shop_cart:tpl_sync_cart_info(Total, Count, Context);
        false -> shop_cart:tpl_sync_cart_info(Context)
    end,
    {ok, Context1}.
