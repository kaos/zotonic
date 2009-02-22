%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Scomp behaviour definition.  A scomp is a screen component that can optionally be cached.
%%      
%%      init(Args) -> {ok, State} | {error, Error}
%%      render(Params, Context, State) -> {ok, NewContext} | {ok, io_list()} | {error, Error}
%%      code_change(OldVsn, State, Extra) -> {ok, NewState}
%%      terminate(Reason) -> ok
%%      
%%      	State = term()
%%      	Params = proplist()
%%      	Context = context()
%%      
%%      depends(Params, Context) -> {EssentialParams, MaxAge, Depends} | undefined
%%      
%%      	Params = proplist()
%%      	MaxAge = integer()
%%          Depends = TermList


-module(gen_scomp).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {init,1},
        {render, 3},
        {code_change, 3},
        {terminate, 1},
        {depends, 2}
     ];
behaviour_info(_Other) ->
    undefined.



%% TODO: define a minimal context for the scomp rendering (in the gen_scomp server?)

%% Add gen_xxx scomp server behaviour
%% scomps are registered using their name, which _must_ start with scomp_

%% Make this easy callable from the zp_scomp module.
%% Each scomp should be started when first called. 