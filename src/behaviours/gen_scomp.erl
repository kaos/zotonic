%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell

%% @doc Scomp behaviour definition.  A scomp is a screen component that can optionally be cached.
%%      
%%      init(Args) -> {ok, State} | {error, Error}
%%      render(Params, Vars, Context, State) -> {ok, NewContext} | {ok, io_list()} | {error, Error}
%%      code_change(OldVsn, State, Extra) -> {ok, NewState}
%%      terminate(Reason) -> ok
%%      
%%      	State = term()
%%      	Params = proplist()
%%      	Context = context()
%%      
%%      varies(Params, Context) -> {EssentialParams, MaxAge, Varies} | undefined
%%      
%%      	Params = proplist()
%%      	MaxAge = integer()
%%          Varies = TermList


-module(gen_scomp).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [
        {init,1},
        {render, 4},
        {code_change, 3},
        {terminate, 1},
        {varies, 2}
     ];
behaviour_info(_Other) ->
    undefined.
