%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @doc Include a template, with possible caching
%%
%%      Example: include "some_file.tpl" and cache it for 3600 seconds
%%      {% @include depend="something" maxage=3600 file="some_file.tpl" %}
%%
%%      Give a maxage of 0 for slam dunk protection but no caching.
%%

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

-module(scomp_include).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/3]).

-include("zophrenic.hrl").

init(_Args) -> {ok, []}.
varies(Params, _Context) -> 
    MaxAge = proplists:get_value(maxage, Params),
    case zp_convert:to_integer(MaxAge) of
        undefined -> 
            undefined; 
        Max ->
            Vary    = proplists:get_all_values(vary, Params),
            Params1 = proplists:delete(maxage, Params),
            Params2 = proplists:delete(vary, Params1),
            {Params2, Max, Vary}
    end.

code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, Context, _State) ->
    File = proplists:get_value(file, Params),
    
    AddC =  fun 
                ({Name,Value}, Ctx) when Name =/= file andalso Name =/= depend andalso Name =/= maxage ->
                    zp_context:set_context(Name, Value, Ctx);
                (_, Ctx) -> 
                    Ctx
            end,
    Context1 = lists:foldl(AddC, Context, Params),
    {ok, zp_template:render(File, Context1)}.

