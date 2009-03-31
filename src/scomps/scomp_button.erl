%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus

-module(scomp_button).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/3]).

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

render(Params, Context, _State) ->
    Postback  = proplists:get_value(postback, Params),
    Text      = proplists:get_value(text, Params, <<"Submit">>),
    Id        = zp_ids:optid(proplists:get_value(id, Params)),
    Class     = [button | proplists:get_all_values(class, Params)],
    Style     = proplists:get_value(style, Params),
    Actions   = proplists:get_all_values(action, Params),

    Options   = [{action,X} || X <- Actions],
    Options1  = case Postback of
                	undefined -> Options;
                	Postback  -> [{postback,Postback} | Options]
                end,

    Context1 = case Options1 of
                    [] -> Context;
                    _  -> zp_render:wire(Id, {event,[{type,click}|Options1]}, Context)
               end,
    
    Context2 = zp_tags:render_tag(
                        <<"button">>,
                        [
                    		{<<"id">>,    Id},
                    		{<<"name">>,  Id},
                    		{<<"class">>, Class},
                    		{<<"style">>, Style}
                    	],
                    	Text,
                    	Context1),
    {ok, Context2}.

