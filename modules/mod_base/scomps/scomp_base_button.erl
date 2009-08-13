%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus

-module(scomp_base_button).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zotonic.hrl").


%%      init(Args) -> {ok, State} | {error, Error}
%%      render(Params, Context, State) -> {ok, NewContext} | {ok, iolist()} | {error, Error}
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
    Postback  = proplists:get_value(postback, Params),
    Text      = proplists:get_value(text, Params, <<"Submit">>),
    Id        = z_ids:optid(proplists:get_value(id, Params)),
    %%Class     = [button | proplists:get_all_values(class, Params)],
    Class     = proplists:get_all_values(class, Params),
    Style     = proplists:get_value(style, Params),
    Type      = proplists:get_value(type, Params),
    Title     = proplists:get_value(title, Params),
    Disabled  = proplists:get_value(disabled, Params, false),
    Actions   = proplists:get_all_values(action, Params),

    Options   = [{action,X} || X <- Actions],
    Options1  = case Postback of
                	undefined -> Options;
                	Postback  -> [{postback,Postback} | Options]
                end,

    Context1 = case Options1 of
                    [] -> Context;
                    _  -> z_render:wire(Id, {event,[{type,click}|Options1]}, Context)
               end,

    Attrs = [
        {<<"id">>,    Id},
        {<<"name">>,  case proplists:is_defined(id, Params) of true -> Id; false -> "" end},
        {<<"style">>, Style},
        {<<"title">>, Title}
    ],
    
    {Class1, Attrs1} = case z_convert:to_bool(Disabled) of
        false -> {Class, Attrs};
        true -> { ["disabled"|Class], [ {<<"disabled">>,"disabled"}|Attrs] }
    end,
    
    Attrs2 = case Type of
        undefined -> Attrs1;
        _ -> [ {<<"type">>, Type} | Attrs1 ]
    end,
    
    Context2 = z_tags:render_tag(
                        <<"button">>,
                        [{<<"class">>,Class1}|Attrs2],
                    	Text,
                    	Context1),
    {ok, Context2}.

