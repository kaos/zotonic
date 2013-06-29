%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009-2010 Marc Worrell
%%
%% @doc Module for rendering and caching scomps.  Scomps can be caching and
%%      non caching, depending on the passed arguments and the results of the
%%      scomp's varies/2 function.

%% Copyright 2009-2010 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(z_scomp).
-author("Marc Worrell <marc@worrell.nl>").

-export([render/3, render/4, render_all/4]).

-include_lib("zotonic.hrl").


render(ScompName, [{'$render_variables', Vars}|Args], Context) ->
    case {ztl_tags:builtin(ScompName), Args} of
        {F, [{'$all', false}|BuiltinArgs]}
          when is_function(F, 3) ->
            render_scomp_module(F, BuiltinArgs, Vars, Context, {vary, nocache});
        {undefined, _} ->
            render(ScompName, Args, Vars, Context)
    end.

render_all(ScompName, Args, Vars, Context) ->
    render(ScompName, [{'$all', true}|Args], Vars, Context).

%% @spec render(ScompName, Args, Vars, Context) -> {ok, Context} | {ok, io_list} | {error, Reason}
%% @doc Render the names scomp, Args are the scomp arguments and Vars are the variables given to the template
render(ScompName, [{'$all', All}|Args], Vars, Context) ->
    Finder = case All of
                 true -> find_all;
                 false -> find
             end,
    case z_module_indexer:Finder(scomp, ScompName, Context) of
        {ok, #module_index{erlang_module=ModuleName}} ->
            render_scomp_module(ModuleName, Args, Vars, Context);
        {error, enoent} ->
            %% No such scomp, as we can switch on/off functionality we do a quiet skip
            ?LOG("custom tag \"~p\" not found", [ScompName]),
            {ok, <<>>};
        [] -> [];
        Modules when is_list(Modules) ->
            [begin
                 {ok, Result} = render_scomp_module(ModuleName, Args, Vars, Context),
                 Result
             end || #module_index{erlang_module=ModuleName} <- Modules]
    end;
render(ScompName, Args, Vars, Context) ->
    render(ScompName, [{'$all', false}|Args], Vars, Context).

module_to_render_fun(ModuleName) ->
    fun (Args, Vars, Context) ->
            ModuleName:render(Args, Vars, Context)
    end.

render_scomp_module(ModuleName, Args, Vars, Context) ->
    %% from R15, fun ModuleName:render/3 will do just fine..
    render_scomp_module(module_to_render_fun(ModuleName), Args, Vars, Context, ModuleName).

render_scomp_module(RenderFun, Args, Vars, Context, Cache) ->
    ScompContext = z_context:prune_for_scomp(z_acl:args_to_visible_for(Args), Context), 
    ScompContextWM = ScompContext#context{wm_reqdata=Context#context.wm_reqdata},
    case vary(Cache, Args, ScompContext) of
        nocache ->
            case RenderFun(Args, Vars, ScompContextWM) of
                {ok, _}=Result -> Result;
                {error, Reason} -> throw({error, Reason})
            end;
        {CachKeyArgs, MaxAge, Varies} ->
            Key = key(Cache, CachKeyArgs, ScompContextWM),
            MemoRenderFun =  fun() ->
                                     case RenderFun(Args, Vars, ScompContextWM) of
                                         {ok, _}=Result -> Result;
                                         {error, Reason} -> throw({error, Reason})
                                     end
                             end,
            z_depcache:memo(MemoRenderFun, Key, MaxAge, Varies, Context)
    end.


%% @doc Create an unique key for the scomp and the visibility level it is rendered for
%% @spec key(atom(), proplist(), context()) -> term()
key(ScompName, EssentialParams, Context) ->
    {ScompName, EssentialParams, z_acl:cache_key(Context), Context#context.language}.


%% @doc Check how and if the scomp wants to be cached.
vary({vary, Vary}, _Args, _ScompContext) -> Vary;
vary(ModuleName, Args, ScompContext) ->
    case ModuleName:vary(Args, ScompContext) of
        default ->
            %% Scomp asks default behaviour, check the arguments for caching args
            MaxAge = proplists:get_value(maxage, Args),
            case z_convert:to_integer(MaxAge) of
                undefined -> 
                    nocache; 
                Max ->
                    Vary  = proplists:get_all_values(vary, Args),
                    Args1 = proplists:delete(maxage, Args),
                    Args2 = proplists:delete(vary, Args1),
                    {Args2, Max, Vary}
            end;
        Other ->
            Other
    end.
