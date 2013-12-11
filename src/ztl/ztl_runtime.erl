%%%-------------------------------------------------------------------
%%%
%%% Copyright (c) 2013 Andreas Stenius <kaos@astekk.se>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%% 
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%% 
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-------------------------------------------------------------------
-module(ztl_runtime).
-author('Andreas Stenius <kaos@astekk.se>').

-export([find_value/3]).

-include("zotonic.hrl").


%%% ----------------------------------------------------------------------------
%%% Exported functions
%%% ----------------------------------------------------------------------------

find_value(Key, #m{ model=undefined }, _Options) ->
    #m{ model=z_convert:to_atom([<<"m_">>, z_convert:to_binary(Key)]) };
find_value(Key, #m{ model=Model }=M, Options) ->
    Model:m_find_value(Key, M, proplists:get_value(
                                 z_context,
                                 proplists:get_value(
                                   render_options, Options, [])));
find_value(Key, Data, Options) ->
    case find_value(Key, Data) of
        undefined ->
            erlydtl_runtime:find_value(Key, Data, Options);
        Value ->
            Value
    end.


%%% ----------------------------------------------------------------------------
%%% Internal functions
%%% ----------------------------------------------------------------------------

find_value(Key, Data) 
  when is_integer(Key), is_list(Data) ->
    index_value(Key, Data);
find_value(m, _) -> #m{};
find_value(_Key, _Data) ->
    undefined.

index_value(Key, Data)
  when Key >= 1, Key =< length(Data) ->
    lists:nth(Key, Data);
index_value(_, _) ->
    undefined.
