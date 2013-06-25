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
-module(ztl).
-author('Andreas Stenius <kaos@astekk.se>').

%% API
-export([
         compile/3, compile/4
         %%, compile/5
        ]).

compile(FileOrBinary, Module, _ZContext) ->
    erlydtl:compile(FileOrBinary, Module).

compile(FileOrBinary, Module, Options, _ZContext) ->
    erlydtl:compile(FileOrBinary, Module, Options).

%% compile(FileOrBinary, BaseFile, Module, Options, _ZContext) ->
%%     erlydtl:compile(FileOrBinary, BaseFile, Module, Options).

