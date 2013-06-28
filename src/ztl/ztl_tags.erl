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
-module(ztl_tags).
-author('Andreas Stenius <kaos@astekk.se>').

%% API
-export([
         builtin/1
        ]).

-include_lib("zotonic.hrl").

builtin(image) -> image(tag);
builtin(image_url) -> image(url);
builtin(_) -> undefined.


image(F) ->
    fun ([Filename|Args], _Vars, Context) ->
            z_media_tag:F(Filename, Args, Context)
    end.
