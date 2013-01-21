%% @author Andreas Stenius <git@astekk.se>
%% @copyright 2013 Andreas Stenius
%% Date: 2013-01-20
%%
%% @doc Model for accessing the session manager from a template.

%% Copyright 2013 Andreas Stenius
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

-module(m_session_manager).
-author("Andreas Stenius <git@astekk.se").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()

%% Session manager keys
m_find_value(session_count, #m{value=undefined}, Context) ->
    z_session_manager:count(Context);

%% Session keys
m_find_value(session, #m{value=undefined}=M, _Context) ->
    M#m{value=session};
m_find_value(Session, #m{value=session}=M, _Context) ->
    M#m{value={z_session, Session}};
m_find_value(pages, #m{value={z_session, Pid}}, _Context) ->
    z_session:get_pages(Pid);

%% Page keys
m_find_value(page, #m{value=undefined}=M, _Context) ->
    M#m{value=page};
m_find_value(Page, #m{value=page}=M, _Context) ->
    M#m{value={z_session_page, Page}};

%% Lookup session prop
m_find_value(Key, #m{value={SessionMod, Pid}}=M, _Context) ->
    SessionMod:get(Key, Pid).


%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{value=undefined}, Context) ->
	z_session_manager:fold(fun (Session, Acc) -> [Session|Acc] end, [], Context);
m_to_list(#m{value={SessionMod, Pid}}, _Context) ->
    SessionMod:get_all(Pid).


%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, _Context) ->
	undefined.

