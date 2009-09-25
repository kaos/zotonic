%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-09
%%
%% @doc Model for the zotonic config table

-module(m_config).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    all/1,
    get/2,
    get/3,
    get_value/3,
    get_value/4,
    set_value/4,
    set_prop/5,
    delete/3,
    get_id/3
]).

-include_lib("zotonic.hrl").

%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Module, #m{value=undefined} = M, _Context) ->
    M#m{value=Module};
m_find_value(Key, #m{value=Module}, Context) ->
    get(Module, Key, Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(#m{value=undefined}, Context) ->
    all(Context);
m_to_list(#m{value=Module}, Context) ->
    get(Module, Context).

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=undefined}, Context) ->
    all(Context);
m_value(#m{value=Module}, Context) ->
    get(Module, Context).
    

%% @doc Return all configurations gives a nested proplist (module, key)

all(Context) ->
    case z_depcache:get(config, Context) of
        {ok, Cs} ->
            Cs;
        undefined ->
            Cs = z_db:assoc_props("select * from config order by module, key", Context),
            Indexed = [ {M, z_utils:index_proplist(key, CMs)} || {M,CMs} <- z_utils:group_proplists(module, Cs) ],
            z_depcache:set(config, Indexed, ?DAY, Context),
            Indexed
    end.


get(Module, Context) when is_atom(Module) ->
    case z_depcache:get(config, Module, Context) of
        {ok, undefined} ->
            undefined;
        {ok, Cs} ->
            Cs;
        undefined ->
            All = all(Context),
            proplists:get_value(Module, All)
    end.

get(Module, Key, Context) when is_atom(Module) andalso is_atom(Key) ->
    case z_depcache:get(config, Module, Context) of
        {ok, undefined} ->
            undefined;
        {ok, Cs} ->
            proplists:get_value(Key, Cs);
        undefined ->
            All = all(Context),
            case proplists:get_value(Module, All) of
                undefined -> undefined;
                Cs -> proplists:get_value(Key, Cs)
            end
    end.

get_value(Module, Key, Context) when is_atom(Module) andalso is_atom(Key) ->
    case get(Module, Key, Context) of
        undefined -> undefined;
        Cfg -> proplists:get_value(value, Cfg)
    end.

get_value(Module, Key, Default, Context) when is_atom(Module) andalso is_atom(Key) ->
    case get_value(Module, Key, Context) of
        undefined -> Default;
        Value -> Value
    end.


set_value(Module, Key, Value, Context) ->
    case z_db:q("update config set value = $1, modified = now() where module = $2 and key = $3", [Value, Module, Key], Context) of
        0 -> z_db:insert(config, [{module,Module}, {key, Key}, {value, Value}], Context);
        1 -> ok
    end,
    z_depcache:flush(config, Context).


set_prop(Module, Key, Prop, PropValue, Context) ->
    case get_id(Module, Key, Context) of
        undefined -> z_db:insert(config, [{module,Module}, {key,Key}, {Prop,PropValue}], Context);
        Id -> z_db:update(config, Id, [{Prop,PropValue}], Context)
    end,
    z_depcache:flush(config, Context).

delete(Module, Key, Context) ->
    z_db:q("delete from config where module = $1 and key = $2", [Module, Key], Context),
    z_depcache:flush(config, Context).


get_id(Module, Key, Context) ->
    z_db:q1("select id from config where module = $1 and key = $2", [Module, Key], Context).    


