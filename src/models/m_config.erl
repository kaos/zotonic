%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-09
%%
%% @doc Model for the zophrenic config table

-module(m_config).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    all/1,
    get/2,
    get/3,
    get_value/3,
    set_value/4,
    set_prop/5,
    delete/3,
    get_id/3
]).

-include_lib("zophrenic.hrl").

%% @doc Return all configurations gives a nested proplist (module, key)

all(Context) ->
    case zp_depcache:get(config) of
        {ok, Cs} ->
            Cs;
        undefined ->
            Cs = zp_db:assoc_props("select * from config order by module, key", Context),
            Indexed = [ {M, zp_utils:index_proplist(key, CMs)} || {M,CMs} <- zp_utils:group_proplists(module, Cs) ],
            zp_depcache:set(config, Indexed, ?DAY),
            Indexed
    end.


get(Module, Context) ->
    case zp_depcache:get(config, Module) of
        {ok, undefined} ->
            undefined;
        {ok, Cs} ->
            Cs;
        undefined ->
            All = all(Context),
            proplists:get_value(Module, All)
    end.

get(Module, Key, Context) ->
    case zp_depcache:get(config, Module) of
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

get_value(Module, Key, Context) ->
    case get(Module, Key, Context) of
        undefined -> undefined;
        Cfg -> proplists:get_value(value, Cfg)
    end.

set_value(Module, Key, Value, Context) ->
    case zp_db:q("update config set value = $1 where module = $2 and key = $3", [Value, Module, Key], Context) of
        0 -> zp_db:insert(config, [{module,Module}, {key, Key}, {value, Value}], Context);
        1 -> ok
    end,
    zp_depcache:flush(config).


set_prop(Module, Key, Prop, PropValue, Context) ->
    case get_id(Module, Key, Context) of
        undefined -> zp_db:insert(config, [{module,Module}, {key,Key}, {Prop,PropValue}], Context);
        Id -> zp_db:update(config, Id, [{Prop,PropValue}], Context)
    end,
    zp_depcache:flush(config).

delete(Module, Key, Context) ->
    zp_db:q("delete from config where module = $1 and key = $2", [Module, Key], Context),
    zp_depcache:flush(config).


get_id(Module, Key, Context) ->
    zp_db:q1("select id from config where module = $1 and key = $2", [Module, Key], Context).    


