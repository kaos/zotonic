%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-24
%%
%% @doc Update routines for resources.  For use by the m_rsc module.

-module(m_rsc_update).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    insert/2,
    delete/2,
    update/3
]).

-include_lib("zophrenic.hrl").


%% @doc Insert a new resource
%% @spec insert(Props, Context) -> {ok, Id}
insert(Props, Context) ->
    zp_db:insert(rsc, Props, Context).


%% @doc Delete a resource
%% @spec delete(Props, Context) -> void()
delete(Id, Context) when is_integer(Id) ->
    case zp_acl:rsc_editable(Id, Context) of
        true ->
            zp_db:delete(rsc, Id, Context),
            zp_depcache:flush(#rsc{id=Id}),
            ok;
        false ->
            {error, eacces}
    end.
    

%% @doc Update a predicate
%% @spec update(Props, Props, Context) -> void()
update(Id, Props, Context) when is_integer(Id) ->
    case zp_acl:rsc_editable(Id, Context) of
        true ->
            zp_db:update(rsc, Id, Props, Context),
            zp_depcache:flush(#rsc{id=Id}),
            ok;
        false ->
            {error, eacces}
    end.




valid_fields() ->
    [
        title,
        intro,
        body,
        seo_noindex,
        seo_title,
        seo_keywords,
        seo_desc
    ].
