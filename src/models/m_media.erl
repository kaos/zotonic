%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-09
%%
%% @doc Model for media database

-module(m_media).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    get/2,
    get_acl_props/2,
    get_rsc/2,
    get_rsc/3,
    get_rsc_media/3,
    delete/2,
    insert_file/2,
    insert_file/3,
    insert_file_rsc/3,
    insert_file_rsc/4,
    insert_rsc_media/3,
    delete_rsc_media/2,
    delete_rsc_media/3
]).

-include_lib("zophrenic.hrl").



%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Id, #m{value=undefined}, Context) ->
    get(Id, Context);
m_find_value(_Key, #m{}, _Context) ->
    undefined.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context)
m_to_list(#m{}, _Context) ->
    [].
    
%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{}, _Context) ->
    undefined.



%% @doc Get the media record with the id
%% @spec get(MediaId, Context) -> PropList
get(Id, Context) ->
    F = fun() -> zp_db:assoc_props_row("select * from media where id = $1", [Id], Context) end,
    zp_depcache:memo(F, {media, Id}, ?WEEK).


%% @doc Get the ACL fields for the resource with the id. The id must be an integer
%% @spec get_acl_fields(Id, #context) -> #acl_props
get_acl_props(Id, Context) when is_integer(Id) ->
    F = fun() ->
        case zp_db:q_row("
            select visible_for, group_id 
            from media 
            where id = $1", [Id], Context) of

            {Vis, Group} ->
                #acl_props{visible_for=Vis, group_id=Group};
            false ->
                #acl_props{is_published=false, visible_for=3, group_id=0}
        end
    end,
    zp_depcache:memo(F, {media_acl_props, Id}, ?WEEK, [{media, Id}]).


%% @doc Get all media for a certain resource
%% @spec get_rsc(RscId, Context) -> [PropList]
get_rsc(RscId, Context) ->
    case zp_depcache:get({media_rsc, RscId}) of
        {ok, Media} -> 
            Media;
        undefined ->
            Media = zp_db:assoc_props("
                select m.*, rm.id as rsc_media_id 
                from media m, rsc_media rm
                where rm.rsc_id = $1
                  and rm.media_id = m.id
                order by rm.seq, rm.id", [RscId], Context),
            zp_depcache:set({media_rsc, RscId}, Media, ?WEEK),
            Media
    end.

%% @doc Get the Nth media for a certain resource
%% @spec get_rsc(RscId, N, Context) -> PropList | undefined
get_rsc(RscId, N, Context) ->
    case zp_depcache:get({media_rsc, RscId}, N) of
        {ok, M} -> 
            M;
        undefined ->
            Ms = get_rsc(RscId, Context),
            try
                lists:nth(N, Ms)
            catch
                _:_ -> undefined
            end
    end.


%% @doc Fetch the media information for the media coupled to the resource
%% @spec get_rsc_media(RscId, MediaId, Context) -> [PropList]
get_rsc_media(RscId, MediaId, Context) ->
    F = fun() ->
        zp_db:assoc_props("
            select m.*, rm.id as rsc_media_id 
            from media m, rsc_media rm
            where rm.rsc_id = $1
              and rm.media_id = m.id
              and rm.media_id = $2
            order by rm.seq, rm.id", [RscId, MediaId], Context)
    end,
    zp_depcache:memo(F, {media_rsc_id, RscId, MediaId}, ?DAY, [{media_rsc, RscId}]).


%% @doc Delete the media at the id, the file is also deleted.
%% @spec delete(MediaId, Context) -> ok | {error, Reason}
delete(MediaId, Context) ->
    case zp_acl:media_editable(MediaId, Context) of
        true ->
            case zp_db:q1("select filename from media where id = $1", [MediaId], Context) of
                undefined ->
                    {error, enoent};
                Filename ->
                    Abs = zp_media_archive:abspath(Filename, Context),
                    DeleteF = fun(Ctx) ->
                        zp_db:delete(media, MediaId, Ctx),
                        case file:delete(Abs) of
                            {error, enoent} -> ok;
                            {error, enotdir} -> ok;
                            ok -> ok
                        end
                    end,
                    zp_db:transaction(DeleteF, Context)
            end;
        false ->
            {error, eacces}
    end.


%% @doc Insert a file, when the file is not in archive then a copy is made in the archive
%% @spec insert_file(File, Context) -> {ok, Id} | {error, Reason}
insert_file(File, Context) ->
    insert_file(File, [], Context).

insert_file(File, Props, Context) ->
    OriginalFilename = proplists:get_value(original_filename, Props, File),
    PropsMedia = add_media_info(File, Props),
    PropsAccess = zp_acl:add_defaults(PropsMedia, Context),
    PropsCreator = zp_acl:add_user(creator_id, PropsAccess, Context),
    case zp_acl:group_editable(proplists:get_value(group_id, PropsCreator), Context) of
        true ->
            SafeRootName = zp_string:to_rootname(OriginalFilename),
            SafeFilename = SafeRootName ++ zp_media_identify:extension(proplists:get_value(mime, PropsCreator)),
            ArchiveFile = zp_media_archive:archive_copy_opt(File, SafeFilename, Context),
            RootName = filename:rootname(filename:basename(ArchiveFile)),
            zp_db:insert(media, [{filename, ArchiveFile},{rootname, RootName}|PropsCreator], Context);
        false ->
            {error, eacces}
    end.


%% @doc Insert a file, when the file is not in archive then a copy is made in the archive.  After inserting it is coupled
%% to the given resource.
%% @spec insert_file(File, Context) -> {ok, Id} | {error, Reason}
insert_file_rsc(File, RscId, Context) ->
    insert_file_rsc(File, RscId, [], Context).

insert_file_rsc(File, RscId, Props, Context) ->
    case zp_acl:rsc_editable(RscId, Context) of
        true ->
            F = fun(Ctx) ->
                {ok, Id} = insert_file(File, Props, Ctx),
                insert_rsc_media(RscId, Id, Ctx),
                {ok, Id}
            end,
            {ok, Id} = zp_db:transaction(F, Context),
            zp_depcache:flush({media_rsc, RscId}),
            {ok, Id};
        false ->
            {error, eacces}
    end.


%% @doc Couple the media to the resource
%% @spec insert_rsc_media(RscId, Id, Context) -> {ok, RscMediaId} | {error, Reason}
insert_rsc_media(RscId, Id, Context) ->
    case zp_acl:rsc_editable(RscId, Context) of
        true ->
            {ok, RscMediaId} = zp_db:insert(rsc_media, [{rsc_id, RscId}, {media_id, Id}], Context),
            zp_depcache:flush({media_rsc, RscId}),
            {ok, RscMediaId};
        false ->
            {error, eacces}
    end.
    

%% @doc Decouple a media from an resource
%% @spec delete_rsc_media(RscId, MediaId, Context) -> ok | {error, Reason}
delete_rsc_media(RscId, Id, Context) ->
    case zp_acl:rsc_editable(RscId, Context) of
        true ->
            zp_db:q("delete from rsc_media where rsc_id = $1 and media_id = $2", [RscId, Id], Context),
            zp_depcache:flush({media_rsc, RscId}),
            ok;
        false ->
            {error, eacces}
    end.

%% @doc Remove a media from a resource
%% @spec delete_rsc_media(RscMediaId, Context) -> ok | {error, Reason}
delete_rsc_media(RscMediaId, Context) ->
    RscId = zp:q1("select rsc_id from rsc_media where id = $2", [RscMediaId], Context),
    case RscId of
        undefined ->
            {error, enoent};
        _ ->
            case zp_acl:rsc_editable(RscId, Context) of
                true ->
                    zp_db:delete(rsc_media, RscMediaId, Context),
                    zp_depcache:flush({media_rsc, RscId}),
                    ok;
                false ->
                    {error, eacces}
            end
    end.

%% @doc Fetch the media information of the file, if they are not set in the Props
add_media_info(File, Props) ->
    PropsSize = case proplists:get_value(size, Props) of
        undefined ->
            [{size, filelib:file_size(File)}|Props];
        _ -> 
            Props
    end,
    PropsMime = case proplists:get_value(mime, PropsSize) of
        undefined ->
            case zp_media_identify:identify(File) of
                {ok, MediaInfo} -> MediaInfo ++ PropsSize;
                {error, _Reason} -> PropsSize
            end;
        _ -> 
            PropsSize
    end,
    PropsMime.
