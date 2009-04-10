%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-09
%%
%% @doc Model for media database

-module(m_media).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    get/2,
    get_rsc/2,
    get_rsc/3,
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

%% @doc Get the media record with the id
%% @spec get(MediaId, Context) -> PropList
get(Id, Context) ->
    zp_db:assoc_props_row("select * from media where id = $1", [Id], Context).

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


%% @doc Delete the media at the id, the file is also deleted.
%% @spec delete(MediaId, Context) -> ok | {error, Reason}
delete(MediaId, Context) ->
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
    end.


%% @doc Insert a file, when the file is not in archive then a copy is made in the archive
%% @spec insert_file(File, Context) -> {ok, Id} | {error, Reason}
insert_file(File, Context) ->
    insert_file(File, [], Context).

insert_file(File, Props, Context) ->
    PropsMedia = add_media_info(File, Props),
    PropsAccess = zp_access_control:add_defaults(PropsMedia, Context),
    PropsCreator = zp_access_control:add_person(creator_id, PropsAccess, Context),
    ArchiveFile = zp_media_archive:archive_copy(File, Context),
    RootName = filename:rootname(filename:basename(ArchiveFile)),
    zp_db:insert(media, [{filename, ArchiveFile},{rootname, RootName}|PropsCreator], Context).


%% @doc Insert a file, when the file is not in archive then a copy is made in the archive.  After inserting it is coupled
%% to the given resource.
%% @spec insert_file(File, Context) -> {ok, Id} | {error, Reason}
insert_file_rsc(File, RscId, Context) ->
    insert_file_rsc(File, RscId, [], Context).

insert_file_rsc(File, RscId, Props, Context) ->
    F = fun(Ctx) ->
        {ok, Id} = insert_file(File, Props, Ctx),
        insert_rsc_media(RscId, Id, Ctx),
        {ok, Id}
    end,
    zp_db:transaction(F, Context).


%% @doc Couple the media to the resource
%% @spec insert_rsc_media(RscId, Id, Context) -> {ok, RscMediaId}
insert_rsc_media(RscId, Id, Context) ->
    zp_db:insert(rsc_media, [{rsc_id, RscId}, {media_id, Id}], Context).
    

%% @doc Decouple a media from an resource
delete_rsc_media(RscId, Id, Context) ->
    zp_db:q("delete from rsc_media where rsc_id = $1 and media_id = $2", [RscId, Id], Context).


%% @doc Decouple a media from an resource
delete_rsc_media(RscMediaId, Context) ->
    zp_db:delete(rsc_media, RscMediaId, Context).


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
            case zp_media_identify:identify_cached(File) of
                {ok, MediaInfo} -> MediaInfo ++ PropsSize;
                {error, _Reason} -> PropsSize
            end;
        _ -> 
            PropsSize
    end,
    PropsMime.
