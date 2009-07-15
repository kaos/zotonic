%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-09
%%
%% @doc Model for medium database

-module(m_media).
-author("Marc Worrell <marc@worrell.nl").

-behaviour(gen_model).

%% interface functions
-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2,
    identify/2,
    get/2,
    exists/2,
    depiction/2,
    depicts/2,
    delete/2,
    replace/3,
    insert_file/2,
    insert_file/3,
    replace_file/3,
    replace_file/4
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


%% @doc Return the identification of a medium. Compatible with zp_media_identify:identify()
%% @spec identify(ImageFilePath, Context) -> {ok, PropList} | {error, Reason}
identify(ImageFile, Context) ->
    F = fun() ->
        case zp_media_archive:is_archived(ImageFile, Context) of
            true ->
                RelFile = zp_media_archive:rel_archive(ImageFile, Context),
                case zp_db:q_assoc("select id, mime, width, height, orientation from medium where filename = $1", [RelFile], Context) of
                    undefined ->
                        {error, enoent};
                    Props ->
                        {ok, Props}
                end;
            false ->
                {error, enoent}
        end
    end,
    zp_depcache:memo(F, {medium_identify, ImageFile}, ?DAY).


%% @doc Check if a medium record exists
exists(undefined, _Context) ->
    false;
exists([C|_] = Name, Context) when is_integer(C) ->
    case zp_utils:only_digits(Name) of
        true -> exists(list_to_integer(Name), Context);
        false -> false
    end;
exists(Id, Context) when is_binary(Id) ->
    exists(binary_to_list(Id), Context);
exists(Id, Context) -> 
    case zp_db:q1("select id from medium where id = $1", [Id], Context) of
        undefined -> false;
        _ -> true
    end.


%% @doc Get the medium record with the id
%% @spec get(RscId, Context) -> PropList
get(Id, Context) ->
    F = fun() -> zp_db:assoc_props_row("select * from medium where id = $1", [Id], Context) end,
    zp_depcache:memo(F, {medium, Id}, ?WEEK).


%% @doc Get the medium record that depicts the resource id. "depiction" Predicates are preferred, when 
%% missing then the attached medium record itself is returned.
%% @spec depiction(RscId, Context) -> PropList | undefined
depiction(Id, Context) ->
    F = fun() ->
        RscId = case m_edge:objects(Id, depiction, Context) of
            [DepictId|_Rest] -> DepictId;
            [] -> Id
        end,
        get(RscId, Context)
    end,
    zp_depcache:memo(F, {depiction, Id}, ?WEEK, [#rsc{id=Id}]).


%% @doc Return the list of resources that is depicted by the medium (excluding the rsc itself)
%% @spec depicts(RscId, Context) -> [Id]
depicts(Id, Context) ->
    m_edge:subjects(Id, depiction, Context).        


%% @doc Delete the medium at the id.  The file is queued for later deletion.
%% @spec delete(RscId, Context) -> ok | {error, Reason}
delete(Id, Context) ->
    case zp_acl:rsc_editable(Id, Context) of
        true ->
            Depicts = depicts(Id, Context),
            zp_db:delete(medium, Id, Context),
            [ zp_depcache:flush(#rsc{id=DepictId}) || DepictId <- Depicts ],
            zp_depcache:flush(#rsc{id=Id}),
            ok;
        false ->
            {error, eacces}
    end.


%% @doc Replace or insert a medium record for the page.  This is useful for non-file related media.
%% Resets all non mentioned attributes.
%% @spec insert(Id, Props, Context) -> ok | {error, Reason}
replace(Id, Props, Context) ->
    Depicts = depicts(Id, Context),
    F = fun(Ctx) ->
        {ok, _}  = zp_db:delete(medium, Id, Context),
        {ok, Id} = zp_db:insert(medium, [{id, Id} | Props], Ctx)
    end,
    
    case zp_db:transaction(F, Context) of
        {ok, _} -> 
            [ zp_depcache:flush(#rsc{id=DepictId}) || DepictId <- Depicts ],
            zp_depcache:flush(#rsc{id=Id});
        {rollback, {Error, _Trace}} ->
             {error, Error}
    end.


%% @doc Make a new resource for the file, when the file is not in the archive dir then a copy is made in the archive dir
%% @spec insert_file(File, Context) -> {ok, Id} | {error, Reason}
insert_file(File, Context) ->
    insert_file(File, [], Context).

insert_file(File, Props, Context) ->
    Props1 = case proplists:get_value(category_id, Props, proplists:get_value(category, Props)) of
        undefined ->
            [{category, media} | Props];
        _ ->
            Props
    end,
    Props2 = case proplists:get_value(is_published, Props1) of
        undefined ->
            [{is_published, true} | Props1];
        _ ->
            Props1
    end,
    InsertFun = fun(Ctx) ->
        case m_rsc:insert(Props2, Ctx) of
            {ok, Id} ->
                replace_file(File, Id, Props2, Ctx);
            Error ->
                Error
        end
    end, 
    case zp_db:transaction(InsertFun, Context) of
        {ok, Id} ->
            CatList = m_rsc:is_a_list(Id, Context),
            [ zp_depcache:flush(Cat) || Cat <- CatList ],
            {ok, Id};
        {error, Reason} -> 
            {error, Reason}
    end.


%% @doc Replaces a medium file, when the file is not in archive then a copy is made in the archive.
%% When the resource is in the media category, then the category is adapted depending on the mime type of the uploaded file.
%% @spec insert_file(File, Context) -> {ok, Id} | {error, Reason}
replace_file(File, RscId, Context) ->
    replace_file(File, RscId, [], Context).

replace_file(File, RscId, Props, Context) ->
    case zp_acl:rsc_editable(RscId, Context) of
        true ->
            Depicts = depicts(RscId, Context),
            F = fun(Ctx) ->
                zp_db:delete(medium, RscId, Context),
                OriginalFilename = proplists:get_value(original_filename, Props, File),
                PropsMedia = add_medium_info(File, [{original_filename, OriginalFilename}]),
                SafeRootName = zp_string:to_rootname(OriginalFilename),
                SafeFilename = SafeRootName ++ zp_media_identify:extension(proplists:get_value(mime, PropsMedia)),
                ArchiveFile = zp_media_archive:archive_copy_opt(File, SafeFilename, Ctx),
                RootName = filename:rootname(filename:basename(ArchiveFile)),
                case zp_db:insert(medium, [{id, RscId}, {filename, ArchiveFile}, {rootname, RootName}|PropsMedia], Ctx) of
                    {ok, _MediaId} ->
                        % When the resource is in the media category, then move it to the correct sub-category depending
                        % on the mime type of the uploaded file.
                        case rsc_is_media_cat(RscId, Context) of
                            true ->
                                case proplists:get_value(mime, PropsMedia) of
                                    "image/" ++ _ -> m_rsc:update(RscId, [{category, image}], Ctx);
                                    "video/" ++ _ -> m_rsc:update(RscId, [{category, video}], Ctx);
                                    "sound/" ++ _ -> m_rsc:update(RscId, [{category, sound}], Ctx);
                                    _ -> nop
                                end;
                            false -> nop
                        end,
                        {ok, RscId};
                    Error ->
                        Error
                end
            end,
            
            {ok, Id} = zp_db:transaction(F, Context),
            [ zp_depcache:flush(#rsc{id=DepictId}) || DepictId <- Depicts ],
            zp_depcache:flush(#rsc{id=Id}),
            {ok, Id};
        false ->
            {error, eacces}
    end.


    rsc_is_media_cat(Id, Context) ->
        case zp_db:q1("select c.name from rsc c join rsc r on r.category_id = c.id where r.id = $1", [Id], Context) of
            <<"media">> -> true;
            <<"image">> -> true;
            <<"audio">> -> true;
            <<"video">> -> true;
            _ -> false
        end.



%% @doc Fetch the medium information of the file, if they are not set in the Props
add_medium_info(File, Props) ->
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

