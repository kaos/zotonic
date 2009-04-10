%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-04-10
%%
%% @doc Media archiving utilities

-module(zp_media_archive).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    abspath/2,
    ensure_relative/2,
    path_archive/1,
    archive_file/2,
    archive_copy/2,
    archive_filename/2
]).

-include_lib("zophrenic.hrl").

%% @doc Return the absolute path name of a relative file in the archive
abspath(File, Context) ->
    filename:join([path_archive(Context), zp_convert:to_list(File)]).

%% @doc Ensure that the filename is relative to the archive.  When needed move the file to the archive.  Return the relative path.
ensure_relative(File, Context) ->
    Archive = path_archive(Context) ++ "/",
    case lists:prefix(Archive, File) of
        true ->
            lists:nthtail(File, length(Archive));
        false ->
            % Not in the archive dir, move the file
            archive_file(File, Context)
    end.

%% @doc Archive a file in the archive directory (when it is not archived yet)
%% @spec archive_file(Filename, Context) -> ArchivedFilename
archive_file(Filename, Context) ->
    NewFile = archive_filename(Filename, Context),
    AbsPath = abspath(NewFile, Context),
    ok = filelib:ensure_dir(AbsPath),
    ok = file:rename(Filename, AbsPath),
    NewFile.

%% @doc Archive a copy of a file in the archive directory (when it is not archived yet)
%% @spec archive_file(Filename, Context) -> ArchivedFilename
archive_copy(Filename, Context) ->
    NewFile = archive_filename(Filename, Context),
    AbsPath = abspath(NewFile, Context),
    ok = filelib:ensure_dir(AbsPath),
    {ok, _Bytes} = file:copy(Filename, AbsPath),
    NewFile.

%% Return an unique filename for archiving the file
archive_filename(Filename, Context) ->
    Archive = path_archive(Context),
    {{Y,M,D}, _} = calendar:local_time(),
    Rootname = filename:rootname(filename:basename(Filename)),
    Extension = filename:extension(Filename),
    RelRoot = filename:join([integer_to_list(Y),integer_to_list(M),integer_to_list(D),Rootname]),
    make_unique(Archive, RelRoot, Extension).


%% @doc Make sure that the filename is unique by appending a number on filename clashes
make_unique(Archive, Rootname, Extension) ->
    File = filename:join([Archive, Rootname]) ++ Extension,
    case filelib:is_file(File) of
        true ->
            make_unique(Archive, Rootname, Extension, 1);
        false -> 
            filename:join([Rootname]) ++ Extension
    end.

make_unique(Archive, Rootname, Extension, Nr) ->
    File = filename:join([Archive, Rootname ++ [$-|integer_to_list(Nr)]]) ++ Extension,
    case filelib:is_file(File) of
        true ->
            make_unique(Archive, Rootname, Extension, Nr+1);
        false -> 
            filename:join([Rootname ++ [$-|integer_to_list(Nr)]]) ++ Extension
    end.


%% @doc Return the path to the media archive directory
path_archive(_Context) ->
    Priv = code:lib_dir(zophrenic, priv),
    filename:join([Priv, "files", "archive"]).

    