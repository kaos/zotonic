%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-08-05
%%
%% @doc Defines all paths for files and directories of a site.

-module(z_path).
-author("Marc Worrell <marc@worrell.nl").

-export([
    media_preview/1,
    media_archive/1
]).

-include("zotonic.hrl").

%% @doc Return the path to the media preview directory
%% @spec media_preview(#context) -> filename()
media_preview(#context{host=Host}) ->
    filename:join([code:lib_dir(zotonic, priv), "sites", Host, "files", "preview"]).

%% @doc Return the path to the media archive directory
%% @spec media_archive(#context) -> filename()
media_archive(#context{host=Host}) ->
    filename:join([code:lib_dir(zotonic, priv), "sites", Host, "files", "archive"]).
