%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-03-02
%%
%% @doc Identify files, fetch metadata about an image
%% @todo Recognize more files based on magic number, think of office files etc.

-module(zp_media_identify).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    identify_cached/1,
    identify/1
]).

-include_lib("zophrenic.hrl").


%% @spec identify(ImageFile) -> {ok, Meta} | {error, Error}
%% @doc Caching version of identify/1. Fetches information about an image, returns width, height, type, etc.
identify_cached(ImageFile) ->
    case zp_depcache:get({identify, ImageFile}) of
        {ok, Meta} -> Meta;
        undefined ->
            Result = identify(ImageFile),
            zp_depcache:set({identify, ImageFile}, Result, ?YEAR, [identify, ImageFile]),
            Result
    end.
    
    
%% @spec identify(ImageFile) -> PropList
%% @doc Fetch information about an image, returns width, height, type, etc.
identify(ImageFile) ->
    CleanedImageFile = zp_utils:os_escape(ImageFile),
    Result    = os:cmd("identify -quiet \"" ++ CleanedImageFile ++ "[0]\""),
    % ["test/a.jpg","JPEG","3440x2285","3440x2285+0+0","8-bit","DirectClass","2.899mb"]
    % sometimes:
    % test.jpg[0]=>test.jpg JPEG 2126x1484 2126x1484+0+0 DirectClass 8-bit 836.701kb 0.130u 0:02
    [Line1|_] = string:tokens(Result, "\r\n"),
    Words = string:tokens(Line1, " "),
    WordCount = length(Words),
    Words1 = if
        WordCount > 4 -> 
            {A,_B} = lists:split(4, Words),
            A;
        true -> 
            Words
    end,
    try 
        [_Path, Type, Dim, _Dim2] = Words1,
        [Width,Height] = string:tokens(Dim, "x"),
        {ok, [
            {width, list_to_integer(Width)},
            {height, list_to_integer(Height)},
            {mime, mime(Type)},
            {orientation, 1}
        ]}
    catch 
        _:_ ->
            ?LOG("identify of ~p failed - ~p", [CleanedImageFile, Line1]),
            {error, "unknown result from 'identify': '"++Line1++"'"}
    end.


%% @spec mime(String) -> MimeType
%% @doc Map the type returned by ImageMagick to a mime type
%% @todo Add more imagemagick types, check the mime types
mime("PDF") -> "application/pdf";
mime("PS") -> "application/postscript";
mime("PS2") -> "application/postscript";
mime("PS3") -> "application/postscript";
mime("PNG8") -> "image/png";
mime("PNG24") -> "image/png";
mime("PNG32") -> "image/png";
mime(Type) -> "image/" ++ string:to_lower(Type).

