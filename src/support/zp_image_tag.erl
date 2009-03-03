%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-03-03
%%
%% @doc Generate image urls and tags, based on the filename, size and optional filters.
%% Does not generate the image itself, this is done when fetching the image.

-module(zp_image_tag).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    tag/2,
    url/2
]).


%% @spec tag(Filename, Options) -> {tag, TagString} | {error, Reason}
%% @doc Generate a HTML image tag for the image with the filename and options
tag(Filename, Options) ->
    case url1(Filename, Options) of
        {url, Url, ExtraOptions} -> 
            {tag, Url};
        {error, Reason} -> 
            {error, Reason}
    end.
    

%% @spec url(Filename, Options) -> {url, Url} | {error, Reason}
%% @doc Generate the url for the image with the filename and options
url(Filename, Options) ->
    {error, not_implemented}.


%% @spec url1(Filename, Options) -> {url, Url, TagOptions} | {error, Reason}
%% @doc Creates an url for the given filename and filters.
%% Typical urls are like: image/2007/03/31/wedding-300x300-crop-north_east-grey-a3ab6605e5c8ce801ac77eb76289ac12.jpg
url1(Filename, Options) ->
    {TagOpts, ImageOpts} = lists:partition(fun is_tagopt/1, Options),
    case zp_image_convert:size(Filename, ImageOpts) of
        {size, Width, Height, Mime} -> {error, not_implemented};
        {error, Reason} -> {error, Reason}
    end.
        
is_tagopt({alt,   _}) -> true;
is_tagopt({title, _}) -> true;
is_tagopt({class, _}) -> true;
is_tagopt({style, _}) -> true;
is_tagopt(_) -> false.
    