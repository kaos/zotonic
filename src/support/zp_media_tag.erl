%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-03-03
%%
%% @doc Generate media urls and tags, based on the filename, size and optional filters.
%% Does not generate media previews itself, this is done when fetching the image.
%%
%% Typical urls are like: 
%% /image/2007/03/31/wedding.jpg(300x300)(crop-center)(a3ab6605e5c8ce801ac77eb76289ac12).jpg
%% /media/inline/2007/03/31/wedding.jpg
%% /media/attachment/2007/03/31/wedding.jpg
%%

-module(zp_media_tag).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    tag/2,
    url/2,
    props2url/1,
    url2props/1,
    test/0
]).

-include_lib("zophrenic.hrl").

%% @spec tag(Filename, Options) -> {tag, TagString} | {error, Reason}
%% @doc Generate a HTML image tag for the image with the filename and options
tag(Filename, Options) when is_binary(Filename) ->
    tag(binary_to_list(Filename), Options);
tag(Filename, Options) ->
    case url1(Filename, Options) of
        {url, Url, TagOpts, ImageOpts} -> 
            % Calculate the real size of the image using the options
            TagOpts1 = case zp_media_preview:size(filename_to_filepath(Filename),ImageOpts) of
                            {size, Width, Height, _Mime} ->
                                [{width,Width},{height,Height}|TagOpts];
                            _ ->
                                TagOpts
                        end,
            % Make sure the required alt tag is present
            TagOpts2 =  case proplists:get_value(alt, TagOpts1) of
                            undefined -> [{alt,""}|TagOpts1];
                            _ -> TagOpts1
                        end,
            {tag, zp_tags:render_tag("img", [{src,Url}|TagOpts2])};
        {error, Reason} -> 
            {error, Reason}
    end.


%% @doc Give the filepath for the filename being served
%% @todo Ensure the file is really in the given directory (ie. no ..'s)
filename_to_filepath(Filename) ->
    filename:join("priv/files/archive/", Filename).


%% @doc Give the base url for the filename being served
%% @todo Use the dispatch rules to find the correct image path
filename_to_urlpath(Filename) ->
    filename:join("/image/", Filename).


%% @spec url(Filename, Options) -> {url, Url} | {error, Reason}
%% @doc Generate the url for the image with the filename and options
url(Filename, Options) ->
    case url1(Filename, Options) of
        {url, Url, _TagOptions, _ImageOptions} -> {url, Url};
        {error, Error} -> {error, Error}
    end.


%% @spec url1(Filename, Options) -> {url, Url, TagOptions, ImageOpts} | {error, Reason}
%% @doc Creates an url for the given filename and filters.  This does not check the filename or if it is convertible.
url1(Filename, Options) ->
    {TagOpts, ImageOpts} = lists:partition(fun is_tagopt/1, Options),
    % Map all ImageOpts to an opt string
    UrlProps = props2url(ImageOpts),
    Checksum = zp_utils:checksum([Filename,UrlProps,".jpg"]),
    {url, list_to_binary(filename_to_urlpath(lists:flatten([Filename,UrlProps,$(,Checksum,").jpg"]))), 
          TagOpts,
          ImageOpts}.


is_tagopt({alt,   _}) -> true;
is_tagopt({title, _}) -> true;
is_tagopt({class, _}) -> true;
is_tagopt({style, _}) -> true;
is_tagopt(_) -> false.


props2url(Props) -> 
    props2url(Props, undefined, undefined, []).

props2url([], Width, Height, Acc) ->
    Size =  case {Width,Height} of
                {undefined,undefined} -> [];
                {_W,undefined} -> [integer_to_list(Width)] ++ "x";
                {undefined,_H} -> [$x|integer_to_list(Height)];
                {_W,_H} -> integer_to_list(Width) ++ [$x|integer_to_list(Height)]
            end,
    lists:flatten([$(, zp_utils:combine(")(", [Size|lists:reverse(Acc)]), $)]);

props2url([{width,Width}|Rest], _Width, Height, Acc) ->
    props2url(Rest, zp_convert:to_integer(Width), Height, Acc);
props2url([{height,Height}|Rest], Width, _Height, Acc) ->
    props2url(Rest, Width, zp_convert:to_integer(Height), Acc);
props2url([{Prop}|Rest], Width, Height, Acc) ->
    props2url(Rest, Width, Height, [atom_to_list(Prop)|Acc]);
props2url([{Prop,true}|Rest], Width, Height, Acc) ->
    props2url(Rest, Width, Height, [atom_to_list(Prop)|Acc]);
props2url([{Prop,Value}|Rest], Width, Height, Acc) ->
    props2url(Rest, Width, Height, [[atom_to_list(Prop),$-,zp_convert:to_list(Value)]|Acc]).


%% @spec url2props(Url) -> {Filepath,PreviewPropList,Checksum,ChecksumBaseString}
%% @doc Translate an url of the format "image.jpg(300x300)(crop-center)(checksum).jpg" to parts
%% @todo Map the extension to the format of the preview (.jpg or .png)
url2props(Url) ->
    {Filepath,Rest} = lists:splitwith(fun(C) -> C =/= $( end, Url),
    PropsRoot       = filename:rootname(Rest),
    % Take the checksum from the string
    LastParen       = string:rchr(PropsRoot, $(),
    {Props,[$(|Check]} = lists:split(LastParen-1, PropsRoot),
    Check1          = string:strip(Check, right, $)),
    zp_utils:checksum_assert([Filepath,Props,".jpg"], Check1),
    PropList        = string:tokens(Props, ")("),
    PropList1       = case PropList of
                        [] -> [];
                        [Size|RestProps]->
                            {W,XH} = lists:splitwith(fun(C) -> C >= $0 andalso C =< $9 end, Size),
                            SizeProps = case {W,XH} of
                                            {"", "x"}            -> [];
                                            {"", ""}             -> [];
                                            {Width, ""}          -> [{width,list_to_integer(Width)}]; 
                                            {Width, "x"}         -> [{width,list_to_integer(Width)}]; 
                                            {"", [$x|Height]}    -> [{height,list_to_integer(Height)}]; 
                                            {Width, [$x|Height]} -> [{width,list_to_integer(Width)},{height,list_to_integer(Height)}]
                                        end,
                            SizeProps ++ url2props1(RestProps, [])
                      end,
    {Filepath,PropList1,Check1,Props}.

url2props1([], Acc) ->
    lists:reverse(Acc);
url2props1([P|Rest], Acc) ->
    {Prop,Arg} = lists:splitwith(fun(C) -> C =/= $- end, P),
    Arg1 =  case Arg of
                [$-|A] -> A;
                _ -> Arg
            end,
    Filter = zp_media_preview:string2filter(Prop, Arg1),
    url2props1(Rest, [Filter|Acc]).


test() ->
    {   "koe.jpg", [{width,400},{crop,east},{blur},{grey}], 
        "61DADDE06035A4CD4862D99688EC0FFF","(400x)(crop-east)(blur)(grey)"} 
    = url2props("koe.jpg(400x)(crop-east)(blur)(grey)(61DADDE06035A4CD4862D99688EC0FFF).jpg"),
    
    "(300x300)(crop-center)" 
    = props2url([{width,300},{height,300},{crop,center}]),
    
    {tag,[60,"img",
          [ [32,<<"src">>,61,39,
             <<"/image/koe.jpg(300x300)(crop-center)(0F96A52C1BD7F22E5833316DC9483913).jpg">>,39],
            [],
            [32,<<"width">>,61,39,<<"300">>,39],
            [32,<<"height">>,61,39,<<"300">>,39],
            [32,<<"class">>,61,39,"some-class",39]],
          47,62]} = 
    tag(<<"koe.jpg">>, [{width,300},{height,300},{crop,center},{class,"some-class"}]).
