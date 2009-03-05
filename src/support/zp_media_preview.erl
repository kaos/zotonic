%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-03-02
%%
%% @doc Make still previews of media, using image manipulation functions.  Resize, crop, gray, etc.
%% This uses the command line imagemagick tools for all image manipulation.
%% This code is adapted from PHP GD2 code, so the resize/crop could've been done more efficiently, but it works :-)
%%
%% @todo Select PNG for small resulting images with lossless source image

-module(zp_media_preview).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    convert/3,
    size/2,
    can_generate_preview/1,
    string2filter/2,
    test/0
]).

-include_lib("zophrenic.hrl").


%% @spec convert(InFile, OutFile, Filters) -> ok | {error, Reason}
%% @doc Convert the Infile to an outfile with a still image using the filters.
%% @todo Check if the conversion has been done
%% @todo Check if the target /= source
convert(InFile, OutFile, Filters) ->
    case zp_media_identify:identify_cached(InFile) of
        {ok, FileProps} ->
            {mime, Mime} = proplists:lookup(mime, FileProps),
            case can_generate_preview(Mime) of
                true ->
                    {_EndWidth, _EndHeight, CmdArgs} = cmd_args(FileProps, Filters),
                    Args1   = lists:flatten(zp_utils:combine(32, CmdArgs)),
                    Cmd     = ["convert \"", zp_utils:os_escape(InFile), "[0]\" ", Args1, " \"", zp_utils:os_escape(OutFile), "\""],
                    file:delete(OutFile),
                    os:cmd(lists:flatten(Cmd)),
                    case filelib:is_regular(OutFile) of
                        true -> ok;
                        false -> {error, "Error during convert."}
                    end;
                false ->
                    {error, "Can not convert "++Mime}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% @spec size(InFile, Filters) -> {size, Width, Height, ResizedMime} | {error, Reason}
%% @doc Calculate the size of the resulting image.
size(InFile, Filters) ->
    case zp_media_identify:identify_cached(InFile) of
        {ok, FileProps} ->
            {mime, Mime} = proplists:lookup(mime, FileProps),
            case can_generate_preview(Mime) of
                true ->
                    {width, ImageWidth}   = proplists:lookup(width, FileProps),
                    {height, ImageHeight} = proplists:lookup(height, FileProps),
                    {orientation, Orientation} = proplists:lookup(orientation, FileProps),
                    
                    ReqWidth   = proplists:get_value(width, Filters),
                    ReqHeight  = proplists:get_value(height, Filters),
                    {CropPar,_Filters1} = fetch_crop(Filters),
                    {ResizeWidth,ResizeHeight,CropArgs} = calc_size(ReqWidth, ReqHeight, ImageWidth, ImageHeight, CropPar, Orientation),
                    case CropArgs of
                        none -> {size, ResizeWidth, ResizeHeight, "image/jpeg"};
                        {_CropL, _CropT, CropWidth, CropHeight} -> {size, CropWidth, CropHeight, "image/jpeg"}
                    end;
                false ->
                    {error, "Can not convert "++Mime}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


%% @spec can_generate_preview(Mime) -> true | false
%% @doc Check if we can generate a preview image of the given mime type
can_generate_preview("image/" ++ _) -> true;
can_generate_preview("application/pdf") -> true;
can_generate_preview("application/postscript") -> true;
can_generate_preview(_Mime) -> false.


%% @doc Map filters to commandline options
cmd_args(FileProps, Filters) ->
    {width, ImageWidth}   = proplists:lookup(width, FileProps),
    {height, ImageHeight} = proplists:lookup(height, FileProps),
    {mime, Mime}          = proplists:lookup(mime, FileProps),
    {orientation, Orientation} = proplists:lookup(orientation, FileProps),
    ReqWidth   = proplists:get_value(width, Filters),
    ReqHeight  = proplists:get_value(height, Filters),
    {CropPar,Filters1} = fetch_crop(Filters),
    {ResizeWidth,ResizeHeight,CropArgs} = calc_size(ReqWidth, ReqHeight, ImageWidth, ImageHeight, CropPar, Orientation),
    Filters2   = [  {make_image, Mime},
                    {correct_orientation, Orientation},
                    {resize, ResizeWidth, ResizeHeight}, 
                    {crop, CropArgs} | Filters1],
    Filters3 = case is_blurred(Filters2) of
                    true ->  Filters2 ++ [{quality}];
                    false -> Filters2 ++ [{sharpen_small}, {quality}]
               end,

    {EndWidth,EndHeight,Args} = lists:foldl(fun (Filter, {W,H,Acc}) -> 
                                                {NewW,NewH,Arg} = filter2arg(Filter, W, H),
                                                {NewW,NewH,[Arg|Acc]} 
                                            end,
                                            {ImageWidth,ImageHeight,[]},
                                            Filters3),
    {EndWidth, EndHeight, lists:reverse(Args)}.


%% @doc Check if there is a blurring filter that prevents us from sharpening the resulting image
is_blurred([]) -> false;
is_blurred([{blur}|_]) -> true;
is_blurred([{blur, _}|_]) -> true;
is_blurred([_|Rest]) -> is_blurred(Rest).


%% @spec out_mime(Mime,Width,Height) -> Mime
%% @doc Return the mime type of the image generated by resizing an image of a certain type.
out_mime(Mime, Width, Height)
    when         Width < 100
         andalso Height < 100
         andalso (Mime == "image/png" orelse Mime == "image/gif" orelse Mime == "application/pdf")
    -> "image/png";
out_mime(_Mime, _Width, _Height) -> "image/jpeg".


%% @spec filter2arg(Filter, Width, Height) -> 
%% @doc Map filters to an ImageMagick argument

filter2arg({make_image, "application/pdf"}, Width, Height) ->
    RArg = ["-resize ", integer_to_list(Width),$x,integer_to_list(Height)],
    {Width, Height, RArg};
filter2arg({make_image, _Mime}, Width, Height) ->
    {Width, Height, []};
filter2arg({correct_orientation, Orientation}, Width, Height) ->
    case Orientation of
    	2 -> {Width, Height, "-flip"};
    	3 -> {Width, Height, "-rotate 80"};
    	4 -> {Width, Height, "-flop"};
    	5 -> {Width, Height, "-transpose"};
    	6 -> {Width, Height, "-rotate 90"};
    	7 -> {Width, Height, "-transverse"};
    	8 -> {Width, Height, "-rotate 270"};
        _ -> {Width, Height, []}
    end;
filter2arg({background, Color}, Width, Height) ->
    {Width, Height, ["-background ", $", zp_utils:os_escape(Color), $"]};
filter2arg({width, _}, Width, Height) ->
    {Width, Height, []};
filter2arg({height, _}, Width, Height) ->
    {Width, Height, []};
filter2arg({resize, EndWidth, EndHeight}, Width, Height) when Width < EndWidth andalso Height < EndHeight ->
    % Prevent scaling up, perform an extent instead
    GArg = "-gravity Center",
    EArg = ["-extent ", integer_to_list(EndWidth),$x,integer_to_list(EndHeight)],
    % Still thumbnail to remove extra info from the image
    RArg = ["-thumbnail ", integer_to_list(EndWidth),$x,integer_to_list(EndHeight),$\\,$!],
    {EndWidth, EndHeight, [GArg, 32, EArg, 32, RArg]};
filter2arg({resize, EndWidth, EndHeight}, _Width, _Height) ->
    GArg = "-gravity NorthWest",
    RArg = ["-thumbnail ", integer_to_list(EndWidth),$x,integer_to_list(EndHeight),$\\,$!],
    {EndWidth, EndHeight, [GArg, 32, RArg]};
filter2arg({crop, none}, Width, Height) ->
    {Width, Height, []};
filter2arg({crop, {CropL, CropT, CropWidth, CropHeight}}, _Width, _Height) ->
    GArg = "-gravity NorthWest",
    CArg = ["-crop ",   integer_to_list(CropWidth),$x,integer_to_list(CropHeight), 
                        $+,integer_to_list(CropL),$+,integer_to_list(CropT)],
    RArg = "+repage",
    {CropWidth, CropHeight, [GArg,32,CArg,32,RArg]};
filter2arg({grey}, Width, Height) ->
    {Width, Height, "-colorspace Gray"};
filter2arg({mono}, Width, Height) ->
    {Width, Height, "-monochrome"};
filter2arg({flip}, Width, Height) ->
    {Width, Height, "-flip"};
filter2arg({flop}, Width, Height) ->
    {Width, Height, "-flop"};
filter2arg({blur}, Width, Height) ->
    filter2arg({blur, 10}, Width, Height);
filter2arg({blur, Blur}, Width, Height) when is_integer(Blur) ->
    {Width, Height, ["-blur ", integer_to_list(Blur)]};
filter2arg({blur, Blur}, Width, Height) when is_list(Blur) ->
    case string:tokens("x", Blur) of
        [A,B] -> {Width, Height, ["-blur ", ensure_integer(A), $x, ensure_integer(B)]};
        [A] ->   {Width, Height, ["-blur ", ensure_integer(A)]}
    end;
filter2arg({sharpen_small}, Width, Height) when Width < 400 andalso Height < 400 ->
    {Width, Height, "-unsharp 0.3x0.7 "};
filter2arg({sharpen_small}, Width, Height) ->
    {Width, Height, []};
filter2arg({quality}, Width, Height) ->
    Pix = Width * Height,
    Q   = if 
            Pix < 10000 -> 100;
            Pix > 90000 -> 50;
            true -> 50 + round((Pix - 10000) * (100-50) / 80000)
          end,
    {Width,Height, ["-quality ",integer_to_list(Q)]}.
    

%% @spec fetch_crop(Filters) -> {Crop, Filters}
%% @doc Split the filters into size/crop and image manipulation filters.
fetch_crop(Filters) ->
    {Crop,OtherFilters} = lists:partition(fun (F) -> element(1,F) == 'crop' end, Filters),
    CropPar = case Crop of
                  [{crop,Gravity}] -> Gravity; % center or one of the wind directions
                  _ -> none
              end,
    {CropPar,OtherFilters}.


%%@doc Calculate the size of the resulting image, depends on the crop and the original image size
calc_size(Width, Height, ImageWidth, ImageHeight, CropPar, Orientation) when Orientation >= 5 ->
    calc_size(Width, Height, ImageHeight, ImageWidth, CropPar, 1);

calc_size(undefined, undefined, ImageWidth, ImageHeight, _CropPar, _Orientation) ->
    {ImageWidth, ImageHeight, none};

calc_size(undefined, Height, ImageWidth, ImageHeight, CropPar, Orientation) ->
    Width = round((ImageWidth / ImageHeight) * Height),
    calc_size(Width, Height, ImageWidth, ImageHeight, CropPar, Orientation);

calc_size(Width, undefined, ImageWidth, ImageHeight, CropPar, Orientation) ->
    Height = round((ImageHeight / ImageWidth) * Width),
    calc_size(Width, Height, ImageWidth, ImageHeight, CropPar, Orientation);

calc_size(Width, Height, ImageWidth, ImageHeight, CropPar, _Orientation) ->
    ImageAspect = ImageWidth / ImageHeight,
    Aspect      = Width / Height,
    case CropPar of
        none ->
            case Aspect > ImageAspect of
                true  -> {ceil(ImageAspect * Height), Height, none};
                false -> {Width, ceil(Width / ImageAspect), none}
            end;
        _ ->
			% When we are doing a crop then we have to calculate the
			% maximum inner bounding box, and not the maximum outer 
			% bounding box for the image
		    {W,H} = case Aspect > ImageAspect of
        		        true ->
        				    % width is the larger one
        				    {Width, Width / ImageAspect};
        				false ->
        				    % height is the larger one
        				    {ImageAspect * Height, Height}
        			end,
        	CropL = case CropPar of
        	            X when X == north_west; X == west; X == south_west -> 0;
        	            X when X == north_east; X == east; X == south_east -> ceil(W - Width);
        	            _ -> ceil((W - Width) / 2)
    	            end,
    	    CropT = case CropPar of
    	                Y when Y == north_west; Y == north; Y == north_east -> 0;
    	                Y when Y == south_west; Y == south; Y == south_east -> ceil(H - Height);
    	                _ -> ceil((H - Height) / 2)
	                end,
	        
	        % @todo Prevent scaleup of the image, but preserve the result size
	        % The crop is relative to the original image
	        {ceil(W), ceil(H), {CropL, CropT, Width, Height}}
    end.


%% @spec string2filter(Filter, Arg) -> FilterTuple
%% @doc Map the list of known filters and known args to atoms.  Used when mapping preview urls back to filter args.
string2filter("crop", []) ->
    {crop,center};
string2filter("crop", Where) -> 
    Dir = case Where of
            "north"      -> north;
            "north_east" -> north_east;
            "east"       -> east;
            "south_east" -> south_east;
            "south"      -> south;
            "south_west" -> south_west;
            "west"       -> west;
            "north_west" -> north_west;
            "center"     -> center
          end,
    {crop,Dir};
string2filter("grey",[]) ->
    {grey};
string2filter("mono",[]) ->
    {monochrome};
string2filter("flip",[]) ->
    {flip};
string2filter("flop",[]) ->
    {flop};
string2filter("blur",[]) ->
    {blur};
string2filter("blur",Arg) ->
    {blur,Arg};
string2filter("quality", Arg) ->
    {quality,list_to_integer(Arg)};
string2filter("background", Arg) ->
    {background,Arg}.



% simple ceil for positive numbers
ceil(A)  -> round(A + 0.499999).
%floor(A) -> round(A - 0.499999).

ensure_integer(A) ->
    integer_to_list(list_to_integer(A)).


test() ->
    Props = [{width,100}, {height,66}, {mime,"image/jpeg"}, {orientation,1}],
    Filters = [{crop,center}, {width,80}, {height,80}],
    {_W,_H,Args} = cmd_args(Props, Filters),
    CmdArgs = lists:flatten(zp_utils:combine(32, Args)),
    "  -gravity Center -extent 122x80 -thumbnail 122x80\\! -gravity NorthWest -crop 80x80+21+0 +repage   -unsharp 0.3x0.7  -quality 100" = CmdArgs.
