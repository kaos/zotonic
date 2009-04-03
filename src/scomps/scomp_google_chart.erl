%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%%
%% Based on code (c) 2008-2009 Rusty Klophaus
%%
%% @doc Generate a google chart for a dataset

-module(scomp_google_chart).
-behaviour(gen_scomp).

-export([init/1, varies/2, code_change/3, terminate/1, render/4]).

-include("zophrenic.hrl").

init(_Args) -> {ok, []}.
varies(_Params, _Context) -> undefined.
code_change(_OldVsn, State, _Extra) -> {ok, State}.    
terminate(_Reason) -> ok.

render(Params, _Vars, _Context, _State) ->
    Class      = proplists:get_value(class, Params),
    Style      = proplists:get_value(style, Params),
    Id         = proplists:get_value(id, Params),
    Title      = proplists:get_value(title, Params),
	Color      = proplists:get_value(color, Params, "909090"),
	FontSize   = proplists:get_value(font_size, Params, 10),
	Width      = proplists:get_value(width, Params, 300),
	Height     = proplists:get_value(height, Params, 150),
	GridX      = proplists:get_value(grid_x, Params),
	GridY      = proplists:get_value(grid_y, Params),
    GridLineLength  = proplists:get_value(grid_line_length, Params, 1),
    GridBlankLength = proplists:get_value(grid_blank_length, Params, 5),
    BGColor    = proplists:get_value(background_color, Params, "ffffff"),
    ChartColor = proplists:get_value(chart_color, Params, "ffffff"),
    LegendLoc  = proplists:get_value(legend_location, Params, "bottom"),
    AxesArg    = proplists:get_value(axes, Params, []),
    DataArg    = proplists:get_value(data, Params, []),
    BarSpace   = proplists:get_value(bar_space, Params, 3),
    BarGroupSpace   = proplists:get_value(bar_group_space, Params, 7),
    
	% Path to Google API
	Path = "http://chart.apis.google.com/chart?",

	% Chart Type...
	Type = [
		"&cht=",
		case proplists:get_value(type, Params, line) of
			line -> "lc";
			sparkline -> "ls";
			stacked_horizontal_bar -> "bhs";
			stacked_vertical_bar -> "bvs";
			grouped_horizontal_bar -> "bhg";
			grouped_vertical_bar -> "bvg";
			pie -> "p";
			pie3d -> "p3";
			OtherType -> erlang:error({unknown_chart_type, OtherType})
		end
	],
	
	% Title...
	TitleText = case Title of
            		undefined -> [];
            		[]        -> [];
                    <<>>      -> <<>>;
            		OtherTitle -> ["&chtt=", zp_utils:url_encode(OtherTitle)]
            	end,
	
	% Title Color and Font Size...
	TitleStyle = io_lib:format("&chts=~s,~b", [zp_convert:to_list(Color), FontSize]),
	
	% Size...
	Size = io_lib:format("&chs=~bx~b", [Width, Height]),
	
	% Grid...
	Grid = io_lib:format("&chg=~s,~s,~b,~b", [
        		zp_convert:to_list(zp_utils:coalesce([GridX, 0])),
        		zp_convert:to_list(zp_utils:coalesce([GridY, 0])),
        		GridLineLength,
        		GridBlankLength
        	]),
	
	% Background Colors...
	BGColors = io_lib:format("&chf=bg,s,~s|c,s,~s", [
        		zp_convert:to_list(BGColor), 
        		zp_convert:to_list(ChartColor)
        	]),
	
	% Legend Location...
	LegendLocation = "&chdlp=" 
	                ++  case LegendLoc of
                	    	"top"    -> "t";
                    		"left"   -> "l";
                    		"bottom" -> "b";
                    		"right"  -> "r";
                	    	top    -> "t";
                    		left   -> "l";
                    		bottom -> "b";
                    		_  -> "r"
                    	end,
	
	% Axes...
	Axes = case AxesArg of 
    		undefined   -> [];
    		[]          -> [];
    		<<>>        -> <<>>;
    		AxesRecords ->			
    			ProcessedAxes = [process_axis(N - 1, lists:nth(N, AxesRecords)) || N <- lists:seq(1, length(AxesRecords))],
    			AxesPositions = "&chxt=" ++ string:join([X || [X, _, _] <- ProcessedAxes], ","),
    			AxesLabels    = "&chxl=" ++ string:join([X || [_, X, _] <- ProcessedAxes], "|"),
    			AxesColors    = "&chxs=" ++ string:join([X || [_, _, X] <- ProcessedAxes], "|"),
    			AxesPositions ++ AxesLabels ++ AxesColors
    	end,
	
	% Data...
	Data = case DataArg of
        		undefined   -> MaxValueLength=0, [];
        		[]          -> MaxValueLength=0, [];
        		<<>>        -> MaxValueLength=0, [];
        		DataRecords ->
        			ProcessedData = [process_data(N -1, lists:nth(N, DataRecords)) || N <- lists:seq(1, length(DataRecords))],
        			DataColors  = "&chco="  ++ string:join([X || [X, _, _, _, _, _] <- ProcessedData], ","),
        			DataLegends = "&chdl="  ++ string:join([X || [_, X, _, _, _, _] <- ProcessedData], "|"),
        			DataScales  = "&chds="  ++ string:join([X || [_, _, X, _, _, _] <- ProcessedData], ","),
        			DataStyles  = "&chls="  ++ string:join([X || [_, _, _, X, _, _] <- ProcessedData], "|"),
        			DataValues  = "&chd=t:" ++ string:join([X || [_, _, _, _, X, _] <- ProcessedData], "|"),
        			MaxValueLength = lists:max([X || [_, _, _, _, _, X] <- ProcessedData]),
        			DataLegends1 = case string:strip(DataLegends, both, $|) of
        				"&chdl=" -> [];
        				_ -> DataLegends
        			end,				
			
        			DataColors ++ DataLegends1 ++ DataScales ++ DataValues ++ DataStyles
        	end,
	
	% Calculate bar size...
	BarSize = case MaxValueLength of 
        		0 -> [];
        		_ -> 
        			DataGroupsLength = length(Data),
        			GroupSpacerPixels = MaxValueLength * BarGroupSpace,
        			BarSpacerPixels = MaxValueLength * (DataGroupsLength * BarSpace),
        			AvailablePixels = case Type of 
        				stacked_horizontal_bar -> Height;
        				grouped_horizontal_bar -> Height;
        				stacked_vertical_bar -> Width;
        				grouped_vertical_bar -> Width;
        				_ -> 0
        			end,
        			IndividualBarSize = (AvailablePixels - GroupSpacerPixels - BarSpacerPixels) / (DataGroupsLength * MaxValueLength),
        			io_lib:format("&chbh=~b,~b,~b", [trunc(IndividualBarSize), BarSpace, BarGroupSpace])
        	end,

    ImageUri = lists:flatten([Path, Type, TitleText, TitleStyle, Size, Grid, BGColors, LegendLocation, BarSize, Axes, Data]),
	{ok, zp_tags:render_tag(
	        <<"image">>, 
	        [
        		{<<"id">>,     Id},
        		{<<"name">>,   Id},
        		{<<"class">>,  [<<"google_chart">>, Class]},
        		{<<"style">>,  Style},
        		{<<"src">>,    ImageUri},
        		{<<"width">>,  Width},
        		{<<"height">>, Height}
        	])}.


process_axis(N, Axis) ->
    FontSize = proplists:get_value(font_size, Axis, 10),
    Color    = proplists:get_value(color, Axis, "909090"),
    
	Position = case zp_convert:to_atom(proplists:get_value(position, Axis)) of
            		top    -> "t";
            		right  -> "r";
            		bottom -> "x";
            		left   -> "y";
            		OtherPosition -> erlang:error({unknown_axis_position, OtherPosition})
            	end,

	StringLabels = [zp_convert:to_list(X) || X <- proplist:get_value(labels, Axis)],
	Labels       = integer_to_list(N) ++ ":|" ++ string:join(StringLabels, "|"),
	Style        = io_lib:format("~b,~s,~b", [N, zp_convert:to_list(Color), FontSize]),
	[Position, Labels, Style].
	
process_data(_N, Data) ->
    LineWidth    = proplists:get_value(line_width,  Data, 1),
    LineLength   = proplists:get_value(line_length, Data, 1),
    BlankLength  = proplists:get_value(blank_width, Data, 0),
    MinValue     = proplists:get_value(min_value, Data, 0),
    MaxValue     = proplists:get_value(max_value, Data, 100),
	Color        = zp_convert:to_list(proplists:get_value(color, Data)),
	Legend       = zp_convert:to_list(proplists:get_value(legend, Data)),
    Values       = proplists:get_value(values, Data, 100),
    
	Scale        = io_lib:format("~b,~b", [MinValue,MaxValue]),
	StringValues = [zp_convert:to_list(X) || X <- Values],
	Values       = string:join(StringValues, ","),
	Styles       = io_lib:format("~b,~b,~b", [LineWidth, LineLength, BlankLength]),
	[Color, Legend, Scale, Styles, Values, length(StringValues)].
