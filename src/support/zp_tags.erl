%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell <marc@worrell.nl>
%%
%% Based on code Copyright (c) 2008-2009 Rusty Klophaus
%% Original author Tom McNulty <tom.mcnulty@cetiforge.com>
%%
%% @doc Generate a XHTML element with attributes, correctly escape all attributes.

-module (zp_tags).

-include("include/zophrenic.hrl").

-export([render_tag/2, render_tag/3, render_tag/4]).
-export([optional_escape/1]).

%% @doc Render a tag with properties, return the tag text. div has special handling as <div/> is not allowed.
render_tag("div", Props) ->
    render_tag(<<"div">>, Props);
render_tag(<<"div">>, Props) ->
	[<<"<div ">>, write_props(Props), <<"></div>">> ];
render_tag(TagName, Props) ->
	[$<, TagName, write_props(Props), $/, $> ].


%% @doc Render a tag into the context
render_tag("div", Props, Context) ->
    render_tag(<<"div">>, Props, Context);
render_tag(<<"div">>, Props, Context) ->
	Render   = [<<"<div ">>, write_props(Props), <<"></div>">> ],
	zp_render:render(Render, Context);
render_tag(TagName, Props, Context) ->
	Render   = [$<, TagName, write_props(Props), $/, $> ],
	zp_render:render(Render, Context).
	

%%% Tags with child content %%%
render_tag(TagName, Props, undefined, Context) ->
    render_tag(TagName, Props, Context);
render_tag(TagName, Props, [], Context) ->
    render_tag(TagName, Props, Context);
render_tag(TagName, Props, Content, Context) ->
	Render   = [ $<, TagName, write_props(Props), $>, Content, $<, $/, TagName, $> ],
	zp_render:render(Render, Context).
   
    
%%% Property display functions %%%
    
write_props(Props) ->
    [ display_property(optional_escape_property(P)) || P <- Props ].

display_property({_Prop, undefined}) ->
    [];
display_property({Prop, V}) when is_atom(Prop) ->
    display_property({list_to_binary(atom_to_list(Prop)), V});
display_property({_, []}) ->
    [];
display_property({Prop, Value}) when is_integer(Value) ->
    [32, Prop, $=, $', list_to_binary(integer_to_list(Value)), $'];
display_property({Prop, Value}) when is_float(Value) ->
	[32, Prop, $=, $', list_to_binary(io_lib:format("~f",Value)), $'];
display_property({Prop, Value}) when is_list(Value) ->
    case io_lib:char_list(Value) of
        true -> 
            [32, Prop, $=, $', Value, $'];
        false ->
            [32, Prop, $=, $', zp_utils:combine_defined(32, Value), $']
	end;
display_property({Prop, Value}) when is_atom(Value) ->
	[32, Prop, $=, $', atom_to_list(Value), $'];
display_property({Prop, Value}) ->
	[32, Prop, $=, $', Value, $'].


optional_escape_property({href, Uri}) -> {href, optional_escape(Uri)};
optional_escape_property({src, Uri}) -> {src, optional_escape(Uri)};
optional_escape_property({<<"src">>, Uri}) -> {<<"src">>, optional_escape(Uri)};
optional_escape_property({<<"href">>, Uri}) -> {<<"href">>, optional_escape(Uri)};
optional_escape_property(P) -> P.

optional_escape(S) when is_list(S) ->
    case string:chr(S, $&) of
        0 -> S;
        _ ->
            case string:str(S, "&amp;") of
                0 -> mochiweb_html:escape(S);
                _ -> S
            end
    end;
optional_escape(S) -> S.
