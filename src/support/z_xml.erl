%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2009 Marc Worrell
%% @date 2009-08-04
%%
%% @doc Utility functions for xml processing.

-module(z_xml).
-author("Marc Worrell <marc@worrell.nl").

%% interface functions
-export([
    escape/1
]).


%% @doc Escape a html text to valid a xml text so that it can be transported in XML.  Translates control characters to 
%% spaces, except for TAB, CR and LF.  Escapes the characters $&, $<, $>, $" and $'.
%% @spec escape(iolist()) -> iolist()
escape(undefined) -> 
    undefined;
escape(<<>>) -> 
    <<>>;
escape([]) ->
    [];
escape(L) when is_list(L) ->
    escape(list_to_binary(L));
escape(B) when is_binary(B) ->
    escape(B, <<>>).

    escape(<<>>, Acc) -> 
        Acc;
    escape(<<$&, T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "&#38;">>);
    escape(<<$<, T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "&#60;">>);
    escape(<<$>, T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "&#62;">>);
    escape(<<$", T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "&#34;">>);
    escape(<<$', T/binary>>, Acc) ->
        escape(T, <<Acc/binary, "&#39;">>);
    escape(<<C, T/binary>>, Acc) when C == 9; C == 10; C == 13 ->
        escape(T, <<Acc/binary, C>>);
    escape(<<C, T/binary>>, Acc) when C < 32 ->
        escape(T, <<Acc/binary, 32>>);
    escape(<<C, T/binary>>, Acc) ->
        escape(T, <<Acc/binary, C>>).


