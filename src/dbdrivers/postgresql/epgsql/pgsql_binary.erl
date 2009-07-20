%%% Copyright (C) 2008 - Will Glozer.  All rights reserved.
%%% 20090311 Marc Worrell - Added support for encoding terms and lists of terms in bytea values

-module(pgsql_binary).

-export([encode/3, decode/3, supports/1]).

-include_lib("zotonic.hrl").

-define(int32, 1/big-signed-unit:32).
-define(TERM_MAGIC_NUMBER, 16#01326A3A:1/big-unsigned-unit:32).


encode(_Any, null, _IntegerDatetime)  -> <<-1:?int32>>;
encode(_Any, undefined, _IntegerDatetime)  -> <<-1:?int32>>;
encode(bool, <<1>>, _IntegerDatetime) -> <<1:?int32, 1:1/big-signed-unit:8>>;
encode(bool, <<>>, _IntegerDatetime)  -> <<1:?int32, 0:1/big-signed-unit:8>>;
encode(bool, true, _IntegerDatetime)  -> <<1:?int32, 1:1/big-signed-unit:8>>;
encode(bool, false, _IntegerDatetime) -> <<1:?int32, 0:1/big-signed-unit:8>>;
encode(int2, N, IntegerDatetime) when is_binary(N); is_list(N) -> encode(int2, z_convert:to_integer(N), IntegerDatetime);
encode(int4, N, IntegerDatetime) when is_binary(N); is_list(N) -> encode(int4, z_convert:to_integer(N), IntegerDatetime);
encode(int8, N, IntegerDatetime) when is_binary(N); is_list(N) -> encode(int8, z_convert:to_integer(N), IntegerDatetime);
encode(int2, N, _IntegerDatetime)     -> <<2:?int32, N:1/big-signed-unit:16>>;
encode(int4, N, _IntegerDatetime)     -> <<4:?int32, N:1/big-signed-unit:32>>;
encode(int8, N, _IntegerDatetime)     -> <<8:?int32, N:1/big-signed-unit:64>>;
encode(float4, N, _IntegerDatetime)   -> <<4:?int32, N:1/big-float-unit:32>>;
encode(float8, N, _IntegerDatetime)   -> <<8:?int32, N:1/big-float-unit:64>>;
encode(bpchar, C, _IntegerDatetime) when is_integer(C) -> <<1:?int32, C:1/big-unsigned-unit:8>>;
encode(bpchar, B, _IntegerDatetime) when is_binary(B)  -> <<(byte_size(B)):?int32, B/binary>>;
encode(Type, B, IntegerDatetime) when Type == time; Type == timetz          -> pgsql_datetime:encode(Type, B, IntegerDatetime);
encode(Type, B, IntegerDatetime) when Type == date; Type == timestamp       -> pgsql_datetime:encode(Type, B, IntegerDatetime);
encode(Type, B, IntegerDatetime) when Type == timestamptz; Type == interval -> pgsql_datetime:encode(Type, B, IntegerDatetime);
encode(bytea, B, _IntegerDatetime) when is_binary(B)   -> <<(byte_size(B)):?int32, B/binary>>;
encode(text, B, _IntegerDatetime) when is_binary(B)    -> <<(byte_size(B)):?int32, B/binary>>;
encode(varchar, B, _IntegerDatetime) when is_binary(B) -> <<(byte_size(B)):?int32, B/binary>>;
encode(bytea, T, IntegerDatetime) when is_tuple(T)   -> 
    B = term_to_binary(T),
    encode(bytea, <<?TERM_MAGIC_NUMBER, B/binary>>, IntegerDatetime);
encode(bytea, [T|_Rest]=L, IntegerDatetime) when is_tuple(T)   -> 
    B = term_to_binary(L),
    encode(bytea, <<?TERM_MAGIC_NUMBER, B/binary>>, IntegerDatetime);
encode(Type, A, IntegerDatetime) when is_atom(A)      -> encode(Type, atom_to_list(A), IntegerDatetime);
encode(Type, L, IntegerDatetime) when is_list(L)      -> encode(Type, iolist_to_binary(L), IntegerDatetime);
encode(_Type, _Value, _IntegerDatetime)                -> {error, unsupported}.

decode(bool, <<1:1/big-signed-unit:8>>, _IntegerDatetime)     -> true;
decode(bool, <<0:1/big-signed-unit:8>>, _IntegerDatetime)     -> false;
decode(bool, <<"t">>, _IntegerDatetime) -> true;
decode(bool, <<"f">>, _IntegerDatetime) -> false;
decode(bpchar, <<C:1/big-unsigned-unit:8>>, _IntegerDatetime) -> C;
decode(int2, <<N:1/big-signed-unit:16>>, _IntegerDatetime)    -> N;
decode(int4, <<N:1/big-signed-unit:32>>, _IntegerDatetime)    -> N;
decode(int8, <<N:1/big-signed-unit:64>>, _IntegerDatetime)    -> N;
decode(float4, <<N:1/big-float-unit:32>>, _IntegerDatetime)   -> N;
decode(float8, <<N:1/big-float-unit:64>>, _IntegerDatetime)   -> N;
decode(record, <<_:?int32, Rest/binary>>, IntegerDatetime)   -> list_to_tuple(decode_record(Rest, IntegerDatetime, []));
decode(Type, B, IntegerDatetime) when Type == time; Type == timetz          -> pgsql_datetime:decode(Type, B, IntegerDatetime);
decode(Type, B, IntegerDatetime) when Type == date; Type == timestamp       -> pgsql_datetime:decode(Type, B, IntegerDatetime);
decode(Type, B, IntegerDatetime) when Type == timestamptz; Type == interval -> pgsql_datetime:decode(Type, B, IntegerDatetime);
decode(bytea, <<?TERM_MAGIC_NUMBER, B/binary>>, _IntegerDatetime) -> binary_to_term(B);
decode(_Other, Bin, _IntegerDatetime) -> Bin.

decode_record(<<>>, _IntegerDatetime, Acc) ->
    lists:reverse(Acc);
decode_record(<<_Type:?int32, -1:?int32, Rest/binary>>, IntegerDatetime, Acc) ->
    decode_record(Rest, IntegerDatetime, [undefined | Acc]);
decode_record(<<Type:?int32, Len:?int32, Value:Len/binary, Rest/binary>>, IntegerDatetime, Acc) ->
    Value2 = decode(pgsql_types:oid2type(Type), Value, IntegerDatetime),
    decode_record(Rest, IntegerDatetime, [Value2 | Acc]).

supports(bool)    -> true;
supports(bpchar)  -> true;
supports(int2)    -> true;
supports(int4)    -> true;
supports(int8)    -> true;
supports(float4)  -> true;
supports(float8)  -> true;
supports(bytea)   -> true;
supports(text)    -> true;
supports(varchar) -> true;
supports(record)  -> true;
supports(date)    -> true;
supports(time)    -> true;
supports(timetz)  -> true;
supports(timestamp)   -> true;
supports(timestamptz) -> true;
supports(interval)    -> true;
supports(_Type)       -> false.
