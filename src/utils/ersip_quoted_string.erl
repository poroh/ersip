%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Quoted string
%%%

-module(ersip_quoted_string).

-export([skip/1,
         quote/1,
         unquote/1,
         unquoting_parse/1
        ]).

%%===================================================================
%% Types
%%===================================================================

-type skip_result() :: {ok, Rest :: binary()} | error.
-type skip_impl_state() :: start | raw | escaped.

%%===================================================================
%% API
%%===================================================================

-spec skip(binary()) -> skip_result().
skip(String) ->
    skip_impl(String, start).

-spec quote(binary()) -> binary().
quote(String) ->
    <<"\"", (escape(String))/binary, "\"">>.

-spec unquote(binary()) -> binary().
unquote(<<"\"", _/binary>> = V) ->
    {ok, Unquoted, <<>>} = unquoting_parse(V),
    Unquoted;
unquote(V) ->
    V.

-spec unquoting_parse(binary()) -> ersip_parser_aux:parse_result(binary()).
unquoting_parse(Quoted) ->
    Trimmed = ersip_bin:trim_head_lws(Quoted),
    unquoting_parse_impl(Trimmed, start, []).

%%===================================================================
%% Internal implementation
%%===================================================================

-spec skip_impl(binary(), skip_impl_state()) -> {ok, Rest :: binary()} | error.
skip_impl(<<>>, _)                   -> error;
skip_impl(<<"\"", R/binary>>, start) -> skip_impl(R, raw);
skip_impl(_, start)                  -> error;
skip_impl(<<"\"", R/binary>>, raw)   -> {ok, R};
skip_impl(<<"\\", R/binary>>, raw)   -> skip_impl(R, escaped);
skip_impl(<<Byte:8, _/binary>>, raw) when Byte == 16#0A orelse Byte == 16#0D -> error;
skip_impl(<<_:8, R/binary>>, raw)    -> skip_impl(R, raw);
skip_impl(<<Byte:8, R/binary>>, escaped) when Byte =< 16#7F -> skip_impl(R, raw);
skip_impl(<<_:8, _/binary>>, escaped) -> error.

-spec escape(binary()) -> binary().
escape(Binary) ->
    escape_impl(Binary, [], []).

-spec escape_impl(binary(), [non_neg_integer()], [binary()]) -> binary().
escape_impl(<<>>, Bytes, Acc) ->
    AccBin = << <<X:8>> || X <- lists:reverse(Bytes) >>,
    iolist_to_binary(lists:reverse([AccBin | Acc]));
escape_impl(<<$", R/binary>>, Bytes, Acc) ->
    AccBin = << <<X:8>> || X <- lists:reverse(Bytes) >>,
    escape_impl(R, [], ["\\\"", AccBin | Acc]);
escape_impl(<<$\\, R/binary>>, Bytes, Acc) ->
    AccBin = << <<X:8>> || X <- lists:reverse(Bytes) >>,
    escape_impl(R, [], ["\\\\", AccBin | Acc]);
escape_impl(<<Byte:8, R/binary>>, Bytes, Acc) ->
    escape_impl(R, [Byte | Bytes], Acc).


%% @private
-spec unquoting_parse_impl(binary(), start | raw | escaped, [binary()]) -> ersip_parser_aux:parse_result(binary()).
unquoting_parse_impl(<<>>, _, _) ->
    {error, {invalid_quoted_string, unexpected_end}};
unquoting_parse_impl(<<"\"", R/binary>>, start, Acc) ->
    unquoting_parse_impl(R, raw, Acc);
unquoting_parse_impl(_, start, _) ->
    {error, {invalid_quoted_string, no_quote}};
unquoting_parse_impl(<<"\"", R/binary>>, raw, Acc) ->
    {ok, iolist_to_binary(lists:reverse(Acc)), R};
unquoting_parse_impl(<<"\\", R/binary>>, raw, Acc) ->
    unquoting_parse_impl(R, escaped, Acc);
unquoting_parse_impl(B, raw, Acc) ->
    {ok, Len} = utf8_len(B),
    <<Char:Len/binary, Rest/binary>> = B,
    unquoting_parse_impl(Rest, raw, [Char | Acc]);
unquoting_parse_impl(<<Byte:8, R/binary>>, escaped, Acc) when Byte =< 16#7F ->
    unquoting_parse_impl(R, raw, [Byte | Acc]);
unquoting_parse_impl(<<Byte:8, _/binary>>, escaped, _) ->
    {error, {invalid_quoted_string, {bad_char, Byte}}}.

%% @private
%% @doc get length of the first UTF8 character in binary
-spec utf8_len(binary()) -> {ok, Len :: 1..6} | error.
utf8_len(<<ASCII:8, _/binary>>) when ASCII =< 16#7F ->
    {ok, 1};
utf8_len(<<UTF8_1:8, _:8, _/binary>>)
  when UTF8_1 >= 16#C0 andalso UTF8_1 =< 16#DF  ->
    {ok, 2};
utf8_len(<<UTF8_2:8, _:16, _/binary>>)
  when UTF8_2 >= 16#E0 andalso UTF8_2 =< 16#EF ->
    {ok, 3};
utf8_len(<<UTF8_3:8, _:24, _/binary>>)
  when UTF8_3 >= 16#F0 andalso UTF8_3 =< 16#F7 ->
    {ok, 4};
utf8_len(<<_:8, _/binary>>) ->
    {ok, 1}.
