%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Quoted string
%%%

-module(ersip_quoted_string).

-export([skip/1,
         quote/1
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

%%===================================================================
%% Internal implementation
%%===================================================================

-spec skip_impl(binary(), skip_impl_state()) -> {ok, Rest :: binary()} | error.
skip_impl(<<>>, _)                   -> error;
skip_impl(<<"\"", R/binary>>, start) -> skip_impl(R, raw);
skip_impl(_, start)                  -> error;
skip_impl(<<"\"", R/binary>>, raw)   -> {ok, R};
skip_impl(<<"\\", R/binary>>, raw)   -> skip_impl(R, escaped);
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
