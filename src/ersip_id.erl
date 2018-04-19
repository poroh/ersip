%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% ID generation functions
%%
%% Idea of these functions is to generate header-capable strings from
%% some entropy (random or not random binary).
%%

-module(ersip_id).

-export([token/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(char_table, {size :: pos_integer(),
                     tfun :: fun((Byte :: non_neg_integer()) -> char())
                    }).
-type char_table() :: #char_table{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc generate SIP token capable binary.
-spec token(binary()) -> binary().
token(Bin) ->
    encode(Bin, token_chars()).

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec encode(binary(), char_table()) -> binary().
encode(Bin, TokenTab) ->
    encode_impl(Bin, TokenTab, 0, []).

-spec encode_impl(binary(), char_table(), non_neg_integer(), list()) -> binary().
encode_impl(<<>>, _, 0, L) ->
    list_to_binary(L);
encode_impl(<<>>, #char_table{size = S, tfun = TF} = CT, X, L) when X < S ->
    encode_impl(<<>>, CT, 0, [TF(X) | L]);
encode_impl(Bin, #char_table{size = S, tfun = TF} = CT, X, L) when X >= S ->
    encode_impl(Bin, CT, X div S, [ TF(X rem S) | L ]);
encode_impl(<<Byte:8, Rest/binary>>, #char_table{size = S} =CT, X, L) when X =< S ->
    encode_impl(Rest, CT, X * 256 + Byte, L).

-define(SMALL_ALPHA_SIZE, ($z-$a+1)).
-define(CAP_ALPHA_SIZE,   ($Z-$A+1)).
-define(ALPHA_SIZE,       (?SMALL_ALPHA_SIZE + ?CAP_ALPHA_SIZE)).
-define(DIGIT_SIZE,       ($9-$0+1)).
-define(OTHER_SIZE,       10).

-spec token_chars() -> char_table().
token_chars() ->
    #char_table{size = ?ALPHA_SIZE + ?DIGIT_SIZE + ?OTHER_SIZE,
                tfun = fun token_translate/1
               }.

-define(DIGIT_SHIFT, 10).
-define(SMALL_ALPHA_SHIFT, (?DIGIT_SHIFT + ?DIGIT_SIZE)).
-define(CAP_ALPHA_SHIFT, (?SMALL_ALPHA_SHIFT + ?SMALL_ALPHA_SIZE)).

token_translate(0) -> $-;
token_translate(1) -> $.;
token_translate(2) -> $!;
token_translate(3) -> $%;
token_translate(4) -> $*;
token_translate(5) -> $_;
token_translate(6) -> $+;
token_translate(7) -> $`;
token_translate(8) -> $';
token_translate(9) -> $~;
token_translate(X) when X >= ?DIGIT_SHIFT,
                        X < ?SMALL_ALPHA_SHIFT ->
    X - ?DIGIT_SHIFT + $0;
token_translate(X) when X >= ?SMALL_ALPHA_SHIFT,
                        X < ?CAP_ALPHA_SHIFT ->
    X - ?SMALL_ALPHA_SHIFT + $a;
token_translate(X) when X >= ?CAP_ALPHA_SHIFT,
                        X < ?CAP_ALPHA_SHIFT + ?CAP_ALPHA_SIZE ->
    X - ?CAP_ALPHA_SHIFT + $A.
