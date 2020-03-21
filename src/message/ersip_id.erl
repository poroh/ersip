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

-export([token/1,
         word/1,
         alphanum/1]).

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

-spec word(binary()) -> binary().
word(Bin) ->
    encode(Bin, word_chars()).

-spec alphanum(binary()) -> binary().
alphanum(Bin) ->
    encode(Bin, alphanum_chars()).

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec encode(binary(), char_table()) -> binary().
encode(Bin, TokenTab) ->
    encode_impl(Bin, TokenTab, 0, []).

-spec encode_impl(binary(), char_table(), non_neg_integer(), [byte()]) -> binary().
encode_impl(<<>>, _, 0, L) ->
    list_to_binary(L);
encode_impl(<<>>, #char_table{size = S, tfun = TF} = CT, X, L) when X < S ->
    encode_impl(<<>>, CT, 0, [TF(X) | L]);
encode_impl(Bin, #char_table{size = S, tfun = TF} = CT, X, L) when X >= S ->
    encode_impl(Bin, CT, X div S, [TF(X rem S) | L]);
encode_impl(<<Byte:8, Rest/binary>>, #char_table{size = S} =CT, X, L) when X =< S ->
    encode_impl(Rest, CT, X * 256 + Byte, L).

-define(SMALL_ALPHA_SIZE, ($z-$a+1)).
-define(CAP_ALPHA_SIZE,   ($Z-$A+1)).
-define(ALPHA_SIZE,       (?SMALL_ALPHA_SIZE + ?CAP_ALPHA_SIZE)).
-define(DIGIT_SIZE,       ($9-$0+1)).
-define(OTHER_SIZE,       9).

-spec token_chars() -> char_table().
token_chars() ->
    #char_table{size = ?ALPHA_SIZE + ?DIGIT_SIZE + ?OTHER_SIZE,
                tfun = fun token_translate/1
               }.

-define(TOKEN_DIGIT_SHIFT, 9).
-define(TOKEN_SMALL_ALPHA_SHIFT, (?TOKEN_DIGIT_SHIFT + ?DIGIT_SIZE)).
-define(TOKEN_CAP_ALPHA_SHIFT, (?TOKEN_SMALL_ALPHA_SHIFT + ?SMALL_ALPHA_SIZE)).

token_translate(0) -> $-;
token_translate(1) -> $.;
token_translate(2) -> $!;
token_translate(3) -> $*;
token_translate(4) -> $_;
token_translate(5) -> $+;
token_translate(6) -> $`;
token_translate(7) -> $';
token_translate(8) -> $~;
token_translate(X) when X >= ?TOKEN_DIGIT_SHIFT,
                        X < ?TOKEN_SMALL_ALPHA_SHIFT ->
    X - ?TOKEN_DIGIT_SHIFT + $0;
token_translate(X) when X >= ?TOKEN_SMALL_ALPHA_SHIFT,
                        X < ?TOKEN_CAP_ALPHA_SHIFT ->
    X - ?TOKEN_SMALL_ALPHA_SHIFT + $a;
token_translate(X) when X >= ?TOKEN_CAP_ALPHA_SHIFT,
                        X < ?TOKEN_CAP_ALPHA_SHIFT + ?CAP_ALPHA_SIZE ->
    X - ?TOKEN_CAP_ALPHA_SHIFT + $A.


%% word     =  1*(alphanum /
%%             "-" / "." / "!" / "%" / "*" /
%%             "_" / "+" / "`" / "'" / "~" /
%%             "(" / ")" / "<" / ">" /
%%             ":" / "\" / DQUOTE /
%%             "/" / "[" / "]" / "?" /
%%             "{" / "}" )
-define(WORD_OTHER_SIZE, 22).
-spec word_chars() -> char_table().
word_chars() ->
    #char_table{size = ?ALPHA_SIZE + ?DIGIT_SIZE + ?WORD_OTHER_SIZE,
                tfun = fun word_translate/1
               }.

-define(WORD_DIGIT_SHIFT, ?WORD_OTHER_SIZE).
-define(WORD_SMALL_ALPHA_SHIFT, (?WORD_DIGIT_SHIFT + ?DIGIT_SIZE)).
-define(WORD_CAP_ALPHA_SHIFT, (?WORD_SMALL_ALPHA_SHIFT + ?SMALL_ALPHA_SIZE)).

word_translate(0)  -> $-;
word_translate(1)  -> $.;
word_translate(2)  -> $!;
word_translate(3)  -> $*;
word_translate(4)  -> $_;
word_translate(5)  -> $+;
word_translate(6)  -> $`;
word_translate(7)  -> $';
word_translate(8)  -> $~;
word_translate(9)  -> $(;
word_translate(10) -> $);
word_translate(11) -> $<;
word_translate(12) -> $>;
word_translate(13) -> $:;
word_translate(14) -> $\\;
word_translate(15) -> $";
word_translate(16) -> $/;
word_translate(17) -> $[;
word_translate(18) -> $];
word_translate(19) -> $?;
word_translate(20) -> ${;
word_translate(21) -> $};
word_translate(X) when X >= ?WORD_DIGIT_SHIFT,
                        X < ?WORD_SMALL_ALPHA_SHIFT ->
    X - ?WORD_DIGIT_SHIFT + $0;
word_translate(X) when X >= ?WORD_SMALL_ALPHA_SHIFT,
                        X < ?WORD_CAP_ALPHA_SHIFT ->
    X - ?WORD_SMALL_ALPHA_SHIFT + $a;
word_translate(X) when X >= ?WORD_CAP_ALPHA_SHIFT,
                        X < ?WORD_CAP_ALPHA_SHIFT + ?CAP_ALPHA_SIZE ->
    X - ?WORD_CAP_ALPHA_SHIFT + $A.


-spec alphanum_chars() -> char_table().
alphanum_chars() ->
    #char_table{size = ?ALPHA_SIZE + ?DIGIT_SIZE,
                tfun = fun alphanum_translate/1
               }.

-define(ALPHANUM_SMALL_ALPHA_SHIFT, ?DIGIT_SIZE).
-define(ALPHANUM_CAP_ALPHA_SHIFT, (?ALPHANUM_SMALL_ALPHA_SHIFT + ?SMALL_ALPHA_SIZE)).

alphanum_translate(X) when X < ?ALPHANUM_SMALL_ALPHA_SHIFT ->
    X + $0;
alphanum_translate(X) when X >= ?ALPHANUM_SMALL_ALPHA_SHIFT,
                        X < ?ALPHANUM_CAP_ALPHA_SHIFT ->
    X - ?ALPHANUM_SMALL_ALPHA_SHIFT + $a;
alphanum_translate(X) when X >= ?ALPHANUM_CAP_ALPHA_SHIFT,
                           X < ?ALPHANUM_CAP_ALPHA_SHIFT + ?CAP_ALPHA_SIZE ->
    X - ?ALPHANUM_CAP_ALPHA_SHIFT + $A.
