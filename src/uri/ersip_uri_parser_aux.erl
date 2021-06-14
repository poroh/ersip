%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP URI parser auxilary functions
%%%

-module(ersip_uri_parser_aux).

-export([unquote_hex/1,
         split_scheme/1]).

%%===================================================================
%% API
%%===================================================================

-spec unquote_hex(binary()) -> binary().
unquote_hex(Bin) ->
    do_unquote_hex(Bin, Bin, {0, 0}, []).

-spec split_scheme(binary()) -> {binary(), binary()}.
split_scheme(Bin) ->
    case binary:split(Bin, <<":">>) of
        [Scheme, Suffix] ->
            {ersip_bin:to_lower(Scheme), Suffix};
        [Suffix] ->
            {<<>>, Suffix}
    end.

%%===================================================================
%% Implementation
%%===================================================================

-include("ersip_sip_abnf.hrl").

-spec do_unquote_hex(binary(), binary(), {non_neg_integer(), integer()}, iolist()) -> binary().
do_unquote_hex(<<>>, Orig, {_, Len}, []) when Len == byte_size(Orig) ->
    Orig;
do_unquote_hex(<<>>, _, {_, 0}, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
do_unquote_hex(<<>>, Orig, Part, Acc) ->
    PartBin = binary:part(Orig, Part),
    iolist_to_binary(lists:reverse([PartBin | Acc]));
do_unquote_hex(<<$%, H1:8, H2:8, Rest/binary>>, Orig, {Pos, Len} = Part, Acc) when ?is_HEXDIG(H1) andalso ?is_HEXDIG(H2) ->
    Char = 16 * hex_char_to_num(H1) + hex_char_to_num(H2),
    case Len of
        0 ->
            do_unquote_hex(Rest, Orig, {Pos + 3, 0}, [Char | Acc]);
        _ ->
            PartBin = binary:part(Orig, Part),
            do_unquote_hex(Rest, Orig, {Pos + Len + 3, 0}, [Char, PartBin | Acc])
    end;
do_unquote_hex(<<_:8, Rest/binary>>, Orig, {Pos, Len}, Acc) ->
    do_unquote_hex(Rest, Orig, {Pos, Len+1}, Acc).

-spec hex_char_to_num(char()) -> 0..15.
hex_char_to_num(X) when X >= $0 andalso X =< $9 ->
    X - $0;
hex_char_to_num(X) when X >= $A andalso X =< $F ->
    X - $A + 10;
hex_char_to_num(X) when X >= $a andalso X =< $f ->
    X - $a + 10.

