%%
%% Copyright (c) 2020 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media attribute rtpmap
%%

-module(ersip_sdp_attr_rtpmap).

-export([new/3, new/4]).

-export([payload_type/1]).
-export([encoding_name/1]).
-export([clock_rate/1]).
-export([encoding_params/1]).
-export([set_payload_type/2]).
-export([set_encoding_name/2]).
-export([set_clock_rate/2]).
-export([set_encoding_params/2]).

-export([parse/1]).
-export([assemble/1]).
-export([assemble_bin/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(rtpmap, {
    type           :: payload_type(),
    encoding_name  :: encoding_name(),
    clock_rate     :: clock_rate(),
    params         :: encoding_params()
}).

-type payload_type()     :: pos_integer().
-type encoding_name()    :: binary().
-type clock_rate()       :: pos_integer().
-type encoding_params()  :: pos_integer() | undefined.
-type rtpmap()           :: #rtpmap{}.
-type parse_result()     :: {ok, rtpmap()} |
                            {error, {invalid_rtpmap_candidate, term()}}.
-type parse_result(T)    :: ersip_parser_aux:parse_result(T).

-export_type([
    payload_type/0,
    clock_rate/0,
    encoding_name/0,
    encoding_params/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(payload_type(), encoding_name(), clock_rate()) -> rtpmap().
new(Type, Name, ClockRate) ->
    new(Type, Name, ClockRate, undefined).

-spec new(payload_type(), encoding_name(), clock_rate(), encoding_params()) -> rtpmap().
new(Type, Name, ClockRate, Params) ->
    #rtpmap{
        type = Type,
        encoding_name = Name,
        clock_rate = ClockRate,
        params = Params
    }.

-spec payload_type(rtpmap()) -> payload_type().
payload_type(#rtpmap{type = Type}) ->
    Type.

-spec encoding_name(rtpmap()) -> encoding_name().
encoding_name(#rtpmap{encoding_name = Name}) ->
    Name.

-spec clock_rate(rtpmap()) -> clock_rate().
clock_rate(#rtpmap{clock_rate = ClockRate}) ->
    ClockRate.

-spec encoding_params(rtpmap()) -> encoding_params().
encoding_params(#rtpmap{params = Params}) ->
    Params.

-spec set_payload_type(payload_type(), rtpmap()) -> rtpmap().
set_payload_type(Type, Rtpmap) ->
    Rtpmap#rtpmap{type = Type}.

-spec set_encoding_name(encoding_name(), rtpmap()) -> rtpmap().
set_encoding_name(Name, Rtpmap) ->
    Rtpmap#rtpmap{encoding_name = Name}.

-spec set_clock_rate(clock_rate(), rtpmap()) -> rtpmap().
set_clock_rate(ClockRate, Rtpmap) ->
    Rtpmap#rtpmap{clock_rate = ClockRate}.

-spec set_encoding_params(encoding_params(), rtpmap()) -> rtpmap().
set_encoding_params(Params, Rtpmap) ->
    Rtpmap#rtpmap{params = Params}.

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    do_parse_rtpmap(Bin).

-spec assemble(rtpmap()) -> iolist().
assemble(Rtpmap) ->
    assemble_rtpmap(Rtpmap).

-spec assemble_bin(rtpmap()) -> binary().
assemble_bin(Rtpmap) ->
    iolist_to_binary(assemble(Rtpmap)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================
%% https://datatracker.ietf.org/doc/html/rfc8866#section-6.6
%%
%% a=rtpmap:<payload type> <encoding name>/<clock rate> [/<encoding parameters>]
%%

-spec do_parse_rtpmap(binary()) -> parse_result().
do_parse_rtpmap(Bin) ->
    Parsers = [
        fun ersip_parser_aux:parse_non_neg_int/1,
        fun ersip_parser_aux:trim_lws/1,
        fun ersip_sdp_aux:parse_token/1,
        fun(B) -> ersip_parser_aux:parse_sep($/, B) end,
        fun ersip_parser_aux:parse_non_neg_int/1,
        fun parse_encoding_params/1
    ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, Result, _} ->
            [Type, _Lws, Name, _Sep, ClockRate, EncodingParams] = Result,
            {ok, #rtpmap{
                type = Type,
                encoding_name = Name,
                clock_rate = ClockRate,
                params = EncodingParams
            }};
        {error, Reason} ->
            {error, {invalid_rtpmap_candidate, Reason}}
    end.

-spec parse_encoding_params(binary()) -> parse_result(encoding_params()).
parse_encoding_params(<<>>) ->
    {ok, undefined, <<>>};
parse_encoding_params(Bin) ->
    Parsers = [
        fun(B) -> ersip_parser_aux:parse_sep($/, B) end,
        fun ersip_parser_aux:parse_non_neg_int/1
    ],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [_, EncodingParams], Rest} ->
            {ok, EncodingParams, Rest};
        Error ->
            Error
    end.

-spec assemble_rtpmap(rtpmap()) -> iolist().
assemble_rtpmap(#rtpmap{} = Rtpmap) ->
    ClockRateBin = integer_to_binary(clock_rate(Rtpmap)),
    [
        integer_to_binary(payload_type(Rtpmap)), " ",
        encoding_name(Rtpmap), "/",
        ClockRateBin,
        encoding_params_assemble(encoding_params(Rtpmap))
    ].

-spec encoding_params_assemble(encoding_params()) -> iolist().
encoding_params_assemble(undefined) ->
    [];
encoding_params_assemble(EncodingParams) ->
    ["/", integer_to_binary(EncodingParams)].
