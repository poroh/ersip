%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% @doc
%%% SIP Call-ID header.
%%%

-module(ersip_hdr_callid).
-include("ersip_sip_abnf.hrl").

-export([make/1,
         make_key/1,
         make_random/1,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).

-export_type([callid/0, raw/0, make/0]).

%%===================================================================
%% Types
%%===================================================================

-type callid() :: {callid, binary()}.
-type parse_result() :: {ok, callid()} | {error, parse_error()}.
-type parse_error() :: no_callid | {invalid_callid, binary()}.
-type raw() :: binary().
-type make() :: binary().

%%===================================================================
%% API
%%===================================================================

%% @doc Create Call-ID header from binary or from raw value.
%% Raise error if input is not well-formed Call-ID header or incorrect raw value.
%% Examples:
%% ```
%%   CallId = ersip_hdr_callid:make(<<"a@b">>).
%%   CallId2 = ersip_hdr_callid:make(<<"adwkldqwdjqklj">>).
%%   CallId3 = ersip_hdr_callid:make(ersip_hdr:add_value(<<"a@b">>, ersip_hdr:new(<<"CallId">>))).
%% '''
-spec make(ersip_hdr:header() | make()) -> callid().
make(Bin) when is_binary(Bin) ->
    case parse_callid(Bin) of
        {ok, CallId}    -> CallId;
        {error, Reason} -> error(Reason)
    end;
make(Header) ->
    case parse(Header) of
        {ok, CallId} -> CallId;
        {error, Reason} -> error(Reason)
    end.

%% @doc Get value that can be used in comparision Call-ID as erlang
%% terms (for example using ==).
-spec make_key(callid()) -> callid().
make_key({callid, _} = C) ->
    C.

%% @doc Create random Call-Id header.
%% @param NumBytes defines number bits of entropy that used in random.
-spec make_random(NumBytes :: pos_integer()) -> callid().
make_random(NumBytes) ->
    make(ersip_id:alphanum(crypto:strong_rand_bytes(NumBytes))).


%% @doc Parse header from binary or from ersip_hdr header.
-spec parse(ersip_hdr:header() | binary()) -> parse_result().
parse(Value)  when is_binary(Value)->
    parse_callid(Value);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_callid};
        [CallIdIOList]  ->
            parse_callid(iolist_to_binary(CallIdIOList))
    end.

%% @doc Create lowlevel ersip_hdr from Call-ID header.
-spec build(HeaderName :: binary(), callid()) -> ersip_hdr:header().
build(HdrName, {callid, _} = CallId) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(CallId), Hdr).

%% @doc Serialize header to iolist.
-spec assemble(callid()) ->  binary().
assemble({callid, CallIdBin}) ->
    CallIdBin.

%% @doc Serialize the header to binary.
-spec assemble_bin(callid()) ->  binary().
assemble_bin({callid, CallIdBin}) ->
    CallIdBin.

%% @doc Get raw value (in plain erlang types) of the header.
-spec raw(callid()) -> raw().
raw({callid, CallIdBin}) ->
    CallIdBin.

%%===================================================================
%% Internal implementation
%%===================================================================

%% @private
%% callid = word ["@" word]
-spec parse_callid(binary()) -> parse_result().
parse_callid(Binary) ->
   case start(Binary) of
       true -> {ok, {callid, Binary}};
       false -> {error, {invalid_callid, Binary}}
   end.

%% @private
-spec start(binary()) -> boolean().
start(<<U, Rest/binary>>) when ?is_word_char(U) -> first_part(Rest);
start(_) -> false.

%% @private
-spec first_part(binary()) -> boolean().
first_part(<< $@, Rest/binary>>) -> start_second_part(Rest);
first_part(<<U, Rest/binary>>)  when ?is_word_char(U) -> first_part(Rest);
first_part(<<>>) -> true;
first_part(_) -> false.

%% @private
-spec start_second_part(binary()) -> boolean().
start_second_part(<<U, Rest/binary>>) when ?is_word_char(U) -> second_part(Rest);
start_second_part(_) -> false.

%% @private
-spec second_part(binary()) -> boolean().
second_part(<<U, Rest/binary>>) when ?is_word_char(U) -> second_part(Rest);
second_part(<<>>) -> true;
second_part(_) -> false.

