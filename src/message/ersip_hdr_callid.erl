%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Call-ID header
%%%

-module(ersip_hdr_callid).
-include("ersip_sip_abnf.hrl").

-export([make/1,
         make_key/1,
         make_random/1,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1
        ]).

-export_type([callid/0]).

%%===================================================================
%% Types
%%===================================================================

-type callid() :: {callid, binary()}.
-type parse_result() :: {ok, callid()} | {error, parse_error()}.
-type parse_error() :: no_callid | {invalid_callid, binary()}.

%%===================================================================
%% API
%%===================================================================

-spec make(ersip_hdr:header() | binary()) -> callid().
make(Bin) when is_binary(Bin) ->
    case parse_callid(Bin) of
        {ok, CallId} ->
            CallId;
        Error ->
            error(Error)
    end;
make(Header) ->
    case parse(Header) of
        {ok, CallId} ->
            CallId;
        Error ->
            error(Error)
    end.

-spec make_key(callid()) -> callid().
make_key({callid, _} = C) ->
    C.

-spec make_random(NumBytes :: pos_integer()) -> callid().
make_random(NumBytes) ->
    make(ersip_id:alphanum(crypto:strong_rand_bytes(NumBytes))).

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

-spec build(HeaderName :: binary(), callid()) -> ersip_hdr:header().
build(HdrName, {callid, _} = CallId) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(CallId), Hdr).

-spec assemble(callid()) ->  binary().
assemble({callid, CallIdBin}) ->
    CallIdBin.

-spec assemble_bin(callid()) ->  binary().
assemble_bin({callid, CallIdBin}) ->
    CallIdBin.

%%===================================================================
%% Internal implementation
%%===================================================================

%% callid   =  word ["@" word]
-spec parse_callid(binary()) -> parse_error().
parse_callid(Binary) ->
   case start(Binary) of
       true -> {ok, {callid, Binary}};
       false -> {error, {invalid_callid, Binary}}
   end.

start(<<U, Rest/binary>>) when ?is_word_char(U) -> first_part(Rest);
start(_) -> false.

first_part(<< $@, Rest/binary>>) -> start_second_part(Rest);
first_part(<<U, Rest/binary>>)  when ?is_word_char(U) -> first_part(Rest);
first_part(<<>>) -> true;
first_part(_) -> false.

start_second_part(<<U, Rest/binary>>) when ?is_word_char(U) -> second_part(Rest);
start_second_part(_) -> false.

second_part(<<U, Rest/binary>>) when ?is_word_char(U) -> second_part(Rest);
second_part(<<>>) -> true;
second_part(_) -> false.

