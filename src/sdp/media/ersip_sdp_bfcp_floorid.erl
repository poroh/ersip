%%
%% Copyright (c) 2021 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media BFCP 'floorid' attribute
%%

-module(ersip_sdp_bfcp_floorid).

-export([new/2,
         id/1,
         streams/1,
         set_id/2,
         set_streams/2,
         parse/1,
         assemble/1,
         assemble_bin/1
        ]).

-export_type([bfcp_floorid/0]).
-export_type([streams/0]).

-define(is_uint16(I), (is_integer(I) andalso I >= 0 andalso I =< 65535)).

%%%===================================================================
%%% Types
%%%===================================================================
-record(bfcp_floorid, {id :: non_neg_integer(),
                       streams :: streams()
                      }).

-type bfcp_floorid() :: #bfcp_floorid{}.
-type streams() :: [binary(), ...].

-type parse_result()  :: ersip_parser_aux:parse_result(bfcp_floorid()).
-type parse_result(T) :: ersip_parser_aux:parse_result(T).

%%%===================================================================
%%% API
%%%===================================================================
-spec new(non_neg_integer(), streams()) -> bfcp_floorid().
new(I, [_|_] = Streams) when ?is_uint16(I) ->
    #bfcp_floorid{id = I, streams = Streams}.

-spec id(bfcp_floorid()) -> non_neg_integer().
id(#bfcp_floorid{id = I}) ->
    I.

-spec streams(bfcp_floorid()) -> streams().
streams(#bfcp_floorid{streams = Ss}) ->
    Ss.

-spec set_id(non_neg_integer(), bfcp_floorid()) -> bfcp_floorid().
set_id(I, #bfcp_floorid{} = BF) when ?is_uint16(I) ->
    BF#bfcp_floorid{id = I}.

-spec set_streams(streams(), bfcp_floorid()) -> bfcp_floorid().
set_streams([_|_] = Ss, #bfcp_floorid{} = BF) ->
    BF#bfcp_floorid{streams = Ss}.

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    do_parse_bfcp_floorid(Bin).

-spec assemble(bfcp_floorid()) -> iolist().
assemble(BF) ->
    assemble_bfcp_floorid(BF).

-spec assemble_bin(bfcp_floorid()) -> binary().
assemble_bin(BF) ->
    iolist_to_binary(assemble(BF)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================
-define(sp, " ").

%% https://tools.ietf.org/rfc/rfc8856.html#name-sdp-floorid-attribute
%%
%% floor-id = 1*DIGIT SP "mstrm:" token *(SP token)
%% DIGIT = <DIGIT as defined in [RFC5234]>
%% token = <token as defined in [RFC8866]>
%%
-spec do_parse_bfcp_floorid(binary()) -> parse_result().
do_parse_bfcp_floorid(Rest) ->
    Parsers = [fun ersip_parser_aux:parse_non_neg_int/1,
               fun parse_mstrm/1
              ],
    case ersip_parser_aux:parse_all(Rest, Parsers) of
        {ok, [ID, Streams], Rest1} when ID < 65536 ->
            BF = #bfcp_floorid{id = ID, streams = Streams},
            {ok, BF, Rest1};
        {ok, [ID|_], _} ->
            {error, {invalid_bfcp_floorid, {invalid_id, ID}}};
        {error, Reason} ->
            {error, {invalid_bfcp_floorid, Reason}}
    end.

-spec assemble_bfcp_floorid(bfcp_floorid()) -> iolist().
assemble_bfcp_floorid(#bfcp_floorid{
                         id = I,
                         streams = [_|_] = Ss}) ->
    [integer_to_binary(I), ?sp,
     "mstrm:", lists:join(?sp, Ss)].

-spec parse_mstrm(binary()) -> parse_result(streams()).
parse_mstrm(<<?sp, "mstrm:", Rest/binary>>) ->
    parse_streams(Rest);
%% For backward compatibility with RFC4583
parse_mstrm(<<?sp, "m-stream:", Rest/binary>>) ->
    parse_streams(Rest);
parse_mstrm(Bin) ->
    {error, {invalid_mstrm, Bin}}.

%% token *(SP token)
-spec parse_streams(binary()) -> parse_result(streams()).
parse_streams(Bin) ->
    case ersip_parser_aux:token_list(Bin, lws) of
        {ok, Streams, <<>>} ->
            {ok, Streams, <<>>};
        _ ->
            {error, {invalid_mstrm, Bin}}
    end.
