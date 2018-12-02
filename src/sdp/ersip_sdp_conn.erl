%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP connection address
%%

-module(ersip_sdp_conn).

-export([addr/1,
         ttl/1,
         num_addrs/1,
         parse/1]).

-export_type([conn/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(conn, {addr      :: ersip_sdp_addr:addr(),
               ttl       :: non_neg_integer() | undefined,
               num_addrs :: non_neg_integer()}).
-type conn() :: #conn{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec addr(conn()) -> ersip_sdp_addr:addr().
addr(#conn{addr = Addr}) ->
    Addr.

-spec ttl(conn()) -> non_neg_integer() | undefined.
ttl(#conn{ttl = TTL}) ->
    TTL.

-spec num_addrs(conn()) -> non_neg_integer().
num_addrs(#conn{num_addrs = NumAddrs}) ->
    NumAddrs.

-spec parse(binary()) -> ersip_parser_aux:parse_result(conn() | undefined).
parse(<<"c=", Rest/binary>>) ->
    Parsers = [fun ersip_sdp_addr:parse/1,
               fun parse_ttl/1,
               fun parse_num_addrs/1,
               fun ersip_sdp_aux:parse_crlf/1],
    case ersip_parser_aux:parse_all(Rest, Parsers) of
        {ok, [Addr, TTL, NumAddrs, _], Rest1} ->
            Conn = #conn{addr = Addr, ttl = TTL, num_addrs = NumAddrs},
            {ok, Conn, Rest1};
        {error, Reason} ->
            {error, {invalid_conn, Reason}}
    end;
parse(Other) ->
    {ok, undefined, Other}.


%%%===================================================================
%%% Internal Implementation
%%%===================================================================

%% "/" ttl
-spec parse_ttl(binary()) -> ersip_parser_aux:parse_result(non_neg_integer()).
parse_ttl(<<"/", Rest/binary>>) ->
    case ersip_parser_aux:parse_non_neg_int(Rest) of
        {ok, Int, _} = Ok ->
            %% TTL values MUST be in the range 0-255.
            case Int of
                _ when Int > 255 ->
                    {errors, {invalid_ttl, Int}};
                _ -> Ok
            end;
        {error, Reason} ->
            {error, {invalid_ttl, Reason}}
    end;
parse_ttl(Other) ->
    {ok, undefined, Other}.

%% "/" integer
-spec parse_num_addrs(binary()) -> ersip_parser_aux:parse_result(non_neg_integer()).
parse_num_addrs(<<"/", Rest/binary>>) ->
    case ersip_parser_aux:parse_non_neg_int(Rest) of
        {ok, Int, _} = Ok ->
            %% TTL values MUST be in the range 0-255.
            case Int of
                _ when Int > 255 ->
                    {errors, {invalid_ttl, Int}};
                _ -> Ok
            end;
        {error, Reason} ->
            {error, {invalid_ttl, Reason}}
    end;
parse_num_addrs(Other) ->
    {ok, 1, Other}.
