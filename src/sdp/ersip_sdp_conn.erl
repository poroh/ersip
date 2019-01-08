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
    case ersip_sdp_addr:parse(Rest) of
        {ok, {ip4, _} = Addr, Rest1} ->
            do_parse_ip4_conn(Addr, Rest1);
        {ok, {ip6, _} = Addr, Rest1} ->
            do_parse_ip6_conn(Addr, Rest1);
        {ok, {ip6_host, _} = Addr, Rest1} ->
            do_parse_unicast_conn(Addr, Rest1);
        {ok, {ip4_host, _} = Addr, Rest1} ->
            do_parse_unicast_conn(Addr, Rest1);
        {ok, {unknown_addr, _, _, _} = Addr, Rest1} ->
            do_parse_unicast_conn(Addr, Rest1);
        {error, Reason} ->
            {error, {invalid_conn, Reason}}
    end;
parse(Other) ->
    {ok, undefined, Other}.

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-spec do_parse_ip4_conn(ersip_sdp_addr:addr(), binary()) -> ersip_parser_aux:parse_result(conn() | undefined).
do_parse_ip4_conn(Addr, Bin) ->
    case is_ip4_multicast(Addr) of
        true ->
            do_parse_ip4_multicast_conn(Addr, Bin);
        false ->
            do_parse_unicast_conn(Addr, Bin)
    end.

-spec do_parse_ip4_multicast_conn(ersip_sdp_addr:addr(), binary()) -> ersip_parser_aux:parse_result(conn() | undefined).
do_parse_ip4_multicast_conn(Addr, Bin) ->
    Parsers = [fun parse_ttl/1,
               fun parse_num_addrs/1,
               fun ersip_sdp_aux:parse_crlf/1],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [TTL, NumAddrs, _], Rest1} ->
            Conn = #conn{addr = Addr, ttl = TTL, num_addrs = NumAddrs},
            {ok, Conn, Rest1};
        {error, Reason} ->
            {error, {invalid_conn, Reason}}
    end.

-spec do_parse_ip6_conn(ersip_sdp_addr:addr(), binary()) -> ersip_parser_aux:parse_result(conn() | undefined).
do_parse_ip6_conn(Addr, Bin) ->
    case is_ip6_multicast(Addr) of
        true ->
            do_parse_ip6_multicast_conn(Addr, Bin);
        false ->
            do_parse_unicast_conn(Addr, Bin)
    end.

-spec do_parse_ip6_multicast_conn(ersip_sdp_addr:addr(), binary()) -> ersip_parser_aux:parse_result(conn() | undefined).
do_parse_ip6_multicast_conn(Addr, Bin) ->
    Parsers = [fun parse_num_addrs/1,
               fun ersip_sdp_aux:parse_crlf/1],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [NumAddrs, _], Rest1} ->
            Conn = #conn{addr = Addr, ttl = undefined, num_addrs = NumAddrs},
            {ok, Conn, Rest1};
        {error, Reason} ->
            {error, {invalid_conn, Reason}}
    end.

-spec do_parse_unicast_conn(ersip_sdp_addr:addr(), binary()) -> ersip_parser_aux:parse_result(conn() | undefined).
do_parse_unicast_conn(Addr, Bin) ->
    case ersip_sdp_aux:parse_crlf(Bin) of
        {ok, _, Rest1} ->
            Conn = #conn{addr = Addr, ttl = undefined, num_addrs = 1},
            {ok, Conn, Rest1};
        {error, Reason} ->
            {error, {invalid_conn, Reason}}
    end.

%% "/" ttl
%% ttl = (POS-DIGIT *2DIGIT) / "0"
-spec parse_ttl(binary()) -> ersip_parser_aux:parse_result(non_neg_integer()).
parse_ttl(<<"/", Rest/binary>>) ->
    case ersip_parser_aux:parse_non_neg_int(Rest) of
        {ok, Int, _} = Ok ->
            %% TTL values MUST be in the range 0-255.
            case Int of
                _ when Int > 255 ->
                    {error, {invalid_ttl, Int}};
                _ -> Ok
            end;
        {error, Reason} ->
            {error, {invalid_ttl, Reason}}
    end;
parse_ttl(_Other) ->
    %% Sessions using an IPv4 multicast connection address MUST also have
    %% a time to live (TTL) value present in addition to the multicast
    %% address.
    {error, ip4_multicast_ttl_expected}.

%% "/" integer
-spec parse_num_addrs(binary()) -> ersip_parser_aux:parse_result(non_neg_integer()).
parse_num_addrs(<<"/", Rest/binary>>) ->
    case ersip_parser_aux:parse_non_neg_int(Rest) of
        {ok, _, _} = Ok ->
            Ok;
        {error, Reason} ->
            {error, {invalid_num_addrs, Reason}}
    end;
parse_num_addrs(Other) ->
    {ok, 1, Other}.


%% See https://tools.ietf.org/html/rfc5771#page-4
-spec is_ip4_multicast(ersip_sdp_addr:addr()) -> boolean().
is_ip4_multicast({ip4, {A, _, _, _}}) when A >= 224, A =< 239 ->
    true;
is_ip4_multicast(_) ->
    false.

%% https://tools.ietf.org/html/rfc4291#section-2.7
-spec is_ip6_multicast(ersip_sdp_addr:addr()) -> boolean().
is_ip6_multicast({ip6, {A, _, _, _,  _, _, _, _}}) when A >= 16#FF00, A =< 16#FFFF ->
    true;
is_ip6_multicast(_) ->
    false.


