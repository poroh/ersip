%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP address
%%

-module(ersip_sdp_addr).

-export([raw/1,
         make/1,
         parse/1,
         parse/3,
         assemble/1
        ]).

-export_type([addr/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type addr() :: {ip4, inet:ip4_address()}
              | {ip4_host, fqdn()}
              | {ip6, inet:ip6_address()}
              | {ip6_host, fqdn()}
              | {unknown_addr, binary(), binary(), binary()}.

-type fqdn() :: binary().
-type parse_result() :: {ok, addr()}
                      | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec raw(addr()) -> {binary(), binary(), binary()}.
raw({ip4, IP4}) ->
    {<<"IN">>, <<"IP4">>, list_to_binary(inet:ntoa(IP4))};
raw({ip4_host, FQDN}) ->
    {<<"IN">>, <<"IP4">>, FQDN};
raw({ip6, IP6}) ->
    {<<"IN">>, <<"IP6">>, list_to_binary(inet:ntoa(IP6))};
raw({ip6_host, FQDN}) ->
    {<<"IN">>, <<"IP6">>, FQDN};
raw({unknown_addr, NetType, AddrType, Address}) ->
    {NetType, AddrType, Address}.

-spec make(binary()) -> addr().
make(Bin) when is_binary(Bin) ->
    case parse(Bin) of
        {ok, Addr, <<>>} ->
            Addr;
        {ok, _, _} ->
            error({invalid_addr, Bin});
        {error, Reason} ->
            error(Reason)
    end.

-spec parse(binary()) -> ersip_parser_aux:parse_result(addr()).
parse(Bin) when is_binary(Bin) ->
    Parsers = [fun ersip_sdp_aux:parse_token/1,
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_sdp_aux:parse_token/1,
               fun ersip_parser_aux:parse_lws/1,
               fun parse_address/1],
    case ersip_parser_aux:parse_all(Bin, Parsers) of
        {ok, [NetType, _, AddrType, _, Address], Rest} ->
            case parse(NetType, AddrType, Address) of
                {ok, Addr} ->
                    {ok, Addr, Rest};
                {error, _} = Error ->
                    Error
            end;
         {error, Reason} ->
            {error, {invalid_addr, Reason}}
    end.

-spec parse(binary(), binary(), binary()) -> parse_result().
parse(NetType, AddrType, Address) ->
    MaybeNetType =
        case ersip_sdp_aux:check_token(NetType) of
            false ->
                {error, {invalid_net_type, NetType}};
            true ->
                {ok, ersip_bin:to_lower(NetType)}
        end,
    MaybeAddrType =
        case ersip_sdp_aux:check_token(AddrType) of
            false ->
                {error, {invalid_addr_type, AddrType}};
            true ->
                {ok, ersip_bin:to_lower(AddrType)}
        end,
    case {MaybeNetType, MaybeAddrType} of
        {{error,_} = E, _} -> E;
        {_, {error, _} = E} -> E;
        {{ok, NetT}, {ok, AddrT}} ->
            R = do_parse(NetT, AddrT, Address),
            case R of
                {error, unknown_addr_type} ->
                    UnknownAddr = {unknown_addr, NetType, AddrType, Address},
                    {ok, UnknownAddr};
                _ -> R
            end
    end.

-spec assemble(addr()) -> iolist().
assemble({ip4, IP4}) ->
    [<<"IN IP4 ">>, inet:ntoa(IP4)];
assemble({ip4_host, FQDN}) ->
    [<<"IN IP4 ">>, FQDN];
assemble({ip6, IP6}) ->
    [<<"IN IP6 ">>, inet:ntoa(IP6)];
assemble({ip6_host, FQDN}) ->
    [<<"IN IP6 ">>, FQDN];
assemble({unknown_addr, NetType, AddrType, Address}) ->
    [NetType, " ", AddrType, " ", Address].

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec do_parse(binary(), binary(), binary()) -> parse_result().
do_parse(<<"in">>, <<"ip4">>, AddrBin) ->
    %% For an address type of IP4, this is either the fully qualified
    %% domain name of the machine or the dotted-decimal
    %% representation of the IP version 4 address of the machine.
    case inet:parse_ipv4strict_address(binary_to_list(AddrBin)) of
        {ok, IPv4} ->
            {ok, {ip4, IPv4}};
        {error, _} ->
            case ersip_host:check_hostname(AddrBin) of
                true ->
                    {ok, {ip4_host, AddrBin}};
                false ->
                    {error, {invalid_ip4_address, AddrBin}}
            end
    end;
do_parse(<<"in">>, <<"ip6">>, AddrBin) ->
    %% For an address type of IP6, this is either the fully qualified
    %% domain name of the machine or the compressed textual
    %% representation of the IP version 6 address of the machine.
    case inet:parse_ipv6strict_address(binary_to_list(AddrBin)) of
        {ok, IPv6} ->
            {ok, {ip6, IPv6}};
        {error, _} ->
            case ersip_host:check_hostname(AddrBin) of
                true ->
                    {ok, {ip6_host, AddrBin}};
                false ->
                    {error, {invalid_ip6_address, AddrBin}}
            end
    end;
do_parse(_, _, _) ->
    {error, unknown_addr_type}.


-spec parse_address(binary()) -> ersip_parser_aux:parse_result(binary()).
parse_address(Bin) ->
    AddrEnd = find_addr_end(Bin, 0),
    <<Addr:AddrEnd/binary, Rest/binary>> = Bin,
    {ok, Addr, Rest}.

-spec find_addr_end(binary(), non_neg_integer()) -> non_neg_integer().
find_addr_end(<<>>, Acc) ->
    Acc;
find_addr_end(<<"/", _/binary>>, Acc) ->
    Acc;
find_addr_end(<<" ", _/binary>>, Acc) ->
    Acc;
find_addr_end(<<"\r", _/binary>>, Acc) ->
    Acc;
find_addr_end(<<_:8, Rest/binary>>, Acc) ->
    find_addr_end(Rest, Acc+1).




