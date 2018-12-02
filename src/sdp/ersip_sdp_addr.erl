%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP address
%%

-module(ersip_sdp_addr).

-export([parse/3]).

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
-type parse_result() :: ersip_parser_aux:parse_result(addr()).

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(binary(), binary(), binary()) -> parse_result().
parse(NetType, AddrType, Address) ->
    R = do_parse(ersip_bin:to_lower(NetType),
                 ersip_bin:to_lower(AddrType),
                 Address),
    case R of
        {error, unknown_addr_type} ->
            UnknownAddr = {unknown_addr, NetType, AddrType, Address},
            {ok, UnknownAddr};
        _ -> R
    end.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec do_parse(binary(), binary(), binary()) -> parse_result().
do_parse(<<"in">>, <<"ip4">>, AddrBin) ->
    %% For an address type of IP4, this is either the fully qualified
    %% domain name of the machine or the dotted-decimal
    %% representation of the IP version 4 address of the machine.
    case inet:parse_ipv4_address(binary_to_list(AddrBin)) of
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
    case inet:parse_ipv6_address(binary_to_list(AddrBin)) of
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
