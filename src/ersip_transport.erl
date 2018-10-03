%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP transport
%%

-module(ersip_transport).

-export([make/1,
         make_by_uri/1,
         tcp/0, tls/0, udp/0,
         is_datagram/1,
         is_tls/1,
         is_reliable/1,
         parse/1,
         parse_port_number/1,
         default_port/1,
         assemble_upper/1,
         assemble/1
        ]).

-export_type([transport/0,
              port_number/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type transport_atom() :: udp | tcp | tls | ws | wss.
-type transport() :: known_transport()
                   | other_transport().
-type known_transport() :: {transport, transport_atom()}.
-type known_transport(X) :: {transport, X}.
-type other_transport() :: {other_transport, binary()}.
-type port_number() :: 0..65535 | {default_port, transport()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(binary() | transport_atom()) -> transport().
make(V) ->
    case parse(V) of
        {ok, T} ->
            T;
        _ ->
            error(badarg)
    end.

-spec make_by_uri(ersip_uri:uri()) -> transport().
make_by_uri(URI) ->
    case ersip_uri:params(URI) of
        #{transport := Transport} ->
            Transport;
        _ ->
            {transport, udp}
    end.


-spec tcp() -> known_transport(tcp).
tcp() ->
    {transport, tcp}.

-spec tls() -> known_transport(tls).
tls() ->
    {transport, tls}.

-spec udp() -> known_transport(udp).
udp() ->
    {transport, udp}.

-spec parse(binary() | transport_atom()) -> Result when
      Result      :: {ok, transport()}
                   | {error, ErrorReason },
      ErrorReason :: {bad_transport_atom, atom()}
                   | {einval, transport}.
parse(V) when is_binary(V) ->
    case parse_bin(V) of
        {error, _} = Error ->
            Error;
        T ->
            {ok, T}
    end;
parse(V) when V =:= tcp; V =:= udp; V =:= tls; V =:= wss; V =:= ws ->
    {ok, {transport, V}};
parse(V) when is_atom(V) ->
    {error, {bad_transport_atom, V}}.

-spec is_datagram(known_transport()) -> boolean().
is_datagram({transport, udp}) ->
    true;
is_datagram({transport, ws}) ->
    true;
is_datagram({transport, wss}) ->
    true;
is_datagram({transport, tls}) ->
    false;
is_datagram({transport, tcp}) ->
    false;
is_datagram({other_transport, Binary}) when is_binary(Binary) ->
    error({error, {unknown_transport_type, Binary}}).

-spec is_tls(known_transport()) -> boolean().
is_tls({transport, udp}) ->
    false;
is_tls({transport, tcp}) ->
    false;
is_tls({transport, ws}) ->
    false;
is_tls({transport, tls}) ->
    true;
is_tls({transport, wss}) ->
    true;
is_tls({other_transport, Binary}) when is_binary(Binary) ->
    error({error, {unknown_transport_type, Binary}}).

-spec is_reliable(known_transport()) -> boolean().
is_reliable({transport, udp}) ->
    false;
is_reliable({transport, ws}) ->
    true;
is_reliable({transport, wss}) ->
    true;
is_reliable({transport, tls}) ->
    true;
is_reliable({transport, tcp}) ->
    true;
is_reliable({other_transport, Binary}) when is_binary(Binary) ->
    error({error, {unknown_transport_type, Binary}}).

-spec parse_port_number(binary()) -> ersip_parser_aux:parse_result(port_number()).
parse_port_number(Bin) ->
    case ersip_parser_aux:parse_non_neg_int(Bin) of
        {ok, Int, _} = R when Int > 0 andalso Int =< 65535 ->
            R;
        _ ->
            {error, {invalid_port, Bin}}
    end.

-spec default_port(transport()) -> 0..65535 | {default_port, transport()}.
default_port({transport, tcp}) ->
    5060; %% RFC 3261 19.1.2
default_port({transport, udp}) ->
    5060; %% RFC 3261 19.1.2
default_port({transport, tls}) ->
    5061; %% RFC 3261 19.1.2
default_port({transport, ws}) ->
    80;   %% RFC 7118 5.5
default_port({transport, wss}) ->
    443;  %% RFC 7118 5.5
default_port({other_transport, _} = T) ->
    {default_port, T}.

-spec assemble_upper(transport()) -> binary().
assemble_upper({transport, udp}) ->
    <<"UDP">>;
assemble_upper({transport, tcp}) ->
    <<"TCP">>;
assemble_upper({transport, tls}) ->
    <<"TLS">>;
assemble_upper({transport, ws}) ->
    <<"WS">>;
assemble_upper({transport, wss}) ->
    <<"WSS">>;
assemble_upper({other_transport, Binary}) ->
    ersip_bin:to_upper(Binary).

-spec assemble(transport()) -> binary().
assemble({transport, udp}) ->
    <<"udp">>;
assemble({transport, tcp}) ->
    <<"tcp">>;
assemble({transport, tls}) ->
    <<"tls">>;
assemble({transport, ws}) ->
    <<"ws">>;
assemble({transport, wss}) ->
    <<"wss">>;
assemble({other_transport, Binary}) ->
    Binary.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec parse_bin(binary()) -> transport() | {error, {einval, transport}}.
parse_bin(V) ->
    case ersip_bin:to_lower(V) of
        <<"tcp">> -> {transport, tcp};
        <<"udp">> -> {transport, udp};
        <<"tls">> -> {transport, tls};
        <<"wss">> -> {transport, wss};
        <<"ws">>  -> {transport, ws };
        Bin ->
            case ersip_parser_aux:check_token(Bin) of
                true  -> {other_transport, Bin};
                false -> {error, {einval, transport}}
            end
    end.
