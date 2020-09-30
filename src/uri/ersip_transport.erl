%%%
%%% Copyright (c) 2018 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP transport
%%%

-module(ersip_transport).

-export([make/1,
         make_by_uri/1,
         tcp/0, tls/0, udp/0, ws/0, wss/0, sctp/0,
         is_known_transport/1,
         is_message_oriented/1,
         is_tls/1,
         is_reliable/1,
         parse/1,
         default_port/1,
         assemble_upper/1,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).

-export_type([transport/0,
              raw/0]).

%%===================================================================
%% Types
%%===================================================================

-type transport_atom() :: udp | tcp | tls | ws | wss | sctp.
-type transport() :: known_transport()
                   | other_transport().
-type known_transport() :: {transport, transport_atom()}.
-type known_transport(X) :: {transport, X}.
-type other_transport() :: {other_transport, binary()}.
-type parse_result() :: {ok, transport()} | {error, parse_error()}.
-type parse_error()  :: {bad_transport_atom, atom()}
                      | {invalid_transport, binary()}.
-type raw() :: binary().

%%===================================================================
%% API
%%===================================================================

%% @doc Create transport from binary or from raw representation.
%% Examples:
%% ```
%% TCP = ersip_transport:tcp(),
%% TCP == ersip_transport:make(<<"tcp">>),
%% TCP == ersip_transport:make(tcp).
%% '''
-spec make(raw() | transport_atom()) -> transport().
make(V) ->
    case parse(V) of
        {ok, T} -> T;
        {error, Reason} -> error(Reason)
    end.

%% @doc Get transport from URI.
-spec make_by_uri(ersip_uri:uri()) -> transport().
make_by_uri(URI) ->
    case ersip_uri:transport(URI) of
        undefined -> {transport, udp};
        T -> T
    end.

%% @doc Construct TCP transport.
-spec tcp() -> known_transport(tcp).
tcp() ->
    {transport, tcp}.

%% @doc Construct TLS transport.
-spec tls() -> known_transport(tls).
tls() ->
    {transport, tls}.

%% @doc Construct UDP transport.
-spec udp() -> known_transport(udp).
udp() ->
    {transport, udp}.

%% @doc Construct WS transport.
-spec ws() -> known_transport(ws).
ws() ->
    {transport, ws}.

%% @doc Construct WSs transport.
-spec wss() -> known_transport(wss).
wss() ->
    {transport, wss}.

%% @doc Construct WSs transport.
-spec sctp() -> known_transport(sctp).
sctp() ->
    {transport, sctp}.

%% @doc Parse transport from raw representation or from binary().
-spec parse(raw() | transport_atom()) -> parse_result().
parse(V) when is_binary(V) ->
    case parse_bin(V) of
        {error, _} = Error ->
            Error;
        T ->
            {ok, T}
    end;
parse(V) when V == tcp; V == udp; V == tls; V == wss; V == ws; V == sctp ->
    {ok, {transport, V}};
parse(V) when is_atom(V) ->
    {error, {bad_transport_atom, V}}.

%% @doc Check if transport is known transport.
-spec is_known_transport(transport()) -> boolean().
is_known_transport({transport, _}) ->
    true;
is_known_transport({other_transport, _}) ->
    false.

%% @doc Check if transport is message (datagram) oriented.
%% Mostly it is require for connection-level parsers. Wether it needs
%% to parse stream or not.
-spec is_message_oriented(known_transport()) -> boolean().
is_message_oriented({transport, udp})  -> true;
is_message_oriented({transport, ws})   -> true;
is_message_oriented({transport, wss})  -> true;
is_message_oriented({transport, sctp}) -> true;
is_message_oriented({transport, tls})  -> false;
is_message_oriented({transport, tcp})  -> false;
is_message_oriented({other_transport, Binary}) when is_binary(Binary) ->
    error({unknown_transport, Binary}).

%% @doc Check if transport is TLS-based (secure).
-spec is_tls(known_transport()) -> boolean().
is_tls({transport, udp})  -> false;
is_tls({transport, tcp})  -> false;
is_tls({transport, sctp}) -> false;
is_tls({transport, ws})   -> false;
is_tls({transport, tls})  -> true;
is_tls({transport, wss})  -> true;
is_tls({other_transport, Binary}) when is_binary(Binary) ->
    error({unknown_transport, Binary}).

%% @doc Check if transport has reliable delivery.
-spec is_reliable(known_transport()) -> boolean().
is_reliable({transport, udp})  -> false;
is_reliable({transport, ws})   -> true;
is_reliable({transport, wss})  -> true;
is_reliable({transport, tls})  -> true;
is_reliable({transport, tcp})  -> true;
is_reliable({transport, sctp}) -> true;
is_reliable({other_transport, Binary}) when is_binary(Binary) ->
    error({unknown_transport, Binary}).

%% @doc Default port for the defined transport.
-spec default_port(transport()) -> 0..65535 | {default_port, transport()}.
default_port({transport, tcp})  -> 5060; %% RFC 3261 19.1.2
default_port({transport, udp})  -> 5060; %% RFC 3261 19.1.2
default_port({transport, sctp}) -> 5060; %% ?
default_port({transport, tls})  -> 5061; %% RFC 3261 19.1.2
default_port({transport, ws})   -> 80;   %% RFC 7118 5.5
default_port({transport, wss})  -> 443;  %% RFC 7118 5.5
default_port({other_transport, Bin}) ->
    error({unknown_transport, Bin}).

%% @doc Assemble transport as upper-case transport (for Via header).
-spec assemble_upper(transport()) -> binary().
assemble_upper({transport, udp})  -> <<"UDP">>;
assemble_upper({transport, tcp})  -> <<"TCP">>;
assemble_upper({transport, sctp}) -> <<"SCTP">>;
assemble_upper({transport, tls})  -> <<"TLS">>;
assemble_upper({transport, ws})   -> <<"WS">>;
assemble_upper({transport, wss})  -> <<"WSS">>;
assemble_upper({other_transport, Binary}) ->
    ersip_bin:to_upper(Binary).

%% @doc Assemble transport as iolist().
-spec assemble(transport()) -> binary().
assemble(T) ->
    assemble_bin(T).

%% @doc Assemble transport as binary().
-spec assemble_bin(transport()) -> binary().
assemble_bin({transport, udp})  -> <<"udp">>;
assemble_bin({transport, tcp})  -> <<"tcp">>;
assemble_bin({transport, sctp}) -> <<"sctp">>;
assemble_bin({transport, tls})  -> <<"tls">>;
assemble_bin({transport, ws})   -> <<"ws">>;
assemble_bin({transport, wss})  -> <<"wss">>;
assemble_bin({other_transport, Binary}) ->
    Binary.

%% @doc Get raw value (in plain erlang types) of the transport.
-spec raw(transport()) -> raw().
raw(Transport) ->
    assemble_bin(Transport).

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_bin(binary()) -> transport() | {error, parse_error()}.
parse_bin(V) ->
    case ersip_bin:to_lower(V) of
        <<"sctp">> -> {transport, sctp};
        <<"tcp">>  -> {transport, tcp};
        <<"udp">>  -> {transport, udp};
        <<"tls">>  -> {transport, tls};
        <<"wss">>  -> {transport, wss};
        <<"ws">>   -> {transport, ws };
        Bin ->
            case ersip_parser_aux:check_token(Bin) of
                true  -> {other_transport, Bin};
                false -> {error, {invalid_transport, Bin}}
            end
    end.
