%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Response common routines
%%

-module(ersip_response).

-export([target/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-type target() :: {ersip_host:host(),
                   inet:port_number(),
                   ersip_transport:transport(),
                   Options :: map()
                  }.

%%%===================================================================
%%% API
%%%===================================================================


%% 18.2.2 Sending Responses
-spec target(ersip_hdr_via:via()) -> Result when
      Result :: {direct, target()}
              | {reuse, target()}.
target(Via) ->
    ViaParams = ersip_hdr_via:params(Via),
    {sent_protocol, _, _, Transport} = ersip_hdr_via:sent_protocol(Via),
    case ersip_transport:is_reliable(Transport) of
        true ->
            %% If the "sent-protocol" is a reliable transport protocol
            %% such as TCP or SCTP, or TLS over those, the response
            %% MUST be sent using the existing connection to the
            %% source of the original request that created the
            %% transaction, if that connection is still open.  This
            %% requires the server transport to maintain an
            %% association between server transactions and transport
            %% connections.  If that connection is no longer open, the
            %% server SHOULD open a connection to the IP address in
            %% the "received" parameter, if present, using the port in
            %% the "sent-by" value, or the default port for that
            %% transport, if no port is specified.  If that connection
            %% attempt fails, the server SHOULD use the procedures in
            %% [4] for servers in order to determine the IP address
            %% and port to open the connection and send the response
            %% to..
            {reuse, make_target_from_received(Via)};
        false ->
            case ViaParams of
                #{maddr := Host} ->
                    %% Otherwise, if the Via header field value
                    %% contains a "maddr" parameter, the response MUST
                    %% be forwarded to the address listed there, using
                    %% the port indicated in "sent-by", or port 5060
                    %% if none is present.  If the address is a
                    %% multicast address, the response SHOULD be sent
                    %% using the TTL indicated in the "ttl" parameter,
                    %% or with a TTL of 1 if that parameter is not
                    %% present.
                    Port = select_port(Via),
                    Options =
                        case ViaParams of
                            #{ttl := TTL} ->
                                #{ttl => TTL};
                            _ ->
                                #{ttl => 1}
                        end,
                    {direct, {Host, Port, Transport, Options}};
                 _ ->
                    target_rfc3261_or_3581(ViaParams, Via)
            end
    end.


%%%===================================================================
%%% Helpers
%%%===================================================================

-spec make_target_from_received(ersip_hdr_via:via()) -> target().
make_target_from_received(Via) ->
    {sent_protocol, _, _, Transport} = ersip_hdr_via:sent_protocol(Via),
    ViaParams = ersip_hdr_via:params(Via),
    Host =
        case ViaParams of
            #{received := H} ->
                H;
            _ ->
                {sent_by, H, _} = ersip_hdr_via:sent_by(Via),
                H
        end,
    {Host, select_port(Via), Transport, #{}}.


-spec select_port(ersip_hdr_via:via()) -> inet:port_number().
select_port(Via) ->
    {sent_by, _, SentByPort} = ersip_hdr_via:sent_by(Via),
    SentByPort.

target_rfc3261_or_3581(ViaParams, Via) ->
    case ViaParams of
        #{received := Host, rport := Port} when Port /= true ->
            %% RFC 3581:
            %% When a server attempts to send a response, it examines the topmost
            %% Via header field value of that response.  If the "sent-protocol"
            %% component indicates an unreliable unicast transport protocol, such as
            %% UDP, and there is no "maddr" parameter, but there is both a
            %% "received" parameter and an "rport" parameter, the response MUST be
            %% sent to the IP address listed in the "received" parameter, and the
            %% port in the "rport" parameter.  The response MUST be sent from the
            %% same address and port that the corresponding request was received on.
            %% This effectively adds a new processing step between bullets two and
            %% three in Section 18.2.2 of SIP [1].
            {sent_protocol, _, _, Transport} = ersip_hdr_via:sent_protocol(Via),
            {direct, {Host, Port, Transport, #{}}};
        _ ->
            %% Otherwise (for unreliable unicast transports), if the
            %% top Via has a "received" parameter, the response MUST
            %% be sent to the address in the "received" parameter,
            %% using the port indicated in the "sent-by" value, or
            %% using port 5060 if none is specified explicitly.  If
            %% this fails, for example, elicits an ICMP "port
            %% unreachable" response, the procedures of Section 5 of
            %% [4] SHOULD be used to determine where to send the
            %% response.
            {direct, make_target_from_received(Via)}
    end.
