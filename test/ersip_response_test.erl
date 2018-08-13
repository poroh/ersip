%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP response test
%%

-module(ersip_response_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

target_test() ->
    %% o  If the "sent-protocol" is a reliable transport protocol such as
    %%    TCP or SCTP, or TLS over those, the response MUST be sent using
    %%    the existing connection to the source of the original request
    %%    that created the transaction, if that connection is still open.
    %%    This requires the server transport to maintain an association
    %%    between server transactions and transport connections.  If that
    %%    connection is no longer open, the server SHOULD open a
    %%    connection to the IP address in the "received" parameter, if
    %%    present, using the port in the "sent-by" value, or the default
    %%    port for that transport, if no port is specified.  If that
    %%    connection attempt fails, the server SHOULD use the procedures
    %%    in [4] for servers in order to determine the IP address and
    %%    port to open the connection and send the response to.
    ?assertEqual({reuse, make_target(<<"1.1.1.1">>, 5070, tcp)},
                 target(<<"SIP/2.0/TCP 192.168.1.1:5070;received=1.1.1.1;branch=x12345">>)),
    ?assertEqual({reuse, make_target(<<"192.168.1.1">>, 5070, tcp)},
                 target(<<"SIP/2.0/TCP 192.168.1.1:5070;branch=x12345">>)),
    ?assertEqual({reuse, make_target(<<"192.168.1.1">>, 5060, tcp)},
                 target(<<"SIP/2.0/TCP 192.168.1.1;branch=x12345">>)),

    %% o  Otherwise, if the Via header field value contains a "maddr"
    %%    parameter, the response MUST be forwarded to the address listed
    %%    there, using the port indicated in "sent-by", or port 5060 if
    %%    none is present.  If the address is a multicast address, the
    %%    response SHOULD be sent using the TTL indicated in the "ttl"
    %%    parameter, or with a TTL of 1 if that parameter is not present.
    ?assertEqual({direct, make_target(<<"244.0.0.1">>, 5070, udp, 1)},
                 target(<<"SIP/2.0/UDP 192.168.1.1:5070;maddr=244.0.0.1;branch=x12345">>)),
    ?assertEqual({direct, make_target(<<"244.0.0.1">>, 5070, udp, 55)},
                 target(<<"SIP/2.0/UDP 192.168.1.1:5070;maddr=244.0.0.1;ttl=55;branch=x12345">>)),

    %% o  Otherwise (for unreliable unicast transports), if the top Via
    %%    has a "received" parameter, the response MUST be sent to the
    %%    address in the "received" parameter, using the port indicated
    %%    in the "sent-by" value, or using port 5060 if none is specified
    %%    explicitly.  If this fails, for example, elicits an ICMP "port
    %%    unreachable" response, the procedures of Section 5 of [4]
    %%    SHOULD be used to determine where to send the response.
    ?assertEqual({direct, make_target(<<"2.2.2.2">>, 5059, udp)},
                 target(<<"SIP/2.0/UDP 192.168.1.1:5059;received=2.2.2.2;branch=x12345">>)),

    %% o  Otherwise, if it is not receiver-tagged, the response MUST be
    %%    sent to the address indicated by the "sent-by" value, using the
    %%    procedures in Section 5 of [4].
    ?assertEqual({direct, make_target(<<"192.168.1.1">>, 5059, udp)},
                 target(<<"SIP/2.0/UDP 192.168.1.1:5059;branch=x12345">>)),
    ok.

rport_test() ->
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
    ?assertEqual({direct, make_target(<<"2.2.2.2">>, 5051, udp)},
                 target(<<"SIP/2.0/UDP 192.168.1.1:5059;received=2.2.2.2;rport=5051;branch=x12345">>)),

    %% rport is ignored for maddr:
    ?assertEqual({direct, make_target(<<"244.0.0.1">>, 5070, udp, 1)},
                 target(<<"SIP/2.0/UDP 192.168.1.1:5070;maddr=244.0.0.1;rport=5071;branch=x12345">>)),

    %% rport is ignored if not filled:
    ?assertEqual({direct, make_target(<<"192.168.1.1">>, 5070, udp)},
                 target(<<"SIP/2.0/UDP 192.168.1.1:5070;rport;branch=x12345">>)),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

make_target(HostBin, Port, Transport) ->
    {ersip_host:make(HostBin), Port, ersip_transport:make(Transport), #{}}.

make_target(HostBin, Port, Transport, TTL) ->
    {ersip_host:make(HostBin), Port, ersip_transport:make(Transport), #{ttl => TTL}}.

target(ViaBin) ->
    {ok, Via} = ersip_hdr_via:parse(ViaBin),
    ersip_response:target(Via).

