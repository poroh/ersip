%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Connection test
%%

-module(ersip_conn_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================
-define(crlf, "\r\n").

conn_add_received_test() ->
    %% When the server transport receives a request over any
    %% transport, it MUST examine the value of the "sent-by" parameter
    %% in the top Via header field value.  If the host portion of the
    %% "sent-by" parameter contains a domain name, or if it contains
    %% an IP address that differs from the packet source address, the
    %% server MUST add a "received" parameter to that Via header field
    %% value.  This parameter MUST contain the source address from
    %% which the packet was received.
    RemoteIP = {127, 0, 0, 1},
    Conn = create_conn(RemoteIP, 5090),
    %% 1. received added if domain name:
    MsgWithDomainName =
        <<"INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
        >>,
    check_received(RemoteIP, MsgWithDomainName, Conn),

    MsgWithAnotherIPv4 =
        <<"INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Via: SIP/2.0/UDP 127.0.0.2;branch=z9hG4bK776asdhds"
          ?crlf "Via: SIP/2.0/UDP 127.0.1.2;branch=z9hG4bK776ddedes"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
        >>,
    check_received(RemoteIP, MsgWithAnotherIPv4, Conn),

    MsgWithAnotherIPv6 =
        <<"INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Via: SIP/2.0/UDP [::1];branch=z9hG4bK776asdhds"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
        >>,
    check_received(RemoteIP, MsgWithAnotherIPv6, Conn).

conn_do_not_add_received_test() ->
    RemoteIP = {127, 0, 0, 1},
    Conn = create_conn(RemoteIP, 5090),
    %% 1. received added if domain name:
    Msg =
        <<"INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Via: SIP/2.0/UDP 127.0.0.1;branch=z9hG4bK776asdhds"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
        >>,
    check_no_received(Msg, Conn).

conn_rport_test() ->
    RemoteIP = {127, 0, 0, 1},
    RemotePort = 5091,
    Conn = create_conn(RemoteIP, RemotePort),
    Msg =
        <<"INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Via: SIP/2.0/UDP 127.0.0.1;rport;branch=z9hG4bK776asdhds"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
        >>,
    %% Adding received in any case if rport is specified
    check_received(RemoteIP, Msg, Conn),
    check_rport(RemotePort, Msg, Conn).

conn_error_invalid_message_test() ->
    RemoteIP = {127, 0, 0, 1},
    Conn = create_conn(RemoteIP, 5090),
    %% 1. received added if domain name:
    MsgWithoutBadFirstline =
        <<"INVITE_sip:bob@biloxi.com SIP/2.0"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
        >>,
    {_, [{bad_message, MsgWithoutBadFirstline, {error, _}}]}
        = ersip_conn:conn_data(MsgWithoutBadFirstline, Conn).

conn_error_no_via_test() ->
    RemoteIP = {127, 0, 0, 1},
    Conn = create_conn(RemoteIP, 5090),
    %% 1. received added if domain name:
    MsgWithoutVia =
        <<"INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
        >>,
    {_, [{bad_message, BadMsg, {error, _}}]} = ersip_conn:conn_data(MsgWithoutVia, Conn),
    CallIdH = ersip_msg:get(<<"call-id">>, BadMsg),
    ?assertEqual(ersip_hdr_callid:make(<<"deadbeef">>), ersip_hdr_callid:make(CallIdH)).


conn_stream_test() ->
    RemoteIP = {127, 0, 0, 1},
    Conn = create_stream_conn(RemoteIP, 5090),
    %% 1. received added if domain name:
    Msgs =
        <<"INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
          "INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef1",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
        >>,
    {_, [{new_request, NewMsg1}, {new_request, NewMsg2}]} = ersip_conn:conn_data(Msgs, Conn),
    CallId1H = ersip_msg:get(<<"call-id">>, NewMsg1),
    CallId2H = ersip_msg:get(<<"call-id">>, NewMsg2),
    ?assertEqual(ersip_hdr_callid:make(<<"deadbeef">>), ersip_hdr_callid:make(CallId1H)),
    ?assertEqual(ersip_hdr_callid:make(<<"deadbeef1">>), ersip_hdr_callid:make(CallId2H)).

conn_stream_no_content_len_test() ->
    RemoteIP = {127, 0, 0, 1},
    Conn = create_stream_conn(RemoteIP, 5090),
    %% 1. received added if domain name:
    Msgs =
        <<"INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
          "INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef1",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf ?crlf "Test"
        >>,
    {_, [{new_request, NewMsg1}, {disconnect, {error, _}}]} = ersip_conn:conn_data(Msgs, Conn),
    CallId1H = ersip_msg:get(<<"call-id">>, NewMsg1),
    ?assertEqual(ersip_hdr_callid:make(<<"deadbeef">>), ersip_hdr_callid:make(CallId1H)).

conn_datagram_truncated_message_test() ->
    RemoteIP = {127, 0, 0, 1},
    Conn = create_conn(RemoteIP, 5090),
    Msg = <<"INVITE">>,
    {_, [{bad_message, Msg, truncated}]} = ersip_conn:conn_data(Msg, Conn).


add_via_test() ->
    %% Check adding sent-by by connection object.
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),
    %% 1. received added if domain name:
    Msg =
        <<"INVITE sip:bob@biloxi.com SIP/2.0"
          ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
          ?crlf "Max-Forwards: 70"
          ?crlf "To: Bob <sip:bob@biloxi.com>"
          ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
          ?crlf "Call-ID: deadbeef",
          ?crlf "CSeq: 314159 INVITE"
          ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
          ?crlf "Content-Type: application/sdp"
          ?crlf "Content-Length: 4"
          ?crlf ?crlf "Test"
        >>,
    RawMsg = parse_msg(Msg),
    Branch = ersip_branch:make_rfc3261(<<"12345">>),
    RawMsg1 = ersip_conn:add_via(RawMsg, Branch, Conn),
    ViaH = ersip_msg:get(<<"via">>, RawMsg1),
    {ok, TopMost} = ersip_hdr_via:topmost_via(ViaH),
    LocalHost = ersip_host:make(<<"127.0.0.2">>),
    ?assertEqual({sent_by, LocalHost, 5061}, ersip_hdr_via:sent_by(TopMost)),
    ?assertEqual({sent_protocol, <<"SIP">>, <<"2.0">>, UDP}, ersip_hdr_via:sent_protocol(TopMost)).

take_via_test() ->
    %% Check taking via from response.
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),
    Msg =
        << "SIP/2.0 200 OK" ?crlf
           "Via: SIP/2.0/UDP 127.0.0.2:5061;branch=z9hG4bKhjhs8ass877" ?crlf
           " ;received=192.0.2.4" ?crlf
           "To: <sip:carol@chicago.com>;tag=93810874" ?crlf
           "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
           "Call-ID: a84b4c76e66710" ?crlf
           "CSeq: 63104 OPTIONS" ?crlf
           "Contact: <sip:carol@chicago.com>" ?crlf
           "Contact: <mailto:carol@chicago.com>" ?crlf
           "Allow: INVITE, ACK, CANCEL, OPTIONS, BYE" ?crlf
           "Accept: application/sdp" ?crlf
           "Accept-Encoding: gzip" ?crlf
           "Accept-Language: en" ?crlf
           "Supported: foo" ?crlf
           "Content-Type: application/sdp" ?crlf
           "Content-Length: 4" ?crlf ?crlf
           "Test">>,
    {_, [{new_response, Via, RawMsg}]} = ersip_conn:conn_data(Msg, Conn),
    %% 1. Smoke check of Via:
    LocalHost = ersip_host:make(<<"127.0.0.2">>),
    ?assertEqual({sent_by, LocalHost, 5061}, ersip_hdr_via:sent_by(Via)),
    %% 2. Check no via in response
    check_no_via(RawMsg),
    ok.

take_via_neg_no_via_test() ->
    %% Check that error is returned if no via in response.
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),
    Msg =
        << "SIP/2.0 200 OK" ?crlf
           "To: <sip:carol@chicago.com>;tag=93810874" ?crlf
           "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
           "Call-ID: a84b4c76e66710" ?crlf
           "CSeq: 63104 OPTIONS" ?crlf
           "Contact: <sip:carol@chicago.com>" ?crlf
           "Contact: <mailto:carol@chicago.com>" ?crlf
           "Allow: INVITE, ACK, CANCEL, OPTIONS, BYE" ?crlf
           "Accept: application/sdp" ?crlf
           "Accept-Encoding: gzip" ?crlf
           "Accept-Language: en" ?crlf
           "Supported: foo" ?crlf
           "Content-Type: application/sdp" ?crlf
           "Content-Length: 4" ?crlf ?crlf
           "Test">>,
    %% Note that it is synthetic test. We cannot get message without
    %% Via from connection.
    RawMsg = parse_msg(Msg),
    Result = ersip_conn:take_via(RawMsg, Conn),
    ?assertMatch({error, no_via}, Result),
    ok.

take_via_neg_bad_via_test() ->
    %% Check that error is returned if via is malformed.
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),
    Msg =
        << "SIP/2.0 200 OK" ?crlf
           "Via: SIP/2.0/UDP 127.0.0.2:5061&branch=z9hG4bKhjhs8ass877" ?crlf
           " ;received=192.0.2.4" ?crlf
           "To: <sip:carol@chicago.com>;tag=93810874" ?crlf
           "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
           "Call-ID: a84b4c76e66710" ?crlf
           "CSeq: 63104 OPTIONS" ?crlf
           "Contact: <sip:carol@chicago.com>" ?crlf
           "Contact: <mailto:carol@chicago.com>" ?crlf
           "Allow: INVITE, ACK, CANCEL, OPTIONS, BYE" ?crlf
           "Accept: application/sdp" ?crlf
           "Accept-Encoding: gzip" ?crlf
           "Accept-Language: en" ?crlf
           "Supported: foo" ?crlf
           "Content-Type: application/sdp" ?crlf
           "Content-Length: 4" ?crlf ?crlf
           "Test">>,
    %% Note that it is synthetic test. We cannot get message with bad
    %% topmost Via from connection.
    RawMsg = parse_msg(Msg),
    Result = ersip_conn:take_via(RawMsg, Conn),
    ?assertMatch({error, {bad_via, _}}, Result),
    ok.

take_via_neg_host_mismatch_test() ->
    %% Check that error returned if host in via does not match
    %% connection.
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),
    Msg =
        << "SIP/2.0 200 OK" ?crlf
           "Via: SIP/2.0/UDP 127.0.1.2:5061;branch=z9hG4bKhjhs8ass877" ?crlf
           " ;received=192.0.2.4" ?crlf
           "To: <sip:carol@chicago.com>;tag=93810874" ?crlf
           "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
           "Call-ID: a84b4c76e66710" ?crlf
           "CSeq: 63104 OPTIONS" ?crlf
           "Contact: <sip:carol@chicago.com>" ?crlf
           "Contact: <mailto:carol@chicago.com>" ?crlf
           "Allow: INVITE, ACK, CANCEL, OPTIONS, BYE" ?crlf
           "Accept: application/sdp" ?crlf
           "Accept-Encoding: gzip" ?crlf
           "Accept-Language: en" ?crlf
           "Supported: foo" ?crlf
           "Content-Type: application/sdp" ?crlf
           "Content-Length: 4" ?crlf ?crlf
           "Test">>,
    ?assertMatch({_, [{bad_message, _Msg, {error, {via_mismatch, _, _}}}]},
                 ersip_conn:conn_data(Msg, Conn)),
    ok.

take_via_neg_transport_mismatch_test() ->
    %% Check that error returned if transport in sent-protocol does
    %% not match connection.
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),
    Msg =
        << "SIP/2.0 200 OK" ?crlf
           "Via: SIP/2.0/TCP 127.0.0.2:5061;branch=z9hG4bKhjhs8ass877" ?crlf
           " ;received=192.0.2.4" ?crlf
           "To: <sip:carol@chicago.com>;tag=93810874" ?crlf
           "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
           "Call-ID: a84b4c76e66710" ?crlf
           "CSeq: 63104 OPTIONS" ?crlf
           "Contact: <sip:carol@chicago.com>" ?crlf
           "Contact: <mailto:carol@chicago.com>" ?crlf
           "Allow: INVITE, ACK, CANCEL, OPTIONS, BYE" ?crlf
           "Accept: application/sdp" ?crlf
           "Accept-Encoding: gzip" ?crlf
           "Accept-Language: en" ?crlf
           "Supported: foo" ?crlf
           "Content-Type: application/sdp" ?crlf
           "Content-Length: 4" ?crlf ?crlf
           "Test">>,
    ?assertMatch({_, [{bad_message, _Msg, {error, {via_mismatch, _, _}}}]},
                 ersip_conn:conn_data(Msg, Conn)),
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================

create_conn(RemoteAddr, RemotePort) ->
    UDP  = ersip_transport:make(udp),
    ersip_conn:new({127, 0, 0, 1}, 5060, RemoteAddr, RemotePort, UDP, #{}).

create_stream_conn(RemoteAddr, RemotePort) ->
    TCP  = ersip_transport:make(tcp),
    ersip_conn:new({127, 0, 0, 1}, 5060, RemoteAddr, RemotePort, TCP, #{}).


check_received(RemoteIp, Msg, Conn) ->
    {_, [{new_request, NewMsg}]} = ersip_conn:conn_data(Msg, Conn),
    ViaH = ersip_msg:get(<<"via">>, NewMsg),
    {ok, Via} = ersip_hdr_via:topmost_via(ViaH),
    RemoteHost = ersip_host:make(RemoteIp),
    ?assertMatch(#{received := RemoteHost}, ersip_hdr_via:params(Via)).

check_rport(RemotePort, Msg, Conn) ->
    {_, [{new_request, NewMsg}]} = ersip_conn:conn_data(Msg, Conn),
    ViaH = ersip_msg:get(<<"via">>, NewMsg),
    {ok, Via} = ersip_hdr_via:topmost_via(ViaH),
    ?assertMatch(#{rport := RemotePort}, ersip_hdr_via:params(Via)).

check_no_received(Msg, Conn) ->
    {_, [{new_request, NewMsg}]} = ersip_conn:conn_data(Msg, Conn),
    ViaH = ersip_msg:get(<<"via">>, NewMsg),
    {ok, Via} = ersip_hdr_via:topmost_via(ViaH),
    ?assertMatch(error, maps:find(received, ersip_hdr_via:params(Via))).

parse_msg(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, RawMsg}, _P2} = ersip_parser:parse(P),
    RawMsg.

check_no_via(RawMsgNoVia) ->
    ViaH = ersip_msg:get(<<"via">>, RawMsgNoVia),
    ?assertEqual({error, no_via}, ersip_hdr_via:topmost_via(ViaH)).
