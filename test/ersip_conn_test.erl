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
    RemoteIP = { 127, 0, 0, 1 },
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

conn_error_on_via_test() ->
    RemoteIP = { 127, 0, 0, 1 },
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
    { _, [ { bad_message, BadMsg, { error, _ } } ] } = ersip_conn:conn_data(MsgWithoutVia, Conn),
    CallIdH = ersip_msg:get(<<"call-id">>, BadMsg),
    ?assertEqual(ersip_hdr_callid:make(<<"deadbeef">>), ersip_hdr_callid:make(CallIdH)).
    

conn_stream_test() ->
    RemoteIP = { 127, 0, 0, 1 },
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
    { _, [ { new_message, NewMsg1 }, { new_message, NewMsg2 } ] } = ersip_conn:conn_data(Msgs, Conn),
    CallId1H = ersip_msg:get(<<"call-id">>, NewMsg1),
    CallId2H = ersip_msg:get(<<"call-id">>, NewMsg2),
    ?assertEqual(ersip_hdr_callid:make(<<"deadbeef">>), ersip_hdr_callid:make(CallId1H)),
    ?assertEqual(ersip_hdr_callid:make(<<"deadbeef1">>), ersip_hdr_callid:make(CallId2H)).

conn_stream_no_content_len_test() ->
    RemoteIP = { 127, 0, 0, 1 },
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
    { _, [ { new_message, NewMsg1 }, { disconnect, { error, _ } } ] } = ersip_conn:conn_data(Msgs, Conn),
    CallId1H = ersip_msg:get(<<"call-id">>, NewMsg1),
    ?assertEqual(ersip_hdr_callid:make(<<"deadbeef">>), ersip_hdr_callid:make(CallId1H)).


%%%===================================================================
%%% Helpers
%%%===================================================================

create_conn(RemoteAddr, RemotePort) ->
    UDP  = ersip_transport:make(udp),
    ersip_conn:new({ 127, 0, 0, 1 }, 5060, RemoteAddr, RemotePort, UDP, #{}).

create_stream_conn(RemoteAddr, RemotePort) ->
    TCP  = ersip_transport:make(tcp),
    ersip_conn:new({ 127, 0, 0, 1 }, 5060, RemoteAddr, RemotePort, TCP, #{}).


check_received(RemoteIp, Msg, Conn) ->
    { _, [ { new_message, NewMsg } ] } = ersip_conn:conn_data(Msg, Conn),
    ViaH = ersip_msg:get(<<"via">>, NewMsg),
    { ok, Via } = ersip_hdr_via:topmost_via(ViaH),
    RemoteHost = ersip_host:make(RemoteIp),
    ?assertMatch(#{ received := RemoteHost }, ersip_hdr_via:params(Via)).
