%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Request test
%%%

-module(ersip_request_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

new_nexthop_from_ruri_test() ->
    SipMsg = default_sipmsg(),
    SipMsg1  = ersip_sipmsg:set_ruri(ersip_uri:make(<<"sip:test@example.com">>), SipMsg),
    SipReq = ersip_request:new(SipMsg1, ersip_branch:make_random(10)),
    ?assertEqual(<<"sip:test@example.com">>, ersip_uri:assemble_bin(ersip_request:nexthop(SipReq))),
    ok.

new_nexthop_from_route_test() ->
    SipMsg = default_sipmsg(),
    Route = ersip_hdr_route_list:make(<<"<sip:alice@atlanta.com>,<sip:carol@chicago.com>,<sip:bob@biloxi.com>">>),
    SipMsg1  = ersip_sipmsg:set(route, Route, SipMsg),
    SipReq = ersip_request:new(SipMsg1, ersip_branch:make_random(10)),
    ?assertEqual(<<"sip:alice@atlanta.com">>, ersip_uri:assemble_bin(ersip_request:nexthop(SipReq))),
    ok.

set_nexthop_test() ->
    SipMsg = default_sipmsg(),
    SipMsg1  = ersip_sipmsg:set_ruri(ersip_uri:make(<<"sip:test@example.com">>), SipMsg),
    SipReq = ersip_request:new(SipMsg1, ersip_branch:make_random(10)),
    ?assertEqual(<<"sip:test@example.com">>, ersip_uri:assemble_bin(ersip_request:nexthop(SipReq))),
    SipReq1 = ersip_request:set_nexthop(ersip_uri:make(<<"sip:alice@atlanta.com">>), SipReq),
    ?assertEqual(<<"sip:alice@atlanta.com">>, ersip_uri:assemble_bin(ersip_request:nexthop(SipReq1))),
    ok.

dialog_id_test() ->
    SipMsg = default_sipmsg(),
    SipReq = ersip_request:new(SipMsg, ersip_branch:make_random(10)),
    ?assertEqual(ersip_sipmsg:dialog_id(uac, SipMsg), ersip_request:dialog_id(SipReq)),
    ok.


%%===================================================================
%% Implementation
%%===================================================================

-define(crlf, "\r\n").

default_msg() ->
    <<"INVITE sip:bob@biloxi.com SIP/2.0"
      ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
      ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
      ?crlf "Max-Forwards: 70"
      ?crlf "To: Bob <sip:bob@biloxi.com>"
      ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
      ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
      ?crlf "CSeq: 314159 INVITE"
      ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
      ?crlf "Content-Type: application/sdp"
      ?crlf "Content-Length: 4"
      ?crlf ?crlf "Test"
    >>.

default_sipmsg() ->
    ersip_sipmsg:make(default_msg()).


