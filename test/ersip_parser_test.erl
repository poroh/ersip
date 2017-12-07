%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message parser tests
%%

-module(ersip_parser_test).

-include_lib("eunit/include/eunit.hrl").

-define(crlf, "\r\n").

basic_request_parse_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
      ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
      ?crlf "Max-Forwards: 70"
      ?crlf "To: Bob <sip:bob@biloxi.com>"
      ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
      ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
      ?crlf "CSeq: 314159 INVITE"
      ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
      ?crlf "Content-Type: application/sdp"
      ?crlf "Content-Length: 142"
      ?crlf ?crlf
          >>,
    P  = ersip_parser:new(),
    P1 = ersip_parser:add_binary(Msg, P),
    {{error, not_implemented_yet}, _P2 } = ersip_parser:parse(P1).


basic_response_parse_test() ->
    Msg = <<"SIP/2.0 200 OK"
      ?crlf "Via: SIP/2.0/UDP server10.biloxi.com"
      ?crlf "   ;branch=z9hG4bKnashds8;received=192.0.2.3"
      ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
      ?crlf "   ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2"
      ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com"
      ?crlf "   ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
      ?crlf "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf"
      ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
      ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
      ?crlf "CSeq: 314159 INVITE"
      ?crlf "Contact: <sip:bob@192.0.2.4>"
      ?crlf "Content-Type: application/sdp"
      ?crlf "Content-Length: 131"
      ?crlf ?crlf
          >>,
    P  = ersip_parser:new(),
    P1 = ersip_parser:add_binary(Msg, P),
    {{error, not_implemented_yet}, _P2 } = ersip_parser:parse(P1).
        
    
