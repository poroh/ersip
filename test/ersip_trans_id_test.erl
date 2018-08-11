%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP transaction id test
%%

-module(ersip_trans_id_test).

-include_lib("eunit/include/eunit.hrl").

-define(crlf, "\r\n").

%%%===================================================================
%%% Cases
%%%===================================================================

server_transaction_id_rfc3261_equal_test() ->
    InviteMsg1 = <<"INVITE sip:bob@biloxi.com SIP/2.0"
                   ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
                   ?crlf "Max-Forwards: 70"
                   ?crlf "To: Bob <sip:bob@biloxi.com>"
                   ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
                   ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
                   ?crlf "CSeq: 314159 INVITE"
                   ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
                   ?crlf "Content-Type: application/sdp"
                   ?crlf "Content-Length: 0"
                   ?crlf ?crlf
                 >>,
    InviteMsg2 = <<"INVITE sip:bob@biloxi.com SIP/2.0"
                   ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
                   ?crlf "Max-Forwards: 70"
                   ?crlf "To: Not a Bob <sip:bob@biloxi.com>"
                   ?crlf "From: Alice Renamed <sip:alice@atlanta.com>;tag=1928301774"
                   ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
                   ?crlf "CSeq: 314160 INVITE"
                   ?crlf "Contact: Another <sip:alice@pc33.atlanta.com>"
                   ?crlf "Content-Type: application/sdp"
                   ?crlf "Content-Length: 0"
                   ?crlf ?crlf
                 >>,
    AckMsg  =    <<"ACK sip:bob@biloxi.com SIP/2.0"
                   ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
                   ?crlf "Max-Forwards: 70"
                   ?crlf "To: Not a Bob <sip:bob@biloxi.com>"
                   ?crlf "From: Alice Renamed <sip:alice@atlanta.com>;tag=1928301774"
                   ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
                   ?crlf "CSeq: 314160 ACK"
                   ?crlf "Contact: Another <sip:alice@pc33.atlanta.com>"
                   ?crlf "Content-Type: application/sdp"
                   ?crlf "Content-Length: 0"
                   ?crlf ?crlf
                 >>,
    CancelMsg  = <<"CANCEL sip:bob@biloxi.com SIP/2.0"
                   ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
                   ?crlf "Max-Forwards: 70"
                   ?crlf "To: Not a Bob <sip:bob@biloxi.com>"
                   ?crlf "From: Alice Renamed <sip:alice@atlanta.com>;tag=1928301774"
                   ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
                   ?crlf "CSeq: 314160 CANCEL"
                   ?crlf "Contact: Another <sip:alice@pc33.atlanta.com>"
                   ?crlf "Content-Type: application/sdp"
                   ?crlf "Content-Length: 0"
                   ?crlf ?crlf
                 >>,
    ?assertEqual(calc_server_trans_id(InviteMsg1), calc_server_trans_id(InviteMsg2)),
    ?assertEqual(calc_server_trans_id(InviteMsg1), calc_server_trans_id(AckMsg)),
    ?assertNotEqual(calc_server_trans_id(InviteMsg1), calc_server_trans_id(CancelMsg)),
    ?assertEqual(calc_server_trans_id(InviteMsg1), calc_server_cancel_trans_id(CancelMsg)),
    ok.

server_transaction_id_rfc3261_not_equal_test() ->
    InviteMsg1 = <<"INVITE sip:bob@biloxi.com SIP/2.0"
                   ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
                   ?crlf "Max-Forwards: 70"
                   ?crlf "To: Bob <sip:bob@biloxi.com>"
                   ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
                   ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
                   ?crlf "CSeq: 314159 INVITE"
                   ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
                   ?crlf "Content-Type: application/sdp"
                   ?crlf "Content-Length: 0"
                   ?crlf ?crlf
                 >>,
    InviteMsg2 = <<"INVITE sip:bob@biloxi.com SIP/2.0"
                   ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds_1"
                   ?crlf "Max-Forwards: 70"
                   ?crlf "To: Not a Bob <sip:bob@biloxi.com>"
                   ?crlf "From: Alice Renamed <sip:alice@atlanta.com>;tag=1928301774"
                   ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
                   ?crlf "CSeq: 314160 INVITE"
                   ?crlf "Contact: Another <sip:alice@pc33.atlanta.com>"
                   ?crlf "Content-Type: application/sdp"
                   ?crlf "Content-Length: 0"
                   ?crlf ?crlf
                 >>,
    ?assertNotEqual(calc_server_trans_id(InviteMsg1), calc_server_trans_id(InviteMsg2)).


server_transaction_id_rfc2543_equal_test() ->
    InviteMsg =
        <<"INVITE sip:watson@h.bell-tel.com SIP/2.0" ?crlf
          "Via:     SIP/2.0/UDP sip.ieee.org ;branch=1" ?crlf
          "Via:     SIP/2.0/UDP c.bell-tel.com" ?crlf
          "From:    A. Bell <sip:a.g.bell@bell-tel.com>" ?crlf
          "To:      T. Watson <sip:t.watson@ieee.org>" ?crlf
          "Call-ID: 31415@c.bell-tel.com" ?crlf
          "CSeq:    1 INVITE" ?crlf
          ?crlf
        >>,
    %% ACK After 404:
    AckMsg =
        <<"ACK sip:watson@h.bell-tel.com SIP/2.0" ?crlf
          "Via:     SIP/2.0/UDP sip.ieee.org ;branch=1" ?crlf
          "From:    A. Bell <sip:a.g.bell@bell-tel.com>" ?crlf
          "To:      T. Watson <sip:t.watson@ieee.org>;tag=87454273" ?crlf
          "Call-ID: 31415@c.bell-tel.com" ?crlf
          "CSeq:    1 ACK" ?crlf
          "" ?crlf>>,
    CancelMsg =
        <<"CANCEL sip:watson@h.bell-tel.com SIP/2.0" ?crlf
          "Via:     SIP/2.0/UDP sip.ieee.org ;branch=1" ?crlf
          "From:    A. Bell <sip:a.g.bell@bell-tel.com>" ?crlf
          "To:      T. Watson <sip:t.watson@ieee.org>" ?crlf
          "Call-ID: 31415@c.bell-tel.com" ?crlf
          "CSeq:    1 CANCEL" ?crlf
          "" ?crlf>>,
    ?assertEqual(calc_server_trans_id(InviteMsg), calc_server_trans_id(AckMsg)),
    ?assertNotEqual(calc_server_trans_id(InviteMsg), calc_server_trans_id(CancelMsg)),
    ?assertEqual(calc_server_trans_id(InviteMsg), calc_server_cancel_trans_id(CancelMsg)),
    ok.

server_transaction_id_rfc2543_equal_with_rport_test() ->
    InviteMsg =
        <<"INVITE sip:watson@h.bell-tel.com SIP/2.0" ?crlf
          "Via:     SIP/2.0/UDP sip.ieee.org ;rport;branch=1" ?crlf
          "Via:     SIP/2.0/UDP c.bell-tel.com" ?crlf
          "From:    A. Bell <sip:a.g.bell@bell-tel.com>" ?crlf
          "To:      T. Watson <sip:t.watson@ieee.org>" ?crlf
          "Call-ID: 31415@c.bell-tel.com" ?crlf
          "CSeq:    1 INVITE" ?crlf
          ?crlf
        >>,
    %% ACK After 404:
    AckMsg =
        <<"ACK sip:watson@h.bell-tel.com SIP/2.0" ?crlf
          "Via:     SIP/2.0/UDP sip.ieee.org ; rport; branch=1" ?crlf
          "From:    A. Bell <sip:a.g.bell@bell-tel.com>" ?crlf
          "To:      T. Watson <sip:t.watson@ieee.org>;tag=87454273" ?crlf
          "Call-ID: 31415@c.bell-tel.com" ?crlf
          "CSeq:    1 ACK" ?crlf
          "" ?crlf>>,
    ?assertEqual(calc_server_trans_id(InviteMsg),
                 calc_server_trans_id(AckMsg)).

server_transaction_id_rfc2543_equal_with_set_rport_test() ->
    InviteMsg =
        <<"INVITE sip:watson@h.bell-tel.com SIP/2.0" ?crlf
          "Via:     SIP/2.0/UDP sip.ieee.org ;rport=4231;branch=1" ?crlf
          "Via:     SIP/2.0/UDP c.bell-tel.com" ?crlf
          "From:    A. Bell <sip:a.g.bell@bell-tel.com>" ?crlf
          "To:      T. Watson <sip:t.watson@ieee.org>" ?crlf
          "Call-ID: 31415@c.bell-tel.com" ?crlf
          "CSeq:    1 INVITE" ?crlf
          ?crlf
        >>,
    %% ACK After 404:
    AckMsg =
        <<"ACK sip:watson@h.bell-tel.com SIP/2.0" ?crlf
          "Via:     SIP/2.0/UDP sip.ieee.org;rport=4231; branch=1" ?crlf
          "From:    A. Bell <sip:a.g.bell@bell-tel.com>" ?crlf
          "To:      T. Watson <sip:t.watson@ieee.org>;tag=87454273" ?crlf
          "Call-ID: 31415@c.bell-tel.com" ?crlf
          "CSeq:    1 ACK" ?crlf
          "" ?crlf>>,
    ?assertEqual(calc_server_trans_id(InviteMsg),
                 calc_server_trans_id(AckMsg)).

server_transaction_id_rfc2543_equal_without_branch_test() ->
    InviteMsg =
        <<"INVITE sip:watson@h.bell-tel.com SIP/2.0" ?crlf
          "Via:     SIP/2.0/UDP sip.ieee.org" ?crlf
          "Via:     SIP/2.0/UDP c.bell-tel.com" ?crlf
          "From:    A. Bell <sip:a.g.bell@bell-tel.com>" ?crlf
          "To:      T. Watson <sip:t.watson@ieee.org>" ?crlf
          "Call-ID: 31415@c.bell-tel.com" ?crlf
          "CSeq:    1 INVITE" ?crlf
          ?crlf
        >>,
    %% ACK After 404:
    AckMsg =
        <<"ACK sip:watson@h.bell-tel.com SIP/2.0" ?crlf
          "Via:     SIP/2.0/UDP sip.ieee.org" ?crlf
          "From:    A. Bell <sip:a.g.bell@bell-tel.com>" ?crlf
          "To:      T. Watson <sip:t.watson@ieee.org>;tag=87454273" ?crlf
          "Call-ID: 31415@c.bell-tel.com" ?crlf
          "CSeq:    1 ACK" ?crlf
          "" ?crlf>>,
    ?assertEqual(calc_server_trans_id(InviteMsg),
                 calc_server_trans_id(AckMsg)).

server_transaction_id_rfc2543_non_invite_test() ->
    Register0 =
        <<"REGISTER sip:bell-tel.com SIP/2.0" ?crlf
          "Via: SIP/2.0/UDP saturn.bell-tel.com" ?crlf
          "From: sip:watson@bell-tel.com" ?crlf
          "To: sip:watson@bell-tel.com" ?crlf
          "Call-ID: 70710@saturn.bell-tel.com" ?crlf
          "CSeq: 1 REGISTER" ?crlf
          "Contact: <sip:watson@saturn.bell-tel.com:3890;transport=udp>" ?crlf
          "Expires: 7200" ?crlf
          "" ?crlf>>,
    Register1 =
        <<"REGISTER sip:bell-tel.com SIP/2.0" ?crlf
          "Via: SIP/2.0/UDP saturn.bell-tel.com" ?crlf
          "From: sip:watson@bell-tel.com" ?crlf
          "To: sip:watson@bell-tel.com" ?crlf
          "Call-ID: 70710@saturn.bell-tel.com" ?crlf
          "CSeq: 1 REGISTER" ?crlf
          "Contact: <sip:watson@saturn.bell-tel.com:3890;transport=udp>" ?crlf
          "Expires: 200" ?crlf
          "" ?crlf>>,
    ?assertEqual(calc_server_trans_id(Register0),
                 calc_server_trans_id(Register1)).

%%%===================================================================
%%% Helpers
%%%===================================================================

make_sipmsg(Binary) ->
    P  = ersip_parser:new_dgram(Binary),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg, all),
    SipMsg.

calc_server_trans_id(Binary) ->
    SipMsg = make_sipmsg(Binary),
    ersip_trans_id:make_server(SipMsg).

calc_server_cancel_trans_id(Binary) ->
    SipMsg = make_sipmsg(Binary),
    ersip_trans_id:make_server_cancel(SipMsg).

