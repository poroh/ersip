%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Server INVITE transaction test
%%

-module(ersip_trans_inv_server_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

reliable_canonnical_flow_test() ->
    InviteSipMsg = invite(),
    {InvTrans0, SE} = ersip_trans_inv_server:new(reliable, InviteSipMsg, #{}),

    %% Req #1: INVITE must be passed to TU when entering Proceeding state.
    ?assertEqual({tu_result, InviteSipMsg}, lists:keyfind(tu_result, 1, SE)),

    %% Req #2: INVITE server transaction must generate 100 Trying response
    {send_response, Resp100} = lists:keyfind(send_response, 1, SE),
    ?assertEqual(response, ersip_sipmsg:type(Resp100)),
    ?assertEqual(100, ersip_sipmsg:status(Resp100)),

    %% Req #3: INVITE server tranasaction passes provisional response
    RingingSipMsg = ringing(),
    {InvTrans1, SE1} = ersip_trans_inv_server:event({send, RingingSipMsg}, InvTrans0),
    {send_response, RingingSipMsg} = lists:keyfind(send_response, 1, SE1),

    %% Req #4: INVITE server tranasaction passes final 200 OK message
    OKSipMsg = ok200(),
    {InvTrans2, SE2} = ersip_trans_inv_server:event({send, OKSipMsg}, InvTrans1),
    {send_response, OKSipMsg} = lists:keyfind(send_response, 1, SE2),

    %% Req #5: Timer L is set after passing 200 OK message (RFC6026 Accepted state):
    {set_timer, {Timeout, {timer, Timer} = TimerLEv}} = lists:keyfind(set_timer, 1, SE2),
    ?assertEqual(32000, Timeout),
    ?assertEqual(timer_l, Timer),

    %% Req #6: Retransmission of 2xx are passed to transport layer (RFC6026):
    {InvTrans3, SE3} = ersip_trans_inv_server:event({send, OKSipMsg}, InvTrans2),
    {send_response, OKSipMsg} = lists:keyfind(send_response, 1, SE3),

    %% Req #7: ACKs matching transaction are passed to TU (RFC6026):
    ACKSipMsg = ack(),
    {InvTrans4, SE4} = ersip_trans_inv_server:event({received, ACKSipMsg}, InvTrans3),
    ?assertEqual({tu_result, ACKSipMsg}, lists:keyfind(tu_result, 1, SE4)),


    %% Req #8: INVITE server tranasaction is stopped after timer L is fired
    {_InvTrans3, SE5} = ersip_trans_inv_server:event(TimerLEv, InvTrans4),
    ?assertEqual([{clear_trans, normal}], SE5),
    ok.


unreliable_with_retransmits_test() ->
    InviteSipMsg = invite(),
    {InvTrans0, SE} = ersip_trans_inv_server:new(unreliable, InviteSipMsg, #{}),

    %% Req #1: INVITE must be passed to TU when entering Proceeding state.
    ?assertEqual({tu_result, InviteSipMsg}, lists:keyfind(tu_result, 1, SE)),

    %% Req #2: INVITE server transaction must generate 100 Trying response
    {send_response, Resp100} = lists:keyfind(send_response, 1, SE),
    ?assertEqual(response, ersip_sipmsg:type(Resp100)),
    ?assertEqual(100, ersip_sipmsg:status(Resp100)),

    %% Req #3: 100 Trying is resent on INVITE retransmit:
    {InvTrans1, SE1} = ersip_trans_inv_server:event({received, InviteSipMsg}, InvTrans0),
    {send_response, Resp100} = lists:keyfind(send_response, 1, SE1),

    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================

invite() ->
    parse_message(invite_bin()).

ringing() ->
    parse_message(ringing_bin()).

ok200() ->
    parse_message(ok200_bin()).

ack() ->
    parse_message(ack_bin()).

-define(crlf, "\r\n").

invite_bin() ->
    <<"INVITE sip:bob@biloxi.com SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8" ?crlf
      "Max-Forwards: 70" ?crlf
      "To: Bob <sip:bob@biloxi.com>" ?crlf
      "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
      "Call-ID: a84b4c76e66710" ?crlf
      "CSeq: 314159 INVITE" ?crlf
      "Contact: <sip:alice@pc33.atlanta.com>" ?crlf
      "Content-Type: application/sdp" ?crlf
      "Content-Length: 0" ?crlf
      ?crlf>>.

ringing_bin() ->
    <<"SIP/2.0 180 Ringing" ?crlf
      "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8" ?crlf
      " ;received=192.0.2.1" ?crlf
      "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf" ?crlf
      "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
      "Call-ID: a84b4c76e66710" ?crlf
      "Contact: <sip:bob@192.0.2.4>" ?crlf
      "CSeq: 314159 INVITE" ?crlf
      "Content-Length: 0" ?crlf
      ?crlf>>.

ok200_bin() ->
    <<"SIP/2.0 200 OK" ?crlf
      "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8" ?crlf
      " ;received=192.0.2.1" ?crlf
      "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf" ?crlf
      "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
      "Call-ID: a84b4c76e66710" ?crlf
      "CSeq: 314159 INVITE" ?crlf
      "Contact: <sip:bob@192.0.2.4>" ?crlf
      "Content-Type: application/sdp" ?crlf
      "Content-Length: 0" ?crlf
      ?crlf>>.

ack_bin() ->
    <<"ACK sip:bob@192.0.2.4 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds9" ?crlf
      "Max-Forwards: 70" ?crlf
      "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf" ?crlf
      "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
      "Call-ID: a84b4c76e66710" ?crlf
      "CSeq: 314159 ACK" ?crlf
      "Content-Length: 0" ?crlf
      ?crlf>>.

parse_message(Bin) when is_binary(Bin) ->
    P  = ersip_parser:new_dgram(Bin),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg, all),
    SipMsg.
