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

reliable_canonnical_2xx_flow_test() ->
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


reliable_canonnical_4xx_flow_test() ->
    InviteSipMsg = invite(),
    {InvTrans0, _} = ersip_trans_inv_server:new(reliable, InviteSipMsg, #{}),

    %% Req #1: INVITE server tranasaction passes final 4xx message
    NotFoundSipMsg = notfound404(),
    {InvTrans1, SE1} = ersip_trans_inv_server:event({send, NotFoundSipMsg}, InvTrans0),
    {send_response, NotFoundSipMsg} = lists:keyfind(send_response, 1, SE1),

    %% Req #2: Timer H is set when Completed state is entered:
    {set_timer, {Timeout, {timer, Timer} = TimerHEv}} = lists:keyfind(set_timer, 1, SE1),
    ?assertEqual(32000, Timeout),
    ?assertEqual(timer_h, Timer),

    %% Req #3: ACKs matching transaction are absorbed by transaction:
    ACKSipMsg = ack(),
    {_, SE2} = ersip_trans_inv_server:event({received, ACKSipMsg}, InvTrans1),
    ?assertEqual(false, lists:keyfind(tu_result, 1, SE2)),

    %% Req #4: Transaction is cleared for reliable transport. (Timer I is set to 0).
    ?assertEqual({clear_trans, normal}, lists:keyfind(clear_trans, 1, SE2)),

    %% Req #5: Transaction is cleared with no_ack if TimerH is fired:
    {_, SE3} = ersip_trans_inv_server:event(TimerHEv, InvTrans1),
    ?assertEqual({clear_trans, no_ack}, lists:keyfind(clear_trans, 1, SE3)),
    ok.

unreliable_with_retransmits_2xx_test() ->
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

    %% Req #4: INVITE server tranasaction passes final 200 OK message
    OKSipMsg = ok200(),
    {InvTrans2, SE2} = ersip_trans_inv_server:event({send, OKSipMsg}, InvTrans1),
    {send_response, OKSipMsg} = lists:keyfind(send_response, 1, SE2),

    %% Req #5: Timer L is set to absort INVITE retransmits (RFC6026)
    {set_timer, {Timeout, {timer, Timer} = TimerLEv}} = lists:keyfind(set_timer, 1, SE2),
    ?assertEqual(32000, Timeout),
    ?assertEqual(timer_l, Timer),

    %% Req #6: INVITE retransmits are ignored in Accepted state.
    {InvTrans3, SE3} = ersip_trans_inv_server:event({received, InviteSipMsg}, InvTrans2),
    ?assertEqual(false, lists:keyfind(tu_result, 1, SE3)),

    %% Req #7: INVITE server tranasaction passes retransmissions of
    %% final 200 OK message to Transport layer
    {InvTrans4, SE4} = ersip_trans_inv_server:event({send, OKSipMsg}, InvTrans3),
    {send_response, OKSipMsg} = lists:keyfind(send_response, 1, SE4),

    {InvTrans5, SE5} = ersip_trans_inv_server:event({send, OKSipMsg}, InvTrans4),
    {send_response, OKSipMsg} = lists:keyfind(send_response, 1, SE5),

    %% Req #8: ACKs are passed to TU in Accepted state
    ACKSipMsg = ack(),
    {InvTrans6, SE6} = ersip_trans_inv_server:event({received, ACKSipMsg}, InvTrans5),
    ?assertEqual({tu_result, ACKSipMsg}, lists:keyfind(tu_result, 1, SE6)),

    {InvTrans7, SE7} = ersip_trans_inv_server:event({received, ACKSipMsg}, InvTrans6),
    ?assertEqual({tu_result, ACKSipMsg}, lists:keyfind(tu_result, 1, SE7)),

    %% Req #9: Transaction is cleared if timer L is fired in Accepted state.
    %% We check it for InvTrans1..6
    lists:foreach(fun(InvTrans) ->
                          {_, LSE} = ersip_trans_inv_server:event(TimerLEv, InvTrans),
                          ?assertEqual({clear_trans, normal}, lists:keyfind(clear_trans, 1, LSE))
                  end,
                  [InvTrans2, InvTrans3, InvTrans4, InvTrans5, InvTrans6, InvTrans7]),
    ok.

unreliable_with_retransmits_4xx_test() ->
    InviteSipMsg = invite(),
    {InvTrans0, _} = ersip_trans_inv_server:new(unreliable, InviteSipMsg, #{}),

    %% Req #1: INVITE server tranasaction passes final 4xx message
    NotFoundSipMsg = notfound404(),
    {InvTrans1, SE1} = ersip_trans_inv_server:event({send, NotFoundSipMsg}, InvTrans0),
    {send_response, NotFoundSipMsg} = lists:keyfind(send_response, 1, SE1),

    %% Req #2: Timer G is set for retransmission 4xx:
    {set_timer, {TimeoutG1, {timer, TimerG1} = TimerGEv1}} = lists:keyfind(set_timer, 1, SE1),
    ?assertEqual(timer_g, TimerG1),
    ?assertEqual(500, TimeoutG1),

    %% Req #3: Timer H is set when Completed state is entered:
    %% {set_timer, {TimeoutH, {timer, TimerH} = TimerHEv}} = lists:keyfind(set_timer, 1, SE1),
    %% ?assertEqual(32000, TimeoutH),
    %% ?assertEqual(timer_h, TimerH),

    %% Req #5: Timer G timeout is doubled but limited with T4 each time:
    {_, InvTrans2} =
        lists:foldl(fun(Timeout, {TimerGEv, Trans}) ->
                            {NewTrans, SE} = ersip_trans_inv_server:event(TimerGEv, Trans),
                            {set_timer, {TimeoutG, {timer, TimerG} = TimerGEv}} = lists:keyfind(set_timer, 1, SE),
                            {send_response, NotFoundSipMsg} = lists:keyfind(send_response, 1, SE),
                            ?assertEqual(timer_g, TimerG),
                            ?assertEqual(Timeout, TimeoutG),
                            {TimerGEv, NewTrans}
                    end,
                    {TimerGEv1, InvTrans1},
                    [1000, 2000, 4000, 4000]),

    %% Req #6: Response returned on request retransmission.
    {InvTrans3, SE3} = ersip_trans_inv_server:event({received, InviteSipMsg}, InvTrans2),
    ?assertEqual({send_response, NotFoundSipMsg}, lists:keyfind(send_response, 1, SE3)),

    ACKSipMsg = ack(),
    {InvTrans4, SE4} = ersip_trans_inv_server:event({received, ACKSipMsg}, InvTrans3),
    %% Req #7: ACK is not passed to TU:
    ?assertEqual(false, lists:keyfind(tu_result, 1, SE4)),

    %% Req #8: Timer I is set after ACK (Confirmed state)
    {set_timer, {TimeoutI, {timer, TimerI} = TimerIEv}} = lists:keyfind(set_timer, 1, SE4),
    ?assertEqual(5000, TimeoutI),
    ?assertEqual(timer_i, TimerI),

    %% Req #8: Transaction is cleared after timer I is fired.
    {_InvTrans5, SE5} = ersip_trans_inv_server:event(TimerIEv, InvTrans4),
    ?assertEqual({clear_trans, normal}, lists:keyfind(clear_trans, 1, SE5)),

    ok.

error_handling_test() ->
    %% Invite transaction must be intialized with request.
    ?assertError({api_error, _}, ersip_trans_inv_server:new(reliable, ok200(), #{})),
    %% Invite transaction must be replied with reply
    {InvTrans, _} = ersip_trans_inv_server:new(reliable, invite(), #{}),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, invite()}, InvTrans)),
    %% Response cannot match server transaction so receive of response is error:
    ?assertError({api_error, _}, ersip_trans_inv_server:event({received, ok200()}, InvTrans)),

    %% After transaction is replied we cannot reply with different status code:
    {InvTrans2, _} = ersip_trans_inv_server:event({send, ok200()}, InvTrans),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, notfound404()}, InvTrans2)),

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

notfound404() ->
    parse_message(notfound404_bin()).

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

notfound404_bin() ->
    <<"SIP/2.0 404 Not Found" ?crlf
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
