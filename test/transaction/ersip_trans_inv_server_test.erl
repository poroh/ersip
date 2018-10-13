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
    T1 = 70,
    InviteSipMsg = invite(),
    {ProceedingTrans0, SE} = ersip_trans_inv_server:new(reliable, InviteSipMsg, #{sip_t1 => T1}),

    %% Req #1: INVITE must be passed to TU when entering Proceeding state.
    ?assertEqual({tu_result, InviteSipMsg}, se_event(tu_result, SE)),

    %% Req #2: INVITE server transaction must generate 100 Trying response
    {send_response, Resp100} = se_event(send_response, SE),
    ?assertEqual(response, ersip_sipmsg:type(Resp100)),
    ?assertEqual(100, ersip_sipmsg:status(Resp100)),

    %% Req #3: INVITE server tranasaction passes provisional response
    RingingSipMsg = ringing(),
    {ProceedingTrans1, SE1} = ersip_trans_inv_server:event({send, RingingSipMsg}, ProceedingTrans0),
    {send_response, RingingSipMsg} = se_event(send_response, SE1),

    %% Req #4: INVITE server tranasaction passes final 200 OK message
    OKSipMsg = ok200(),
    {AcceptedTrans, SE2} = ersip_trans_inv_server:event({send, OKSipMsg}, ProceedingTrans0), %% Without ringing
    {AcceptedTrans, SE2} = ersip_trans_inv_server:event({send, OKSipMsg}, ProceedingTrans1), %% With ringing
    {send_response, OKSipMsg} = se_event(send_response, SE2),

    %% Req #5: Timer L is set after passing 200 OK message (RFC6026 Accepted state):
    TimerLEv = {timer, timer_l},
    TimerL   = ersip_trans_se:set_timer(64*T1, TimerLEv),
    ?assertEqual(TimerL, se_event(set_timer, SE2)),

    %% Req #6: Retransmission of 2xx are passed to transport layer (RFC6026):
    {AcceptedTrans, SE3} = ersip_trans_inv_server:event({send, OKSipMsg}, AcceptedTrans),
    {send_response, OKSipMsg} = se_event(send_response, SE3),

    %% Req #7: ACKs matching transaction are passed to TU (RFC6026):
    ACKSipMsg = ack(),
    {AcceptedTrans, SE4} = ersip_trans_inv_server:event({received, ACKSipMsg}, AcceptedTrans),
    ?assertEqual({tu_result, ACKSipMsg}, se_event(tu_result, SE4)),

    %% Req #8: INVITE server tranasaction is stopped after timer L is fired
    {_TerminatedTrans, SE5} = ersip_trans_inv_server:event(TimerLEv, AcceptedTrans),
    ?assertEqual([{clear_trans, normal}], SE5),
    ok.


reliable_canonnical_4xx_flow_test() ->
    T1 = 70,
    InviteSipMsg = invite(),
    {ProceedingTrans, _} = ersip_trans_inv_server:new(reliable, InviteSipMsg, #{sip_t1 => T1}),

    %% Req #1: INVITE server tranasaction passes final 4xx message
    NotFoundSipMsg = notfound404(),
    {CompletedTrans, SE1} = ersip_trans_inv_server:event({send, NotFoundSipMsg}, ProceedingTrans),
    {send_response, NotFoundSipMsg} = lists:keyfind(send_response, 1, SE1),

    %% Req #2: Timer H is set when Completed state is entered:
    TimerHEv = {timer, timer_h},
    TimerH   = ersip_trans_se:set_timer(64*T1, TimerHEv),
    ?assertEqual(TimerH, se_event(set_timer, SE1)),

    %% Req #3: ACKs matching transaction are absorbed by transaction:
    ACKSipMsg = ack(),
    {_TerminatedTrans1, SE2} = ersip_trans_inv_server:event({received, ACKSipMsg}, CompletedTrans),
    ?assertEqual(not_found, se_event(tu_result, SE2)),

    %% Req #4: Transaction is cleared for reliable transport. (Timer I is set to 0).
    ?assertEqual({clear_trans, normal}, lists:keyfind(clear_trans, 1, SE2)),

    %% Req #5: Transaction is cleared with no_ack if TimerH is fired:
    {_TerminatedTrans2, SE3} = ersip_trans_inv_server:event(TimerHEv, CompletedTrans),
    ?assertEqual({clear_trans, no_ack}, lists:keyfind(clear_trans, 1, SE3)),
    ok.

unreliable_with_retransmits_2xx_test() ->
    T1 = 70,
    InviteSipMsg = invite(),
    {ProceedingTrans, SE} = ersip_trans_inv_server:new(unreliable, InviteSipMsg, #{sip_t1 => T1}),

    %% Req #1: INVITE must be passed to TU when entering Proceeding state.
    ?assertEqual({tu_result, InviteSipMsg}, se_event(tu_result, SE)),

    %% Req #2: INVITE server transaction must generate 100 Trying response
    {send_response, Resp100} = lists:keyfind(send_response, 1, SE),
    ?assertEqual(response, ersip_sipmsg:type(Resp100)),
    ?assertEqual(100, ersip_sipmsg:status(Resp100)),

    %% Req #3: 100 Trying is resent on INVITE retransmit:
    {ProceedingTrans, SE1} = ersip_trans_inv_server:event({received, InviteSipMsg}, ProceedingTrans),
    {send_response, Resp100} = se_event(send_response, SE1),

    %% Req #4: INVITE server tranasaction passes final 200 OK message
    OKSipMsg = ok200(),
    {AcceptedTrans, SE2} = ersip_trans_inv_server:event({send, OKSipMsg}, ProceedingTrans),
    {send_response, OKSipMsg} = se_event(send_response, SE2),

    %% Req #5: Timer L is set to absort INVITE retransmits (RFC6026)
    TimerLEv = {timer, timer_l},
    TimerL   = ersip_trans_se:set_timer(64*T1, TimerLEv),
    ?assertEqual(TimerL, se_event(set_timer, SE2)),

    %% Req #6: INVITE retransmits are ignored in Accepted state.
    {AcceptedTrans, SE3} = ersip_trans_inv_server:event({received, InviteSipMsg}, AcceptedTrans),
    ?assertEqual(not_found, se_event(tu_result, SE3)),

    %% Req #7: INVITE server tranasaction passes retransmissions of
    %% final 200 OK message to Transport layer
    {AcceptedTrans, SE4} = ersip_trans_inv_server:event({send, OKSipMsg}, AcceptedTrans),
    {send_response, OKSipMsg} = se_event(send_response, SE4),

    {AcceptedTrans, SE5} = ersip_trans_inv_server:event({send, OKSipMsg}, AcceptedTrans),
    {send_response, OKSipMsg} = se_event(send_response, SE5),

    %% Req #8: ACKs are passed to TU in Accepted state
    ACKSipMsg = ack(),
    {AcceptedTrans, SE6} = ersip_trans_inv_server:event({received, ACKSipMsg}, AcceptedTrans),
    ?assertEqual({tu_result, ACKSipMsg}, se_event(tu_result, SE6)),

    {AcceptedTrans, SE7} = ersip_trans_inv_server:event({received, ACKSipMsg}, AcceptedTrans),
    ?assertEqual({tu_result, ACKSipMsg}, se_event(tu_result, SE7)),

    %% Req #9: Transaction is cleared if timer L is fired in Accepted state.
    {_, SE8} = ersip_trans_inv_server:event(TimerLEv, AcceptedTrans),
    ?assertEqual({clear_trans, normal}, se_event(clear_trans, SE8)),
    ok.

unreliable_with_retransmits_4xx_test() ->
    T1 = 70,
    T2 = 1000,
    T4 = 4444,
    InviteSipMsg = invite(),
    {ProceedingTrans, _} = ersip_trans_inv_server:new(unreliable, InviteSipMsg, #{sip_t1 => T1,
                                                                                  sip_t2 => T2,
                                                                                  sip_t4 => T4}),

    %% Req #1: INVITE server tranasaction passes final 4xx message
    NotFoundSipMsg = notfound404(),
    {CompletedTrans, SE1} = ersip_trans_inv_server:event({send, NotFoundSipMsg}, ProceedingTrans),
    {send_response, NotFoundSipMsg} = lists:keyfind(send_response, 1, SE1),

    %% Req #2: Timer G is set for retransmission 4xx:
    TimerGEv  = {timer, timer_g},
    TimerG1   = ersip_trans_se:set_timer(T1, TimerGEv),
    {value, TimerG1, SE1_1} = lists:keytake(set_timer, 1, SE1),

    %% Req #3: Timer H is set when Completed state is entered:
    TimerHEv  = {timer, timer_h},
    TimerH    = ersip_trans_se:set_timer(64*T1, TimerHEv),
    ?assertEqual(TimerH, se_event(set_timer, SE1_1)),

    %% Req #5: Timer G timeout is doubled but limited with T4 each time:
    CompletedTrans1 =
        lists:foldl(fun(Timeout, Trans) ->
                            {NewTrans, SE} = ersip_trans_inv_server:event(TimerGEv, Trans),
                            TimerGx = ersip_trans_se:set_timer(Timeout, TimerGEv),
                            ?assertEqual(TimerGx, se_event(set_timer, SE)),
                            ?assertEqual({send_response, NotFoundSipMsg}, se_event(send_response, SE)),
                            NewTrans
                    end,
                    CompletedTrans,
                    [min(2*T1, T2),
                     min(4*T1, T2),
                     min(8*T1, T2),
                     min(16*T1, T2),
                     min(32*T1, T2)]),

    %% Req #6: Response returned on request retransmission.
    {CompletedTrans1, SE3} = ersip_trans_inv_server:event({received, InviteSipMsg}, CompletedTrans1),
    ?assertEqual({send_response, NotFoundSipMsg}, lists:keyfind(send_response, 1, SE3)),


    ACKSipMsg = ack(),
    {ConfirmedTrans, SE4} = ersip_trans_inv_server:event({received, ACKSipMsg}, CompletedTrans1),
    %% Req #7: ACK is not passed to TU:
    ?assertEqual(not_found, se_event(tu_result, SE4)),

    %% Req #8: Timer I is set after ACK (Confirmed state)
    TimerIEv  = {timer, timer_i},
    TimerI    = ersip_trans_se:set_timer(T4, TimerIEv),
    ?assertEqual(TimerI, se_event(set_timer, SE4)),

    %% Req #8: Transaction is cleared after timer I is fired.
    {_TerminatedTrans1, SE5} = ersip_trans_inv_server:event(TimerIEv, ConfirmedTrans),
    ?assertEqual({clear_trans, normal}, lists:keyfind(clear_trans, 1, SE5)),

    %% Req #9: Retransmission & acks are ignored in Confirmed state:
    {ConfirmedTrans, SE6} = ersip_trans_inv_server:event({received, InviteSipMsg}, ConfirmedTrans),
    ?assertEqual([], SE6),

    {ConfirmedTrans, SE7} = ersip_trans_inv_server:event({received, ACKSipMsg}, ConfirmedTrans),
    ?assertEqual([], SE7),

    %% Req #10: Timer G is ignored in Confirmed state:
    {ConfirmedTrans, SE8} = ersip_trans_inv_server:event(TimerGEv, ConfirmedTrans),
    ?assertEqual([], SE8),

    %% Req #11: Transaction is cleared with no_ack if TimerH is fired:
    {_TerminatedTrans2, SE9} = ersip_trans_inv_server:event(TimerHEv, CompletedTrans1),
    ?assertEqual({clear_trans, no_ack}, se_event(clear_trans, SE9)),
    ok.

ignore_ack_in_proceeding_test() ->
    {InvTransProceeding, _} = ersip_trans_inv_server:new(unreliable, invite(), #{}),
    {InvTrans1, SE1} = ersip_trans_inv_server:event({received, ack()}, InvTransProceeding),
    ?assertEqual(InvTransProceeding, InvTrans1),
    ?assertEqual([], SE1),
    ok.

error_handling_test() ->
    %% Invite transaction must be intialized with request.
    ?assertError({api_error, _}, ersip_trans_inv_server:new(reliable, ok200(), #{})),
    %% Invite transaction must be replied with reply
    {InvTrans, _} = ersip_trans_inv_server:new(unreliable, invite(), #{}),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, invite()}, InvTrans)),
    %% Response cannot match server transaction so receive of response is error:
    ?assertError({api_error, _}, ersip_trans_inv_server:event({received, ok200()}, InvTrans)),

    %% After transaction is replied we cannot reply with different status code:
    {InvTransAccepted, _} = ersip_trans_inv_server:event({send, ok200()}, InvTrans),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, notfound404()}, InvTransAccepted)),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, ringing()}, InvTransAccepted)),

    %% After transaction is replied we cannot reply with different status code:
    {InvTransCompleted, _} = ersip_trans_inv_server:event({send, notfound404()}, InvTrans),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, ringing()}, InvTransCompleted)),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, ok200()}, InvTransCompleted)),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, notfound404()}, InvTransCompleted)),

    {InvTransConfirmed, _} = ersip_trans_inv_server:event({received, ack()}, InvTransCompleted),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, ringing()}, InvTransConfirmed)),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, ok200()}, InvTransConfirmed)),
    ?assertError({api_error, _}, ersip_trans_inv_server:event({send, notfound404()}, InvTransConfirmed)),

    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

se_event(Type, SE) ->
    case lists:keyfind(Type, 1, SE) of
        false ->
            not_found;
        X ->
            X
    end.

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
