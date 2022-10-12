%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Client INVITE transaction test
%%

-module(ersip_trans_inv_client_test).


-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-dialyzer({nowarn_function, reliable_transport_flow_test/0}).
reliable_transport_flow_test() ->
    T1 = 250,

    InviteReq = invite_req(),
    {CallingTrans, SE} = ersip_trans_inv_client:new(reliable, InviteReq, #{sip_t1 => T1}),

    %% The client transaction MUST pass the request to the transport
    %% layer for transmission
    ?assertEqual({send_request, InviteReq}, se_event(send_request, SE)),

    %% For any transport, the client transaction MUST start timer B
    %% with a value of 64*T1 seconds (Timer B controls transaction
    %% timeouts).
    TimerBEv = {timer, timer_b},
    TimerB   = ersip_trans_se:set_timer(64*T1, TimerBEv),
    ?assertEqual(TimerB, se_event(set_timer, SE)),

    %% If the client transaction is still in the "Calling" state when
    %% timer B fires, the client transaction SHOULD inform the TU that
    %% a timeout has occurred.  The client transaction MUST NOT
    %% generate an ACK.
    {_Terminated, SE1} = ersip_trans_inv_client:event(TimerBEv, CallingTrans),
    TimeoutClear = ersip_trans_se:clear_trans(timeout),
    ?assertEqual(TimeoutClear, se_event(clear_trans, SE1)),
    ?assertEqual(not_found,    se_event(send_request, SE1)),

    %% If the client transaction receives a provisional response while in
    %% the "Calling" state, it transitions to the "Proceeding" state.
    Trying = trying(),
    {ProceedingTrans, SE2} = ersip_trans_inv_client:event({received, Trying}, CallingTrans),
    %% Furthermore, the provisional response MUST be passed to the TU.
    ?assertEqual({tu_result, Trying}, se_event(tu_result, SE2)),

    %% Any further provisional responses MUST be passed up to the TU
    %% while in the "Proceeding" state.
    Ringing = ringing(),
    {ProceedingTrans, SE3} = ersip_trans_inv_client:event({received, Ringing}, ProceedingTrans),
    ?assertEqual({tu_result, Ringing}, se_event(tu_result, SE3)),

    %% When in either the "Calling" or "Proceeding" states, reception
    %% of a response with status code from 300-699 MUST cause the
    %% client transaction to transition to "Completed".
    NotFound = notfound404(),
    {CompletedTrans, SE4} = ersip_trans_inv_client:event({received, NotFound}, CallingTrans),
    {CompletedTrans, SE4} = ersip_trans_inv_client:event({received, NotFound}, ProceedingTrans),

    %% The client transaction MUST pass the received response up to the TU
    ?assertEqual({tu_result, NotFound}, se_event(tu_result, SE4)),

    %% and the client transaction MUST generate an ACK request, even
    %% if the transport is reliable
    {send_request, ACKMessage} = se_event(send_request, SE4),
    check_ack_message(InviteReq, ACKMessage, NotFound),

    %% RFC6026:
    %% When a 2xx response is received while in either the "Calling"
    %% or "Proceeding" states, the client transaction MUST transition
    %% to the "Accepted" state
    Ok200 = ok200(),
    {AcceptedTrans, SE5} = ersip_trans_inv_client:event({received, Ok200}, CallingTrans),
    {AcceptedTrans, SE5} = ersip_trans_inv_client:event({received, Ok200}, ProceedingTrans),

    %% Timer M MUST be started with a value of 64*T1.
    TimerMEv = {timer, timer_m},
    TimerM   = ersip_trans_se:set_timer(64*T1, TimerMEv),
    ?assertEqual(TimerM, se_event(set_timer, SE5)),

    %% The 2xx response MUST be passed up to the TU
    ?assertEqual({tu_result, Ok200}, se_event(tu_result, SE5)),

    %% The client transaction MUST NOT generate an ACK to the 2xx
    %% response -- its handling is delegated to the TU.
    ?assertEqual(not_found, se_event(send_request, SE5)),

    %% Any 2xx responses that match this client transaction and that are
    %% received while in the "Accepted" state MUST be passed up to the
    %% TU.
    {AcceptedTrans, SE6} = ersip_trans_inv_client:event({received, Ok200}, AcceptedTrans),
    ?assertEqual({tu_result, Ok200}, se_event(tu_result, SE6)),

    %% The client transaction MUST NOT generate an ACK to the 2xx
    %% response.
    ?assertEqual(not_found, se_event(send_request, SE6)),

    %% If Timer M fires while the client transaction is in the "Accepted"
    %% state, the client transaction MUST move to the "Terminated" state.
    {_TerminatedTrans, SE7} = ersip_trans_inv_client:event(TimerMEv, AcceptedTrans),
    ?assertEqual({clear_trans, normal}, se_event(clear_trans, SE7)),

    %% Timer B is ignored in Proceeding/Completed/Accepted states:
    ?assertEqual({ProceedingTrans, []}, ersip_trans_inv_client:event(TimerBEv, ProceedingTrans)),
    ?assertEqual({CompletedTrans, []}, ersip_trans_inv_client:event(TimerBEv, CompletedTrans)),
    ?assertEqual({AcceptedTrans, []}, ersip_trans_inv_client:event(TimerBEv, AcceptedTrans)),
    ok.

unreliable_transport_flow_test() ->
    T1 = 50,
    InviteReq = invite_req(),
    {CallingTrans0, SE} = ersip_trans_inv_client:new(unreliable, InviteReq, #{sip_t1 => T1}),

    %% If an unreliable transport is being used, the client
    %% transaction MUST start timer A with a value of T1.
    TimerAEv = {timer, timer_a},
    TimerA0  = ersip_trans_se:set_timer(T1, TimerAEv),
    ?assertEqual(TimerA0, se_event(set_timer, SE)),

    %% When timer A fires, the client transaction MUST retransmit the
    %% request by passing it to the transport layer, and MUST reset the
    %% timer with a value of 2*T1.
    TimerA1 = ersip_trans_se:set_timer(2*T1, TimerAEv),
    {CallingTrans1, SE1} = ersip_trans_inv_client:event(TimerAEv, CallingTrans0),
    ?assertEqual(TimerA1, se_event(set_timer, SE1)),
    ?assertEqual({send_request, InviteReq}, se_event(send_request, SE1)),

    %% When timer A fires 2*T1 seconds later, the request MUST be
    %% retransmitted again (assuming the client transaction is still
    %% in this state).  This process MUST continue so that the request
    %% is retransmitted with intervals that double after each
    %% transmission.
    _ = lists:foldl(fun(ExpectedTimeout, Trans) ->
                        TimerA = ersip_trans_se:set_timer(ExpectedTimeout, TimerAEv),
                        {Trans1, FSE} = ersip_trans_inv_client:event(TimerAEv, Trans),
                        ?assertEqual(TimerA, se_event(set_timer, FSE)),
                        ?assertEqual({send_request, InviteReq}, se_event(send_request, FSE)),
                        Trans1
                end,
                CallingTrans1,
                [4*T1, 8*T1, 16*T1, 32*T1]),

    {ProceedingTrans, _} = ersip_trans_inv_client:event({received, trying()}, CallingTrans0),

    %% When in either the "Calling" or "Proceeding" states, reception
    %% of a response with status code from 300-699 MUST cause the
    %% client transaction to transition to "Completed".
    NotFound = notfound404(),
    {CompletedTrans, SE2} = ersip_trans_inv_client:event({received, NotFound}, CallingTrans0),
    {CompletedTrans, SE2} = ersip_trans_inv_client:event({received, NotFound}, ProceedingTrans),

    %% and the client transaction MUST generate an ACK request, even
    %% if the transport is reliable
    {send_request, ACKMessage} = se_event(send_request, SE2),
    check_ack_message(InviteReq, ACKMessage, NotFound),

    %% Any retransmissions of the final response that are received while in
    %% the "Completed" state MUST cause the ACK to be re-passed to the
    %% transport layer for retransmission, but the newly received response
    %% MUST NOT be passed up to the TU.
    {CompletedTrans, SE3} = ersip_trans_inv_client:event({received, NotFound}, CompletedTrans),
    ?assertEqual({send_request, ACKMessage}, se_event(send_request, SE3)),
    ?assertEqual(not_found, se_event(tu_result, SE3)),

    %% The client transaction SHOULD start timer D when it enters the
    %% "Completed" state, with a value of at least 32 seconds for
    %% unreliable transports
    TimerDEv = {timer, timer_d},
    TimerD   = ersip_trans_se:set_timer(32000, TimerDEv),
    ?assertEqual(TimerD, se_event(set_timer, SE2)),

    %% If Timer D fires while the client transaction is in the
    %% "Completed" state, the client transaction MUST move to the
    %% "Terminated" state.
    {TerminatedTrans, SE4} = ersip_trans_inv_client:event(TimerDEv, CompletedTrans),
    ?assertEqual({clear_trans, normal}, se_event(clear_trans, SE4)),

    {AcceptedTrans, _} = ersip_trans_inv_client:event({received, ok200()}, CallingTrans1),

    %% Timer A is ignored in Proceeding/Completed/Accepted states:
    ?assertEqual({ProceedingTrans, []}, ersip_trans_inv_client:event(TimerAEv, ProceedingTrans)),
    ?assertEqual({CompletedTrans, []}, ersip_trans_inv_client:event(TimerAEv, CompletedTrans)),
    ?assertEqual({AcceptedTrans, []}, ersip_trans_inv_client:event(TimerAEv, AcceptedTrans)),

    %% Timer B is ignored in Proceeding/Completed/Accepted states:
    TimerBEv = {timer, timer_b},
    ?assertEqual({ProceedingTrans, []}, ersip_trans_inv_client:event(TimerBEv, ProceedingTrans)),
    ?assertEqual({CompletedTrans, []}, ersip_trans_inv_client:event(TimerBEv, CompletedTrans)),
    ?assertEqual({AcceptedTrans, []}, ersip_trans_inv_client:event(TimerBEv, AcceptedTrans)),

    %% Spurious provisional responses are ignored in Complted/Accepted states:
    ?assertEqual({CompletedTrans, []}, ersip_trans_inv_client:event({received, ringing()}, CompletedTrans)),
    ?assertEqual({AcceptedTrans, []}, ersip_trans_inv_client:event({received, ringing()}, AcceptedTrans)),


    %% Check has_final_response for different states:
    ?assertEqual(false, ersip_trans_inv_client:has_final_response(CallingTrans1)),
    ?assertEqual(false, ersip_trans_inv_client:has_final_response(ProceedingTrans)),
    ?assertEqual(true, ersip_trans_inv_client:has_final_response(CompletedTrans)),
    ?assertEqual(true, ersip_trans_inv_client:has_final_response(AcceptedTrans)),
    ?assertEqual(true, ersip_trans_inv_client:has_final_response(TerminatedTrans)),
    ok.

final_response_in_accepted_state_test() ->
    InviteReq = invite_req(),
    {CallingTrans, _} = ersip_trans_inv_client:new(reliable, InviteReq, #{}),
    Ok200 = ok200(),
    {AcceptedTrans, AcceptedSE1} = ersip_trans_inv_client:event({received, ok200()}, CallingTrans),
    ?assertEqual({tu_result, Ok200}, se_event(tu_result, AcceptedSE1)),
    {AcceptedTrans, AcceptedSE2} = ersip_trans_inv_client:event({received, ok200()}, AcceptedTrans),
    %% RFC6026:
    %% 2xx response received while in the "Accepted" state MUST be
    %% passed to the TU and the machine remains in the "Accepted"
    %% state.
    ?assertEqual({tu_result, Ok200}, se_event(tu_result, AcceptedSE2)),
    %% The client transaction MUST NOT generate an ACK to any 2xx
    %% response on its own.  The TU responsible for the transaction
    %% will generate the ACK.
    ?assertEqual(not_found, se_event(send_request, AcceptedSE2)),
    %% Do not pass 4xx to TU
    {AcceptedTrans, AcceptedSE3} = ersip_trans_inv_client:event({received, notfound404()}, AcceptedTrans),
    ?assertEqual(not_found, se_event(tu_result, AcceptedSE3)),
    ok.

-dialyzer({nowarn_function, error_handling_test/0}).
error_handling_test() ->
    %% Invite transaction must be intialized with request.
    ?assertError({api_error, _}, ersip_trans_inv_client:new(reliable, ok200_req(), #{})),

    InviteReq = invite_req(),
    {CallingTrans, _} = ersip_trans_inv_client:new(unreliable, InviteReq, #{}),

    %% Request cannot match client transaction:
    ?assertError({api_error, _}, ersip_trans_inv_client:event({received, invite()}, CallingTrans)),

    %% Cannot send second request via client transaction:
    ?assertError({api_error, _}, ersip_trans_inv_client:event({send, invite()}, CallingTrans)),
    ok.


trans_expire_set_test() ->
    TransExpire = 31713,
    InviteReq = invite_req(),
    {_ClientTrans0, SideEffects0} = ersip_trans_inv_client:new(unreliable, InviteReq, #{trans_expire => TransExpire}),
    _SideEffectsMap0 = maps:from_list(SideEffects0),
    %% Transaction timer is set:
    [{500,   _RetransmitTimer},
     {TransExpire, _TransactionTimer}
    ] = lists:sort(proplists:get_all_values(set_timer, SideEffects0)),

    {_ClientTrans1, SideEffects1} = ersip_trans_client:new(reliable, InviteReq, #{trans_expire => TransExpire}),
    _SideEffectsMap1 = maps:from_list(SideEffects1),
    %% Transaction timer is set:
    [{TransExpire, _TransactionTimer1}] = lists:sort(proplists:get_all_values(set_timer, SideEffects1)),
    ok.

to_map_test() ->
   {Trans0, _} = ersip_trans_inv_client:new(unreliable, invite_req(), #{}),
   Expected0 = #{state => 'Calling',
                 transport => unreliable,
                 options => #{sip_t1 => 500},
                 request => element(5, Trans0),
                 clear_reason => normal,
                 timer_a_timeout => 500,
                 last_ack => undefined
                },
   ?assertEqual(Expected0, ersip_trans_inv_client:to_map(Trans0)),
   ?assertEqual(Trans0, ersip_trans_inv_client:from_map(Expected0)),

   {Trans1, _} = ersip_trans_inv_client:event({received, trying()}, Trans0),
   Expected1 = #{state => 'Proceeding',
                 transport => unreliable,
                 options => #{sip_t1 => 500},
                 request => element(5, Trans1),
                 clear_reason => normal,
                 timer_a_timeout => 500,
                 last_ack => undefined
                },
   ?assertEqual(Expected1, ersip_trans_inv_client:to_map(Trans1)),
   ?assertEqual(Trans1, ersip_trans_inv_client:from_map(Expected1)),

    {Trans2, _} = ersip_trans_inv_client:event({received, ok200()}, Trans1),
    Expected2 = #{state => 'Accepted',
                  transport => unreliable,
                  options => #{sip_t1 => 500},
                  request => element(5, Trans2),
                  clear_reason => normal,
                  timer_a_timeout => 500,
                  last_ack => undefined
                 },
    ?assertEqual(Expected2, ersip_trans_inv_client:to_map(Trans2)),
    ?assertEqual(Trans2, ersip_trans_inv_client:from_map(Expected2)),

    {Trans3, _} =  ersip_trans_inv_client:event({timer, timer_m}, Trans2),
    Expected3 = #{state => 'Terminated',
                  transport => unreliable,
                  options => #{sip_t1 => 500},
                  request => element(5, Trans3),
                  clear_reason => normal,
                  timer_a_timeout => 500,
                  last_ack => undefined
                 },
    ?assertEqual(Expected3, ersip_trans_inv_client:to_map(Trans3)),
    ?assertEqual(Trans3, ersip_trans_inv_client:from_map(Expected3)).

%%%===================================================================
%%% Helpers
%%%===================================================================

invite_req() ->
    SipMsg = invite(),
    Branch = ersip_branch:make(<<"inviteTrans">>),
    Nexthop = ersip_uri:make(<<"sip:biloxi.com">>),
    ersip_request:new(SipMsg, Branch, Nexthop).

ok200_req() ->
    SipMsg = ok200(),
    Branch = ersip_branch:make(<<"inviteTrans">>),
    Nexthop = ersip_uri:make(<<"sip:biloxi.com">>),
    ersip_request:new(SipMsg, Branch, Nexthop).

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

trying() ->
    parse_message(trying_bin()).

ok200() ->
    parse_message(ok200_bin()).

notfound404() ->
    parse_message(notfound404_bin()).


check_ack_message(INVITEReq, ACKReq, ResponseSipMsg) ->
    ACKSipMsg    = ersip_request:sipmsg(ACKReq),
    INVITESipMsg = ersip_request:sipmsg(INVITEReq),
    %% 17.1.1.3

    %% The ACK request constructed by the client transaction MUST
    %% contain values for the Call-ID, From, and Request-URI that are
    %% equal to the values of those header fields in the request
    %% passed to the transport by the client transaction (call this
    %% the "original request").
    check_equal(callid, INVITESipMsg, ACKSipMsg),
    check_equal(from,   INVITESipMsg, ACKSipMsg),
    ?assertEqual(ersip_sipmsg:ruri(INVITESipMsg), ersip_sipmsg:ruri(ACKSipMsg)),

    %% The To header field in the ACK MUST equal the To header field
    %% in the response being acknowledged,
    check_equal(to,     ResponseSipMsg, ACKSipMsg),

    %% The ACK MUST contain a single Via header field, and this MUST
    %% be equal to the top Via header field of the original request.
    ?assertEqual(ersip_request:branch(INVITEReq), ersip_request:branch(ACKReq)),
    ?assertEqual(ersip_request:nexthop(INVITEReq), ersip_request:nexthop(ACKReq)),

    %% The CSeq header field in the ACK MUST contain the same value
    %% for the sequence number as was present in the original request,
    %% but the method parameter MUST be equal to "ACK".
    INVITECSeq = ersip_sipmsg:get(cseq, INVITESipMsg),
    ACKCSeq    = ersip_sipmsg:get(cseq, ACKSipMsg),
    ?assertEqual(ersip_hdr_cseq:number(INVITECSeq), ersip_hdr_cseq:number(ACKCSeq)),
    ?assertEqual(ersip_method:ack(), ersip_hdr_cseq:method(ACKCSeq)),

    %% If the INVITE request whose response is being acknowledged had
    %% Route header fields, those header fields MUST appear in the
    %% ACK.
    check_equal(route, INVITESipMsg, ACKSipMsg),

    %% Not fully specified in RFC 3261. Max-Forward. We copy it from
    %% original request as presented in example in 17.1.1.3.
    check_equal(maxforwards, INVITESipMsg, ACKSipMsg),
    ok.

check_equal(Hdr, SipMsg1, SipMsg2) ->
    H1 = ersip_sipmsg:get(Hdr, SipMsg1),
    H2 = ersip_sipmsg:get(Hdr, SipMsg2),
    ?assertEqual(H1, H2).


-define(crlf, "\r\n").

invite_bin() ->
    <<"INVITE sip:bob@biloxi.com SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8" ?crlf
      "Max-Forwards: 70" ?crlf
      "To: Bob <sip:bob@biloxi.com>" ?crlf
      "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
      "Call-ID: a84b4c76e66710" ?crlf
      "Route: <sip:server10.biloxi.com;lr>," ?crlf %% Need route here to verify ACK generation
      " <sip:bigbox3.site3.atlanta.com;lr>" ?crlf
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

trying_bin() ->
    <<"SIP/2.0 100 Trying" ?crlf
      "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8" ?crlf
      " ;received=192.0.2.1" ?crlf
      "To: Bob <sip:bob@biloxi.com>" ?crlf
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

parse_message(Bin) when is_binary(Bin) ->
    P  = ersip_parser:new_dgram(Bin),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg, all),
    SipMsg.

