%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common SIP transaction interface tests
%%

-module(ersip_trans_test).

-include_lib("eunit/include/eunit.hrl").

-define(crlf, "\r\n").

%%%===================================================================
%%% Cases
%%%===================================================================

server_transaction_new_test() ->
    SipMsg = default_register_request(),
    {ServerTrans, SE} = ersip_trans:new_server(SipMsg, default_sip_options()),
    ?assertEqual(ersip_trans:id(ServerTrans), ersip_trans_id:make_server(SipMsg)),
    ?assertEqual([ersip_trans_se:tu_result(SipMsg)], SE),
    ok.

cancel_transaction_id_test() ->
    InviteReq = invite_req(),
    InviteMsg = pass_request_to_server(InviteReq),
    CancelReq = ersip_request_cancel:generate(InviteReq),
    CancelMsg = pass_request_to_server(CancelReq),
    %% Check that message sent to server can be matched by CANCEL request
    ?assertEqual(ersip_trans_id:make_server(InviteMsg), ersip_trans:server_cancel_id(CancelMsg)),
    ok.

server_transaction_retransmit_test() ->
    SipMsg = default_register_request(),
    {ServerTrans, SE} = ersip_trans:new_server(SipMsg, default_sip_options()),
    ?assertEqual({tu_result, SipMsg}, lists:keyfind(tu_result, 1, SE)),
    ProvResp = ersip_sipmsg:reply(100, SipMsg),
    {ServerTrans1, SE1} = ersip_trans:event({send, ProvResp}, ServerTrans),
    ?assertEqual({send_response, ProvResp}, lists:keyfind(send_response, 1, SE1)),
    %% Received retransmit:
    {ServerTrans2, SE2} = ersip_trans:event({received, SipMsg}, ServerTrans1),
    ?assertEqual({send_response, ProvResp}, lists:keyfind(send_response, 1, SE2)),
    %% Send respons on the transaction:
    FinalResp = ersip_sipmsg:reply(200, SipMsg),
    {ServerTrans3, SE3} = ersip_trans:event({send, FinalResp}, ServerTrans2),
    ?assertEqual({send_response, FinalResp}, lists:keyfind(send_response, 1, SE3)),
    %% Received retransmit after final resp:
    {_ServerTrans4, SE4} = ersip_trans:event({received, SipMsg}, ServerTrans3),
    %% Final response is retransmitted:
    ?assertEqual({send_response, FinalResp}, lists:keyfind(send_response, 1, SE4)),
    ok.

server_transaction_invalid_api_test() ->
    SipMsg = default_register_request(),
    {ServerTrans, _} = ersip_trans:new_server(SipMsg, default_sip_options()),
    ProvResp = ersip_sipmsg:reply(100, SipMsg),
    %% Check: cannot receive response in server transaction (responses
    %% cannot match any server transaction).
    ?assertError({api_error, _}, ersip_trans:event({received, ProvResp}, ServerTrans)),
    %% Check: cannot send request in server transaction (transaction
    %% user reply on request).
    ?assertError({api_error, _}, ersip_trans:event({send, SipMsg}, ServerTrans)),
    ok.

client_transaction_new_test() ->
    SipMsg = default_register_request(),
    Branch = ersip_branch:make_random(7),
    Method = ersip_sipmsg:method(SipMsg),
    OutReq = ersip_request:new(SipMsg, Branch, default_nexthop()),
    {ClientTrans, SE} = ersip_trans:new_client(OutReq, default_sip_options()),
    ?assertEqual({ersip_branch:make_key(Branch), Method}, ersip_trans:id(ClientTrans)),
    ?assertEqual(ersip_trans_se:send_request(OutReq), lists:keyfind(send_request, 1, SE)),
    ok.

client_transaction_complete_test() ->
    SipMsg = default_register_request(),
    Branch = ersip_branch:make_random(7),
    OutReq = ersip_request:new(SipMsg, Branch, default_nexthop()),
    {ClientTrans, _SE} = ersip_trans:new_client(OutReq, default_sip_options()),
    %% Get remote message:
    RemoteMsg = send_req_via_default_conn(OutReq),
    %% Make response on remote message:
    Remote100Trying = ersip_sipmsg:reply(100, RemoteMsg),
    {ClientTrans1, SE1} = ersip_trans:event({received, Remote100Trying}, ClientTrans),
    ?assertEqual({tu_result, Remote100Trying}, lists:keyfind(tu_result, 1, SE1)),
    Remote200OK = ersip_sipmsg:reply(200, RemoteMsg),
    {ClientTrans2, SE2} = ersip_trans:event({received, Remote200OK}, ClientTrans1),
    ?assertEqual({tu_result, Remote200OK}, lists:keyfind(tu_result, 1, SE2)),
    %% Ignore retransmits of response:
    {_ClientTrans3, SE3} = ersip_trans:event({received, Remote200OK}, ClientTrans2),
    ?assertEqual(false, lists:keyfind(tu_result, 1, SE3)),
    ok.

client_transaction_invalid_api_test() ->
    SipMsg = default_register_request(),
    Branch = ersip_branch:make_random(7),
    OutReq = ersip_request:new(SipMsg, Branch, default_nexthop()),
    {ClientTrans, _SE} = ersip_trans:new_client(OutReq, default_sip_options()),
    %% Get remote message:
    RemoteMsg = send_req_via_default_conn(OutReq),
    ?assertError({api_error, _}, ersip_trans:event({received, RemoteMsg}, ClientTrans)),
    ok.

invite_transaction_test() ->
    InviteReq = invite_req(),
    {ClientCalling, SE1} = ersip_trans:new_client(InviteReq, default_sip_options()),
    ?assertEqual({send_request, InviteReq}, se_event(send_request, SE1)),
    InviteSipMsg = pass_request_to_server(InviteReq),

    {ServerProceeding, SE2} = ersip_trans:new_server(InviteSipMsg, default_sip_options()),

    %% Sending 100 Trying to caller
    {send_response, SrvTryingSipMsg} = se_event(send_response, SE2),
    TryingSipMsg = pass_response_to_client(SrvTryingSipMsg),
    {ClientProceeding, SE3} = ersip_trans:event({received, TryingSipMsg}, ClientCalling),
    ?assertEqual({tu_result, TryingSipMsg}, se_event(tu_result, SE3)),

    %% Sending 180 Ringing to caller
    {ServerProceeding1, SE4} = ersip_trans:event({send, ringing()}, ServerProceeding),
    {send_response, SrvRingingSipMsg} = se_event(send_response, SE4),
    RingingSipMsg = pass_response_to_client(SrvRingingSipMsg),

    {ClientProceeding, SE5} = ersip_trans:event({received, RingingSipMsg}, ClientProceeding),
    {tu_result, ClientRinging} = se_event(tu_result, SE5),
    ?assertEqual(ersip_sipmsg:serialize_bin(SrvRingingSipMsg), ersip_sipmsg:serialize_bin(ClientRinging)),

    %% Sending 200 Ringing to caller
    {ServerAccepted, SE6} = ersip_trans:event({send, ok200()}, ServerProceeding1),
    {send_response, SrvOKSipMsg} = se_event(send_response, SE6),
    OKSipMsg = pass_response_to_client(SrvOKSipMsg),

    {_ClientAccepted, SE7} = ersip_trans:event({received, OKSipMsg}, ClientProceeding),
    {tu_result, ClientOK} = se_event(tu_result, SE7),
    ?assertEqual(ersip_sipmsg:serialize_bin(SrvOKSipMsg), ersip_sipmsg:serialize_bin(ClientOK)),

    %% Send ACK from client side:
    ACKReq = ack_req(),
    ACKSipMsg = pass_request_to_server(ACKReq),

    {ServerAccepted, SE9} = ersip_trans:event({received, ACKSipMsg}, ServerAccepted),
    {tu_result, SrvACKSipMsg} = se_event(tu_result, SE9),
    ?assertEqual(ersip_sipmsg:serialize_bin(SrvACKSipMsg), ersip_sipmsg:serialize_bin(ACKSipMsg)),
    ok.

error_on_ack_test() ->
    %% Check that API forbids creating client/server transaction for
    %% ACK requests.
    ?assertError({api_error, _}, ersip_trans:new_client(ack_req(), default_sip_options())),
    ?assertError({api_error, _}, ersip_trans:new_server(ack(), default_sip_options())),
    ok.

client_transaction_match_test() ->
    SipMsg = default_register_request(),
    Branch = ersip_branch:make_random(7),
    OutReq = ersip_request:new(SipMsg, Branch, default_nexthop()),
    {_ClientTrans, SE} = ersip_trans:new_client(OutReq, default_sip_options()),
    {send_request, SendReq} = se_event(send_request, SE),
    RemoteSipMsg = send_req_via_default_conn(SendReq),
    Remote200OK = ersip_sipmsg:reply(200, RemoteSipMsg),
    {Via, Local200OK} = recv_response_via_default_conn(Remote200OK),
    ?assertEqual(ersip_trans:client_id(OutReq), ersip_trans:client_id(Via, Local200OK)),
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

create_sipmsg(Msg, Source) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg1 = ersip_msg:set_source(Source, PMsg),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg1, all),
    SipMsg.

pass_request_to_server(Request) ->
    ReqSipMsg = send_req_via_default_conn(Request),
    Bin = ersip_sipmsg:serialize_bin(ReqSipMsg),
    create_sipmsg(Bin, make_default_source()).

pass_response_to_client(RespSipMsg) ->
    Bin = ersip_sipmsg:serialize_bin(RespSipMsg),
    create_sipmsg(Bin, make_default_source()).

make_default_source() ->
    udp_source(default_peer()).

default_peer() ->
    {{127, 0, 0, 1}, 5060}.

default_nexthop() ->
    ersip_uri:make(<<"sip:127.0.0.2">>).

udp_source(Peer) ->
    ersip_source:new(Peer, udp_transport(), undefined).

udp_transport() ->
    ersip_transport:make(udp).

default_sip_options() ->
    #{}.

default_register_request() ->
    Msg = register_request(),
    create_sipmsg(Msg, make_default_source()).

register_request() ->
    <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: 4 REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Expires: 3600" ?crlf
      "Content-Length: 0" ?crlf
      "Contact: <sip:1000@192.168.100.11:5070;line=69210a2e715cee1>" ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      "User-Agent: Linphone/3.6.1 (eXosip2/4.1.0)" ?crlf
      ?crlf>>.

send_req_via_default_conn(OutReq) ->
    RemoteMsg = iolist_to_binary(ersip_request:send_via_conn(OutReq, default_udp_conn())),
    create_sipmsg(RemoteMsg, make_default_source()).

recv_response_via_default_conn(RemoteSipMsg) ->
    RemoteBin = ersip_sipmsg:serialize_bin(RemoteSipMsg),
    Conn = default_udp_conn(),
    {_Conn, [{new_response, Via, Msg}]} = ersip_conn:conn_data(RemoteBin, Conn),
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    {Via, SipMsg}.

default_udp_conn() ->
    ersip_conn:new({127, 0, 0, 1}, 5061, {127, 0, 0, 2}, 5060, udp_transport(), #{}).

invite_req() ->
    SipMsg = invite(),
    Branch = ersip_branch:make(<<"inviteTrans">>),
    Nexthop = ersip_uri:make(<<"sip:biloxi.com">>),
    ersip_request:new(SipMsg, Branch, Nexthop).

ack_req() ->
    SipMsg = ack(),
    Branch = ersip_branch:make(<<"inviteTrans1">>),
    Nexthop = ersip_uri:make(<<"sip:biloxi.com">>),
    ersip_request:new(SipMsg, Branch, Nexthop).

invite() ->
    parse_message(invite_bin()).

ringing() ->
    parse_message(ringing_bin()).

ok200() ->
    parse_message(ok200_bin()).

ack() ->
    parse_message(ack_bin()).

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
