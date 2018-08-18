%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common proxy routinges tests
%%

-module(ersip_proxy_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

basic_proxy_noninvite_test() ->
    MessageSipMsg = message_request(),
    RURI = ersip_sipmsg:ruri(MessageSipMsg),

    %% 1. Create new stateful proxy request state.
    {StateCreateTrans, SECreateTrans} = ersip_proxy:new_stateful(MessageSipMsg, #{}),
    %%    - Check that new server transaction is created.
    ?assertMatch({create_trans, {server, _, MessageSipMsg}}, se_event(create_trans, SECreateTrans)),
    {create_trans, {server, ServerTransId, MessageSipMsg}} = se_event(create_trans, SECreateTrans),
    ?assertEqual(1, length(SECreateTrans)),

    %% 2. Process server transaction result.
    {StateProcessReq, SEProcessReq} = ersip_proxy:trans_result(ServerTransId, MessageSipMsg, StateCreateTrans),
    %%    - Check that RURI is used to for selecting target of the request.
    RURI = ersip_sipmsg:ruri(MessageSipMsg),
    ?assertMatch({select_target, RURI}, se_event(select_target, SEProcessReq)),

    %% Choose some target to forward:
    SingleTarget = ersip_uri:make(<<"sip:contact@192.168.1.1">>),
    {StateSingleTarget, SESingleTarget} = ersip_proxy:forward_to(SingleTarget, StateProcessReq),
    %%    - Check that client transaction is created:
    ?assertMatch({create_trans, {client, _, _}}, se_event(create_trans, SESingleTarget)),
    {create_trans, {client, SingleClientTransId, SingleReq}} = se_event(create_trans, SESingleTarget),
    %%    - Check that message is sent to target:
    ?assertEqual(SingleTarget, ersip_request:nexthop(SingleReq)),

    %% Creating response and pass as client transaction result:
    {_Via, SingleResp} = create_client_trans_result(200, SingleReq),
    {_, SESingleResp} = ersip_proxy:trans_result(SingleClientTransId, SingleResp, StateSingleTarget),
    %%    - Response is passed to request source and proxy is stopped
    ?assertMatch({response, {ServerTransId, _}}, se_event(response, SESingleResp)),
    ?assertMatch({stop, _}, se_event(stop, SESingleResp)),

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

-define(crlf, "\r\n").

message_request() ->
    create_sipmsg(message_request_bin(), make_default_source()).

message_request_bin() ->
    <<"MESSAGE sip:user2@domain.com SIP/2.0" ?crlf
      "Via: SIP/2.0/TCP proxy.domain.com;branch=z9hG4bK123dsghds" ?crlf
      "Via: SIP/2.0/TCP user1pc.domain.com;branch=z9hG4bK776sgdkse;" ?crlf
      "    received=1.2.3.4" ?crlf
      "Max-Forwards: 69" ?crlf
      "From: sip:user1@domain.com;tag=49394" ?crlf
      "To: sip:user2@domain.com" ?crlf
      "Call-ID: asd88asd77a@1.2.3.4" ?crlf
      "CSeq: 1 MESSAGE" ?crlf
      "Content-Type: text/plain" ?crlf
      "Content-Length: 18" ?crlf
      "" ?crlf
      "Watson, come here.">>.

create_sipmsg(Msg, Source) when is_binary(Msg) ->
    create_sipmsg(Msg, Source, all).

create_sipmsg(Msg, Source, HeadersToParse) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg1 = ersip_msg:set_source(Source, PMsg),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg1, HeadersToParse),
    SipMsg.

make_default_source() ->
    tcp_source(default_peer()).

default_peer() ->
    {{127, 0, 0, 1}, 5060}.

tcp_source(Peer) ->
    ersip_source:new(Peer, tcp_transport(), undefined).

tcp_transport() ->
    ersip_transport:make(tcp).

udp_transport() ->
    ersip_transport:make(udp).

default_udp_conn() ->
    ersip_conn:new({127, 0, 0, 1}, 5061, {127, 0, 0, 2}, 5060, udp_transport(), #{}).

create_client_trans_result(Code, Request) ->
    ReqSipMsg = send_req_via_default_conn(Request),
    Bin = ersip_sipmsg:serialize_bin(ReqSipMsg),
    RemoteReq = create_sipmsg(Bin, make_default_source()),
    RemoteResp = ersip_sipmsg:reply(Code, RemoteReq),
    recv_response_via_default_conn(RemoteResp).

send_req_via_default_conn(OutReq) ->
    RemoteMsg = iolist_to_binary(ersip_request:send_via_conn(OutReq, default_udp_conn())),
    create_sipmsg(RemoteMsg, make_default_source()).

recv_response_via_default_conn(RemoteSipMsg) ->
    RemoteBin = ersip_sipmsg:serialize_bin(RemoteSipMsg),
    Conn = default_udp_conn(),
    {_Conn, [{new_response, Via, Msg}]} = ersip_conn:conn_data(RemoteBin, Conn),
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    {Via, SipMsg}.

