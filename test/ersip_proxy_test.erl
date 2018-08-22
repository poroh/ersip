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

    %% 1. Create new stateful proxy request state.
    %% 2. Process server transaction result.
    {StateProcessReq, ServerTransId} = create_stateful(MessageSipMsg, #{}),

    %% Next we have two branches:
    %% Forward to single target (branch 1)
    %% Forward to two targets (branches 2.*)

    %% ==================== Branch 1
    %% 3. (branch 1) Choose one target to forward:
    Target@1 = ersip_uri:make(<<"sip:contact@192.168.1.1">>),
    {State@1_Forward, SE@1_Forward} = ersip_proxy:forward_to(Target@1, StateProcessReq),
    %%    - Check that client transaction is created:
    ?assertMatch({create_trans, {client, _, _}}, se_event(create_trans, SE@1_Forward)),
    {create_trans, {client, ClientTransId@1, SingleReq}} = se_event(create_trans, SE@1_Forward),
    %%    - Check that message is sent to target:
    ?assertEqual(Target@1, ersip_request:nexthop(SingleReq)),
    %%    - Check that timer C is not set
    ?assertEqual(not_found, se_event(set_timer, SE@1_Forward)),

    %% 4. (branch 1) Creating response and pass as client transaction result:
    {_, Resp@1} = create_client_trans_result(200, SingleReq),
    {_, SESingleResp} = ersip_proxy:trans_result(ClientTransId@1, Resp@1, State@1_Forward),
    %%    - Response is passed to request source and proxy is stopped
    ?assertMatch({response, {ServerTransId, _}}, se_event(response, SESingleResp)),
    %%    - Check that proxy process is stopped
    ?assertMatch({stop, _}, se_event(stop, SESingleResp)),

    %% ==================== Branch 2
    %% 3. (branch 2) Choose two targets to forward:
    Target@2_1 = ersip_uri:make(<<"sip:contact1@192.168.1.1">>),
    Target@2_2 = ersip_uri:make(<<"sip:contact2@192.168.1.2">>),
    Target@2 = [Target@2_1, Target@2_2],
    {State@2_Forward, SE@2_Forward} = ersip_proxy:forward_to(Target@2, StateProcessReq),
    %%    - Check that client transaction is created:
    ClientTrans@2 = se_all(create_trans, SE@2_Forward),
    [?assertMatch({create_trans, {client, _, _}}, X) || X <- ClientTrans@2],
    ?assertEqual(length(Target@2), length(ClientTrans@2)),
    ?assertEqual(not_found, se_event(set_timer, SE@1_Forward)),


    [{create_trans, {client, ClientTransId1@2, Req1@2}},
     {create_trans, {client, ClientTransId2@2, Req2@2}}] = SE@2_Forward,

    %% Idea here to check forking results:
    %% 200 then 200 (branch 2.1)
    %% 200 then 401 (branch 2.2)
    %% 401 then 200 (branch 2.3)
    %% 401 then 401 (branch 2.4)
    %% 600 then 200 (branch 2.5)
    {_, Resp1@2_200} = create_client_trans_result(200, Req1@2),
    {_, Resp1@2_401} = create_client_trans_result(401, Req1@2),
    {_, Resp1@2_600} = create_client_trans_result(600, Req1@2),
    {_, Resp2@2_200} = create_client_trans_result(200, Req2@2),
    {_, Resp2@2_401} = create_client_trans_result(401, Req2@2),

    %% ==================== Branch 2.1 (200 then 200)
    %% 4. (branch 2.1) Creating responses on first transaction and pass result
    {State@2_1_Resp1, SE@2_1_Resp1} = ersip_proxy:trans_result(ClientTransId1@2, Resp1@2_200, State@2_Forward),
    %%    - Response is passed to request source and proxy is not stopped
    ?assertMatch({response, {ServerTransId, _}}, se_event(response, SE@2_1_Resp1)),
    ?assertMatch(not_found, se_event(stop, SE@2_1_Resp1)),
    %%    - Second 200 response is passed:
    {_, SE@2_1_Resp2} = ersip_proxy:trans_result(ClientTransId2@2, Resp2@2_200, State@2_1_Resp1),
    %%    - Response is passed to server transaction and proxy is stopped
    %%      Note: Response will be discarded by real server transaction by this caluse:
    %%            Any other final responses passed by the TU to the server
    %%            transaction MUST be discarded while in the "Completed"
    %%            state.
    ?assertMatch({response, {ServerTransId, _}}, se_event(response, SE@2_1_Resp2)),
    ?assertMatch({stop, _}, se_event(stop, SE@2_1_Resp2)),

    %% ==================== Branch 2.2 (200 then 401)
    %% 4. (branch 2.2) Creating responses on first transaction and pass result
    {State@2_2_Resp1, SE@2_2_Resp1} = ersip_proxy:trans_result(ClientTransId1@2, Resp1@2_200, State@2_Forward),
    %%    - Response is passed to request source and proxy is not stopped
    ?assertMatch({response, {ServerTransId, _}}, se_event(response, SE@2_2_Resp1)),
    ?assertMatch(not_found, se_event(stop, SE@2_2_Resp1)),
    %%    - Second 200 response is passed:
    {_, SE@2_2_Resp2} = ersip_proxy:trans_result(ClientTransId2@2, Resp2@2_200, State@2_2_Resp1),
    %%    - Response is passed to server transaction and proxy is stopped
    %%      Note: same as in branch 2.1.
    ?assertMatch({response, {ServerTransId, _}}, se_event(response, SE@2_2_Resp2)),
    ?assertMatch({stop, _}, se_event(stop, SE@2_2_Resp2)),

    %% ==================== Branch 2.3 (401 then 200)
    %% 4. (branch 2.3) Creating responses on first transaction and pass result
    {State@2_3_Resp1, SE@2_3_Resp1} = ersip_proxy:trans_result(ClientTransId1@2, Resp1@2_401, State@2_Forward),
    %%    - Response is not passed (collected)
    ?assertMatch(not_found, se_event(response, SE@2_3_Resp1)),
    ?assertMatch(not_found, se_event(stop, SE@2_3_Resp1)),
    %%    - Second 200 response is passed:
    {_, SE@2_3_Resp2} = ersip_proxy:trans_result(ClientTransId2@2, Resp2@2_200, State@2_3_Resp1),
    %%    - Response 200 is passed to server transaction and proxy is stopped
    ?assertMatch({response, {ServerTransId, Resp2@2_200}}, se_event(response, SE@2_3_Resp2)),
    ?assertMatch({stop, _}, se_event(stop, SE@2_3_Resp2)),

    %% ==================== Branch 2.4 (401 then 401)
    %% 4. (branch 2.4) Creating responses on first transaction and pass result
    {State@2_4_Resp1, SE@2_4_Resp1} = ersip_proxy:trans_result(ClientTransId1@2, Resp1@2_401, State@2_Forward),
    %%    - Response is not passed (collected)
    ?assertMatch(not_found, se_event(response, SE@2_4_Resp1)),
    ?assertMatch(not_found, se_event(stop, SE@2_4_Resp1)),
    %%    - Second 401 response is passed:
    {_, SE@2_4_Resp2} = ersip_proxy:trans_result(ClientTransId2@2, Resp2@2_401, State@2_4_Resp1),
    %%    - Response 401 is passed to server transaction and proxy is stopped
    ?assertMatch({response, {ServerTransId, _}}, se_event(response, SE@2_4_Resp2)),
    ?assertMatch({stop, _}, se_event(stop, SE@2_4_Resp2)),
    {response, {ServerTransId, Resp401@2_4}} = se_event(response, SE@2_4_Resp2),
    ?assertEqual(401, ersip_sipmsg:status(Resp401@2_4)),

    %% ==================== Branch 2.5 (600 then 200)
    %% 4. (branch 2.5) Creating responses on first transaction and pass result
    {State@2_5_Resp1, SE@2_5_Resp1} = ersip_proxy:trans_result(ClientTransId1@2, Resp1@2_600, State@2_Forward),
    %%    - Response 600 is not passed (collected)
    ?assertMatch(not_found, se_event(response, SE@2_5_Resp1)),
    ?assertMatch(not_found, se_event(stop, SE@2_5_Resp1)),
    %%    - Second 200 response is passed:
    {_, SE@2_5_Resp2} = ersip_proxy:trans_result(ClientTransId2@2, Resp2@2_200, State@2_5_Resp1),
    %%    - Response 200 is passed to server transaction and proxy is stopped
    ?assertMatch({response, {ServerTransId, Resp2@2_200}}, se_event(response, SE@2_5_Resp2)),
    ?assertMatch({stop, _}, se_event(stop, SE@2_5_Resp2)),

    ok.

basic_proxy_invite_test() ->
    InviteSipMsg = invite_request(),

    DefaultTimerCTimeout = timer:seconds(180),

    %% 1. Create new stateful proxy request state.
    %% 2. Process server transaction result.
    {SelectTargetState, ServerTransId} = create_stateful(InviteSipMsg, #{}),

    %% 3. Choose one target to forward:
    Target = ersip_uri:make(<<"sip:contact@192.168.1.1">>),
    {Forward_State, Forward_SE} = ersip_proxy:forward_to(Target, SelectTargetState),
    %%    - Check that client transaction is created:
    ?assertMatch({create_trans, {client, _, _}}, se_event(create_trans, Forward_SE)),
    {create_trans, {client, ClientTransId, Req}} = se_event(create_trans, Forward_SE),
    %%    - Check that message is sent to target:
    ?assertEqual(Target, ersip_request:nexthop(Req)),
    %%    - Check that timer C is set to default timeout 180 seconds.
    ?assertMatch({set_timer, {DefaultTimerCTimeout, _}}, se_event(set_timer, Forward_SE)),

    %% 4. Pass 180 provisional response:
    {_, Resp180} = create_client_trans_result(180, Req),
    {Provisional_State, Provisional_SE} = ersip_proxy:trans_result(ClientTransId, Resp180, Forward_State),
    %%    - Response is passed to request source and proxy is stopped
    ?assertMatch({response, {ServerTransId, _}}, se_event(response, Provisional_SE)),
    %%    - Check that timer C is reset to default timeout 180 seconds.
    ?assertMatch({set_timer, {DefaultTimerCTimeout, _}}, se_event(set_timer, Provisional_SE)),
    %%    - Check that process is not stopped
    ?assertMatch(not_found, se_event(stop, Provisional_SE)),
    %%    - Check no new transaction is created:
    ?assertMatch(not_found, se_event(create_trans, Provisional_SE)),

    %% 5. Creating 200 response and pass as client transaction result:
    {_, Resp200} = create_client_trans_result(200, Req),
    {_, Final_SE} = ersip_proxy:trans_result(ClientTransId, Resp200, Provisional_State),
    %%    - Response is passed to request source and proxy is stopped
    ?assertMatch({response, {ServerTransId, _}}, se_event(response, Final_SE)),
    %%    - Check no new transaction is created:
    ?assertMatch(not_found, se_event(create_trans, Provisional_SE)),
    %%    - Check that process is stopped
    ?assertMatch({stop, _}, se_event(stop, Final_SE)),
    ok.

basic_proxy_invite_cancel_test() ->
    InviteSipMsg = invite_request(),
    %% 1. Create new stateful proxy request state.
    %% 2. Process server transaction result.
    {SelectTargetState, ServerTransId} = create_stateful(InviteSipMsg, #{}),

    %% 3. Choose one target to forward:
    Target = ersip_uri:make(<<"sip:contact@192.168.1.1">>),
    {Forward_State, Forward_SE} = ersip_proxy:forward_to(Target, SelectTargetState),
    {create_trans, {client, ClientTransId, Req}} = se_event(create_trans, Forward_SE),

    %% 4. Pass 180 provisional response:
    {_, Resp180} = create_client_trans_result(180, Req),
    {Provisional_State, Provisional_SE} = ersip_proxy:trans_result(ClientTransId, Resp180, Forward_State),

    %% 5. Cancel request:
    {Cancel_State, Cancel_SE} = ersip_proxy:cancel(Forward_State),
    %%    - CANCEL transaction is created:
    ?assertMatch({create_trans, {client, _, _}}, se_event(create_trans, Cancel_SE)),
    {create_trans, {client, CancelClientTransId, CancelReq}} = se_event(create_trans, Cancel_SE),
    ?assertEqual(ersip_method:cancel(), ersip_sipmsg:method(ersip_request:sipmsg(CancelReq))),

    %% 6. Response 200 on CANCEL request:
    {_, CancelResp200} = create_client_trans_result(200, CancelReq),
    {Cancel200_State, Cancel200_SE} = ersip_proxy:trans_result(CancelClientTransId, CancelResp200, Cancel_State),
    ?assertEqual(0, length(Cancel200_SE)),

    %% 7. Response 487 on INVITE request:
    {_, Resp487} = create_client_trans_result(487, Req),
    {Resp487_State, Resp487_SE} = ersip_proxy:trans_result(ClientTransId, Resp487, Cancel200_State),
    %%   - Check that 487 is passed as server transaction result:
    ?assertMatch({response, {ServerTransId, _}}, se_event(response, Resp487_SE)),
    %%   - Check that request processing is stopped:
    ?assertMatch({stop, _}, se_event(stop, Resp487_SE)),
    ok.

%% TODO:
%% Testcases:
%%   - 487 on INVITE then 200 OK on CANCEL
%%   - Cancel request and Timer C fires after CANCEL sent
%%   - Cancel request when Server transaction created
%%   - Cancel request before target is created
%%   - Cancel request when some 4xx is collected
%%   - Cancel request when 2xx is passed
%%   - Cancel request when 6xx is collected
%%   - 6xx received after CANCEL request
%%   - 2xx received after CANCEL request
%%   - Timer C fired when 4xx is collected
%%

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

se_all(Type, SE) ->
    proplists:lookup_all(Type, SE).


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


invite_request() ->
    create_sipmsg(invite_request_bin(), make_default_source()).

cancel_request() ->
    ersip_request:cancel(invite_request()).

invite_request_bin() ->
    <<"INVITE sip:bob@biloxi.com SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8" ?crlf
      "Max-Forwards: 70" ?crlf
      "To: Bob <sip:bob@biloxi.com>" ?crlf
      "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
      "Call-ID: a84b4c76e66710" ?crlf
      "CSeq: 314159 INVITE" ?crlf
      "Contact: <sip:alice@pc33.atlanta.com>" ?crlf
      "Content-Type: application/sdp" ?crlf
      "Content-Length: 4" ?crlf
      ?crlf
      "Test">>.

create_sipmsg(Msg, Source) when is_binary(Msg) ->
    create_sipmsg(Msg, Source, all).

create_sipmsg(Msg, Source, HeadersToParse) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg1 = ersip_msg:set_source(Source, PMsg),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg1, HeadersToParse),
    SipMsg.

create_stateful(SipMsg, Options) ->
    RURI = ersip_sipmsg:ruri(SipMsg),

    %% 1. Create new stateful proxy request state.
    {StateCreateTrans, SECreateTrans} = ersip_proxy:new_stateful(SipMsg, Options),
    %%    - Check that new server transaction is created.
    ?assertMatch({create_trans, {server, _, SipMsg}}, se_event(create_trans, SECreateTrans)),
    {create_trans, {server, ServerTransId, SipMsg}} = se_event(create_trans, SECreateTrans),
    ?assertEqual(1, length(SECreateTrans)),

    %% 2. Process server transaction result.
    {StateProcessReq, SEProcessReq} = ersip_proxy:trans_result(ServerTransId, SipMsg, StateCreateTrans),
    %%    - Check that RURI is used to for selecting target of the request.
    RURI = ersip_sipmsg:ruri(SipMsg),
    ?assertMatch({select_target, RURI}, se_event(select_target, SEProcessReq)),
    ?assertMatch(1, length(SEProcessReq)),
    {StateProcessReq, ServerTransId}.

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

