%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Server non-INVITE transaction test
%%

-module(ersip_trans_server_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

uas_reliable_test() ->
    {UAS0, SideEffects0} = ersip_trans_server:new(reliable, request(), #{}),
    SideEffectsMap0 = maps:from_list(SideEffects0),
    ?assertEqual(request(), maps:get(tu_result, SideEffectsMap0)),

    %% Branch 1: (Retransmit before provisional response is ignored)
    {UAS0, []} = ersip_trans_server:event(retransmit_event(), UAS0),

    %% Branch 2: (Sending provisional response)
    {UAS_2_0, SideEffects_2_0} = ersip_trans_server:event(prov_response_event(), UAS0),
    SideEffectsMap_2_0 = maps:from_list(SideEffects_2_0),
    %% Response with provisional resp:
    ?assertEqual(prov_response(), maps:get(send, SideEffectsMap_2_0)),

    %% Branch 2.1: (Retransmit after provisional response)
    {UAS_2_1_0, SideEffects_2_1_0} = ersip_trans_server:event(retransmit_event(), UAS_2_0),
    SideEffectsMap_2_1_0 = maps:from_list(SideEffects_2_1_0),
    %% Last provisional response sent
    ?assertEqual(prov_response(), maps:get(send, SideEffectsMap_2_1_0)),
    %% Send one more provisional response:
    {UAS_2_1_1, SideEffects_2_1_1} = ersip_trans_server:event(prov_response_event_2(), UAS_2_1_0),
    SideEffectsMap_2_1_1 = maps:from_list(SideEffects_2_1_1),
    ?assertEqual(prov_response_2(), maps:get(send, SideEffectsMap_2_1_1)),
    {_, SideEffects_2_1_2} = ersip_trans_server:event(retransmit_event(), UAS_2_1_1),
    SideEffectsMap_2_1_2 = maps:from_list(SideEffects_2_1_2),
    %% Last provisional response sent
    ?assertEqual(prov_response_2(), maps:get(send, SideEffectsMap_2_1_2)),

    %% Branch 2.2: (Final after provisional response)
    {_UAS_2_2_0, SideEffects_2_2_0} = ersip_trans_server:event(final_response_event(), UAS_2_0),
    SideEffectsMap_2_2_0 = maps:from_list(SideEffects_2_2_0),
    %% Final response is sent
    ?assertEqual(final_response(), maps:get(send, SideEffectsMap_2_2_0)),
    %% Transaction is cleared
    ?assertMatch(unknown, maps:get(clear_trans, SideEffectsMap_2_2_0)),

    %% Branch 3: (Sending final response without provisional)
    {_UAS_3_0, SideEffects_3_0} = ersip_trans_server:event(final_response_event(), UAS0),
    SideEffectsMap_3_0 = maps:from_list(SideEffects_3_0),
    %% Response with provisional resp:
    ?assertEqual(final_response(), maps:get(send, SideEffectsMap_3_0)),
    %% Transaction is cleared
    ?assertMatch(unknown, maps:get(clear_trans, SideEffectsMap_3_0)).

uas_unreliable_test() ->
    {UAS0, SideEffects0} = ersip_trans_server:new(unreliable, request(), #{}),
    SideEffectsMap0 = maps:from_list(SideEffects0),
    ?assertEqual(request(), maps:get(tu_result, SideEffectsMap0)),

    %% Branch 1: (Sending final response without provisional)
    {UAS_1_0, SideEffects_1_0} = ersip_trans_server:event(final_response_event(), UAS0),
    SideEffectsMap_1_0 = maps:from_list(SideEffects_1_0),
    %% Response with provisional resp:
    ?assertEqual(final_response(), maps:get(send, SideEffectsMap_1_0)),
    %% Timer J is set:
    ?assertMatch({32000, timer_j}, maps:get(set_timer, SideEffectsMap_1_0)),
    %% Retransmits cause send last response:
    {UAS_1_1, SideEffects_1_1} = ersip_trans_server:event(retransmit_event(), UAS_1_0),
    SideEffectsMap_1_1 = maps:from_list(SideEffects_1_1),
    ?assertEqual(final_response(), maps:get(send, SideEffectsMap_1_1)),
    %% Any other final responses passed by the TU to the server
    %% transaction MUST be discarded while in the "Completed" state.
    {UAS_1_1, []} = ersip_trans_server:event(final_response_event(), UAS_1_1),
    %% Timer J fired => clear transaction
    {_UAS_1_2, SideEffects_1_2} = ersip_trans_server:event({timer, timer_j}, UAS_1_1),
    SideEffectsMap_1_2 = maps:from_list(SideEffects_1_2),
    %% Transaction is cleared
    ?assertMatch(unknown, maps:get(clear_trans, SideEffectsMap_1_2)).

%%%===================================================================
%%% Helpers
%%%===================================================================

-define(crlf, "\r\n").

request_bin() ->
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

request() ->
    parse_message(request_bin()).

retransmit_event() ->
    {received, request()}.

prov_response() ->
    make_reply(100).

prov_response_event() ->
    {send, prov_response()}.

prov_response_2() ->
    make_reply(101).

prov_response_event_2() ->
    {send, prov_response_2()}.

make_reply(Code) ->
    ToTag = {tag, <<"4212312424">>},
    Reply = ersip_reply:new(Code, [{to_tag, ToTag}]),
    ersip_sipmsg:reply(Reply, request()).

final_response() ->
    make_reply(200).

final_response_event() ->
    {send, final_response()}.

parse_message(Bin) when is_binary(Bin) ->
    P  = ersip_parser:new_dgram(Bin),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg, all),
    SipMsg.
