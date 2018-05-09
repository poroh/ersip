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

new_server_transaction_test() ->
    Msg = <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
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
            ?crlf>>,
    SipMsg = create_sipmsg(Msg, make_default_source()),
    {ServerTrans, SE} = ersip_trans:new_server(SipMsg, default_sip_options()),
    ?assertEqual(ersip_trans:id(ServerTrans), ersip_trans_id:make_server(SipMsg)),
    ?assertEqual([ersip_trans_se:tu_result(SipMsg)], SE).

new_client_transaction_test() ->
    Msg = <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
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
            ?crlf>>,
    SipMsg = create_sipmsg(Msg, make_default_source()),
    Branch = ersip_branch:make_random(7),
    OutReq = ersip_request:new(SipMsg, Branch),
    {ServerTrans, SE} = ersip_trans:new_client(OutReq, udp_transport(), default_sip_options()),
    ?assertEqual(ersip_trans:id(ServerTrans), Branch),
    ?assertEqual(ersip_trans_se:send(OutReq), lists:keyfind(send, 1, SE)),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

create_sipmsg(Msg, Source) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg1 = ersip_msg:set_source(Source, PMsg),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg1, all),
    SipMsg.

make_default_source() ->
    udp_source(default_peer()).

default_peer() ->
    {{127, 0, 0, 1}, 5060}.

udp_source(Peer) ->
    ersip_source:new(Peer, udp_transport(), undefined).

udp_transport() ->
    ersip_transport:make(udp).

default_sip_options() ->
    #{}.

