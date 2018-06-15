%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAS tests
%%

-module(ersip_uas_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

stateless_uas_basic_test() ->
    Options = #{stateless => true},
    REGISTERSipMsg = register_request(),
    {UAS0, SE0} = ersip_uas:new(REGISTERSipMsg, allowed_methods(), Options),
    ?assertEqual({ua_result, REGISTERSipMsg}, se_find(ua_result, SE0)),
    ?assertEqual(not_found, se_find(set_timer, SE0)),

    {UAS1, SE1} = ersip_uas:reply(trying_response(), UAS0),
    ?assertEqual({send_response, trying_response()}, se_find(send_response, SE1)),
    ?assertEqual(not_found, se_find(set_timer, SE0)),
    ?assertEqual(not_found, se_find(completed, SE1)),

    {_UAS2, SE2} = ersip_uas:reply(ok200_response(), UAS1),
    ?assertEqual({send_response, ok200_response()}, se_find(send_response, SE2)),
    ?assertEqual(not_found, se_find(set_timer, SE0)),
    ?assertEqual({completed, normal}, se_find(completed, SE2)),
    ok.

stateful_uas_basic_test() ->
    Options = #{stateless => false},
    REGISTERSipMsg = register_request(),
    {UAS0, SE0} = ersip_uas:new(REGISTERSipMsg, allowed_methods(), Options),
    ?assertEqual({ua_result, REGISTERSipMsg}, se_find(ua_result, SE0)),

    {UAS1, SE1} = ersip_uas:reply(trying_response(), UAS0),
    ?assertEqual({send_response, trying_response()}, se_find(send_response, SE1)),
    ?assertEqual(not_found, se_find(completed, SE1)),

    %% Retransmission handling
    

    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================


-define(crlf, "\r\n").

allowed_methods() ->
    ersip_method_set:new([ersip_method:register()]).

register_request() ->
    Msg = register_request_bin(),
    create_sipmsg(Msg, make_default_source()).

trying_response() ->
    SipMsg = register_request(),
    ersip_sipmsg:reply(100, SipMsg).

ok200_response() ->
    SipMsg = register_request(),
    ToTag = {tag, <<"ok200tag">>},
    Reply = ersip_reply:new(200, [{to_tag, ToTag}]),
    ersip_sipmsg:reply(Reply, SipMsg).

register_request_bin() ->
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

trying_bin() ->
    <<"SIP/2.0 100 Trying" ?crlf
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

make_default_source() ->
    tcp_source(default_peer()).

default_peer() ->
    {{127, 0, 0, 1}, 5060}.

tcp_source(Peer) ->
    ersip_source:new(Peer, tcp_transport(), undefined).

tcp_transport() ->
    ersip_transport:make(tcp).

create_sipmsg(Msg, Source) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg1 = ersip_msg:set_source(Source, PMsg),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg1, all),
    SipMsg.

se_find(Type, SE) ->
    case lists:keyfind(Type, 1, SE) of
        false ->
            not_found;
        X ->
            X
    end.
