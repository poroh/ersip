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
    ?assertEqual(not_found, se_find(set_timer, SE2)),
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
    {UAS2, SE2} = ersip_uas:request(REGISTERSipMsg, UAS1),
    ?assertEqual({send_response, trying_response()}, se_find(send_response, SE2)),
    ?assertEqual(not_found, se_find(ua_result, SE2)),

    {_UAS3, SE3} = ersip_uas:reply(ok200_response(), UAS2),
    ?assertEqual({send_response, ok200_response()}, se_find(send_response, SE3)),
    ?assertEqual(not_found, se_find(set_timer, SE3)),
    ?assertEqual({completed, normal}, se_find(completed, SE3)),
    ok.

stateful_uas_basic_unreliable_test() ->
    T1 = 70,
    SIPOptions = #{sip_t1 => T1},
    Options = #{stateless => false, sip => SIPOptions},
    REGISTERSipMsg = register_request(make_udp_source()),
    {UAS0, _SE0} = ersip_uas:new(REGISTERSipMsg, allowed_methods(), Options),
    {UAS1, SE1} = ersip_uas:reply(ok200_response(), UAS0),
    ?assertEqual({send_response, ok200_response()}, se_find(send_response, SE1)),
    {set_timer, {Timeout, TimerJEv}} = se_find(set_timer, SE1),
    ?assertEqual(Timeout, 64*T1),
    {_UAS2, SE2} = ersip_uas:timer(TimerJEv, UAS1),
    ?assertEqual({completed, normal}, se_find(completed, SE2)),
    ok.

stateful_uas_not_allowed_method_test() ->
    Options = #{stateless => false},
    REGISTERSipMsg = register_request(),
    AllowedMethods = ersip_method_set:invite_set(),
    {_UAS, SE0} = ersip_uas:new(REGISTERSipMsg, AllowedMethods, Options),
    {send_response, Resp405} = se_find(send_response, SE0),
    ?assertEqual(405, ersip_sipmsg:status(Resp405)),
    %% The UAS MUST also add an Allow header field to the 405 (Method
    %% Not Allowed) response.  The Allow header field MUST list the
    %% set of methods supported by the UAS generating the message.
    AllowHdr = ersip_sipmsg:get(allow, Resp405),
    ?assertEqual({allow, AllowedMethods}, AllowHdr),
    %% Completed immediately for reliable transport.
    ?assertEqual({completed, normal}, se_find(completed, SE0)),
    ok.

stateful_uas_not_allowed_method_for_unreliable_transport_test() ->
    T1 = 71,
    SIPOptions = #{sip_t1 => T1},
    Options = #{stateless => false, sip => SIPOptions},
    REGISTERSipMsg = register_request(make_udp_source()),
    AllowedMethods = ersip_method_set:invite_set(),
    {UAS0, SE0} = ersip_uas:new(REGISTERSipMsg, AllowedMethods, Options),
    {send_response, Resp405} = se_find(send_response, SE0),
    ?assertEqual(405, ersip_sipmsg:status(Resp405)),
    ?assertEqual(not_found, se_find(completed, SE0)),

    {set_timer, {Timeout, TimerJEv}} = se_find(set_timer, SE0),
    ?assertEqual(Timeout, 64*T1),
    {_UAS2, SE1} = ersip_uas:timer(TimerJEv, UAS0),
    ?assertEqual({completed, normal}, se_find(completed, SE1)),
    ok.

stateless_uas_not_allowed_method_for_unreliable_transport_test() ->
    Options = #{stateless => true},
    REGISTERSipMsg = register_request(make_udp_source()),
    AllowedMethods = ersip_method_set:invite_set(),
    {_UAS, SE0} = ersip_uas:new(REGISTERSipMsg, AllowedMethods, Options),
    {send_response, Resp405} = se_find(send_response, SE0),
    ?assertEqual(405, ersip_sipmsg:status(Resp405)),
    %% The UAS MUST also add an Allow header field to the 405 (Method
    %% Not Allowed) response.  The Allow header field MUST list the
    %% set of methods supported by the UAS generating the message.
    AllowHdr = ersip_sipmsg:get(allow, Resp405),
    ?assertEqual({allow, AllowedMethods}, AllowHdr),
    %% Completed immediately for reliable transport.
    ?assertEqual({completed, normal}, se_find(completed, SE0)),
    ok.

cannot_parse_require_field_test() ->
    Options = #{statless => true},
    REGISTERSipMsg = register_request_bad_require(make_default_source()),
    {_UAS0, SE0} = ersip_uas:new(REGISTERSipMsg, allowed_methods(), Options),
    {send_response, Resp400} = se_find(send_response, SE0),
    ?assertEqual(400, ersip_sipmsg:status(Resp400)),
    ok.

scheme_validation_fail_test() ->
    Options = #{stateless => false,
                check_scheme => fun(_) -> false end
               },
    REGISTERSipMsg = register_request(),
    {_UAS, SE0} = ersip_uas:new(REGISTERSipMsg, allowed_methods(), Options),
    {send_response, Resp420} = se_find(send_response, SE0),
    ?assertEqual(420, ersip_sipmsg:status(Resp420)),
    ok.

scheme_validation_success_test() ->
    Options = #{stateless => false,
                check_scheme => fun(S) -> S == {scheme, <<"tel">>} end
               },
    REGISTERSipMsg = register_request_tel_uri(make_default_source()),
    {_UAS, SE0} = ersip_uas:new(REGISTERSipMsg, allowed_methods(), Options),
    ?assertEqual(not_found, se_find(send_response, SE0)),
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

register_request(Source) ->
    Msg = register_request_bin(),
    create_sipmsg(Msg, Source).

register_request_bad_require(Source) ->
    Msg = register_request_bad_require_bin(),
    create_sipmsg(Msg, Source, []).

register_request_tel_uri(Source) ->
    Msg = register_request_tel_uri_bin(),
    create_sipmsg(Msg, Source, []).

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

register_request_bad_require_bin() ->
    <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: 4 REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Expires: 3600" ?crlf
      "Require: ?" ?crlf
      "Content-Length: 0" ?crlf
      "Contact: <sip:1000@192.168.100.11:5070;line=69210a2e715cee1>" ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      "User-Agent: Linphone/3.6.1 (eXosip2/4.1.0)" ?crlf
      ?crlf>>.

register_request_tel_uri_bin() ->
    <<"REGISTER tel:+111 SIP/2.0" ?crlf
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

make_udp_source() ->
    udp_source(default_peer()).

default_peer() ->
    {{127, 0, 0, 1}, 5060}.

tcp_source(Peer) ->
    ersip_source:new(Peer, tcp_transport(), undefined).

udp_source(Peer) ->
    ersip_source:new(Peer, udp_transport(), undefined).

tcp_transport() ->
    ersip_transport:make(tcp).

udp_transport() ->
    ersip_transport:make(udp).

create_sipmsg(Msg, Source) when is_binary(Msg) ->
    create_sipmsg(Msg, Source, all).

create_sipmsg(Msg, Source, HeadersToParse) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg1 = ersip_msg:set_source(Source, PMsg),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg1, HeadersToParse),
    SipMsg.

se_find(Type, SE) ->
    case lists:keyfind(Type, 1, SE) of
        false ->
            not_found;
        X ->
            X
    end.
