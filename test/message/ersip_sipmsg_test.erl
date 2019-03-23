%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message tests
%%

-module(ersip_sipmsg_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

parse_request_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ?assertEqual(ersip_hdr_callid:make(CallId),        ersip_sipmsg:get(callid, SipMsg)),
    ?assertEqual(ersip_hdr_callid:make(CallId),        ersip_sipmsg:callid(SipMsg)),
    ?assertEqual(ersip_hdr_maxforwards:make(<<"70">>), ersip_sipmsg:get(maxforwards, SipMsg)),
    ?assertEqual(ersip_hdr_maxforwards:make(<<"70">>), ersip_sipmsg:maxforwards(SipMsg)),
    From = ersip_sipmsg:get(from, SipMsg),
    From = ersip_sipmsg:from(SipMsg),
    To   = ersip_sipmsg:get(to, SipMsg),
    To   = ersip_sipmsg:to(SipMsg),
    ?assertEqual({tag, <<"1928301774">>},       ersip_hdr_fromto:tag(From)),
    ?assertEqual({display_name, [<<"Bob">>]}, ersip_hdr_fromto:display_name(To)),
    Via  = ersip_sipmsg:get(topmost_via, SipMsg),
    Via  = ersip_sipmsg:topmost_via(SipMsg),
    ?assertEqual({sent_by, {hostname, <<"pc33.atlanta.com">>}, 5060},
                 ersip_hdr_via:sent_by(Via)),
    CSeq = ersip_sipmsg:get(cseq, SipMsg),
    CSeq = ersip_sipmsg:cseq(SipMsg),
    ?assertEqual(314159, ersip_hdr_cseq:number(CSeq)),
    ?assertEqual(ersip_method:invite(), ersip_hdr_cseq:method(CSeq)),
    ?assertEqual(<<"Test">>, ersip_sipmsg:body_bin(SipMsg)),
    ok.


parse_request_append_all_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg0} = ersip_sipmsg:parse(Msg,  [from]),
    {ok, SipMsg} = ersip_sipmsg:parse(SipMsg0, all),
    ?assertEqual(ersip_hdr_callid:make(CallId),        ersip_sipmsg:get(callid, SipMsg)),
    ?assertEqual(ersip_hdr_maxforwards:make(<<"70">>), ersip_sipmsg:get(maxforwards, SipMsg)),
    From = ersip_sipmsg:get(from, SipMsg),
    To   = ersip_sipmsg:get(to, SipMsg),
    ?assertEqual({tag, <<"1928301774">>},       ersip_hdr_fromto:tag(From)),
    ?assertEqual({display_name, [<<"Bob">>]}, ersip_hdr_fromto:display_name(To)),
    Via  = ersip_sipmsg:get(topmost_via, SipMsg),
    ?assertEqual({sent_by, {hostname, <<"pc33.atlanta.com">>}, 5060},
                 ersip_hdr_via:sent_by(Via)).

parse_request_all_required_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: x"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg0} = ersip_sipmsg:parse(Msg,  all_required),
    ?assertMatch({error, _}, ersip_sipmsg:parse(SipMsg0, all)).

parse_response_all_required_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"SIP/2.0 200 OK"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>;tag=123"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: x"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg0} = ersip_sipmsg:parse(Msg,  all_required),
    ?assertMatch({error, _}, ersip_sipmsg:parse(SipMsg0, all)).


parse_response_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"SIP/2.0 200 OK"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ?assertEqual(ersip_hdr_callid:make(CallId),        ersip_sipmsg:get(callid, SipMsg)),
    ?assertEqual(ersip_hdr_maxforwards:make(<<"70">>), ersip_sipmsg:get(maxforwards, SipMsg)),
    From = ersip_sipmsg:get(from, SipMsg),
    To   = ersip_sipmsg:get(to, SipMsg),
    ?assertEqual({tag, <<"1928301774">>},       ersip_hdr_fromto:tag(From)),
    ?assertEqual({display_name, [<<"Bob">>]}, ersip_hdr_fromto:display_name(To)),
    Via  = ersip_sipmsg:get(topmost_via, SipMsg),
    ?assertEqual({sent_by, {hostname, <<"pc33.atlanta.com">>}, 5060},
                 ersip_hdr_via:sent_by(Via)),
    ok.

parse_response_error_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"SIP/2.0 200 OK"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: x"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({error, {invalid_cseq, _}}, ersip_sipmsg:parse(Msg, [content_type])),
    ok.

parse_request_without_body_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Length: 0"
            ?crlf ?crlf
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ?assertEqual(ersip_hdr_callid:make(CallId),        ersip_sipmsg:get(callid, SipMsg)),
    ?assertEqual(ersip_hdr_maxforwards:make(<<"70">>), ersip_sipmsg:get(maxforwards, SipMsg)),
    From = ersip_sipmsg:get(from, SipMsg),
    To   = ersip_sipmsg:get(to, SipMsg),
    ?assertEqual({tag, <<"1928301774">>},       ersip_hdr_fromto:tag(From)),
    ?assertEqual({display_name, [<<"Bob">>]}, ersip_hdr_fromto:display_name(To)),
    Via  = ersip_sipmsg:get(topmost_via, SipMsg),
    ?assertEqual({sent_by, {hostname, <<"pc33.atlanta.com">>}, 5060},
                 ersip_hdr_via:sent_by(Via)),
    ?assertError({error, _}, ersip_sipmsg:get(content_type, SipMsg)),
    ok.

parse_request_with_body_no_content_type_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertEqual({error,{header_error,
                         {content_type,
                          {no_required_header,<<"Content-Type">>}}}},
                 ersip_sipmsg:parse(Msg, all)),
    ok.

parse_request_with_invalid_ruri_test() ->
    Msg = <<"INVITE bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({error,{invalid_ruri, _}}, ersip_sipmsg:parse(Msg, all)),
    ok.

parse_request_with_multiple_values_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "Max-Forwards: 78"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "CSeq: 314160 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ok, _}, ersip_sipmsg:parse(Msg, [])),
    ?assertMatch({error, {header_error, _}}, ersip_sipmsg:parse(Msg, [maxforwards])),
    ?assertMatch({error, {header_error, _}}, ersip_sipmsg:parse(Msg, [cseq])),
    ?assertMatch({error, {header_error, _}}, ersip_sipmsg:parse(Msg, [callid])),
    ?assertMatch({error, {header_error, _}}, ersip_sipmsg:parse(Msg, [from])),
    ?assertMatch({error, {header_error, _}}, ersip_sipmsg:parse(Msg, [to])),
    ok.

parse_invite_with_multiple_contact_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 78"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314160 INVITE"
            ?crlf "Contact: <sip:alice-1@pc33.atlanta.com>"
            ?crlf "Contact: <sip:alice-2@pc33.atlanta.com>"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ok, _}, ersip_sipmsg:parse(Msg, [])),
    ?assertMatch({error, {header_error, {contact, _}}}, ersip_sipmsg:parse(Msg, [contact])),
    ok.

parse_invite_response_with_multiple_contact_test() ->
    Msg = <<"SIP/2.0 200 OK"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>;tag=32134124"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314160 INVITE"
            ?crlf "Contact: <sip:alice-1@pc33.atlanta.com>"
            ?crlf "Contact: <sip:alice-2@pc33.atlanta.com>"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ok, _}, ersip_sipmsg:parse(Msg, [])),
    ?assertMatch({error, {header_error, {contact, _}}}, ersip_sipmsg:parse(Msg, [contact])),
    ok.

parse_invite_with_star_contact_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 78"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314160 INVITE"
            ?crlf "Contact: *"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ok, _}, ersip_sipmsg:parse(Msg, [])),
    ?assertMatch({error, {header_error, {contact, _}}}, ersip_sipmsg:parse(Msg, [contact])),
    ok.

parse_invite_with_multiple_content_type_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 78"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314160 INVITE"
            ?crlf "Contact: <sip:alice-1@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Type: application/sdp-1"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ok, _}, ersip_sipmsg:parse(Msg, [])),
    ?assertMatch({error, {header_error, {content_type, _}}}, ersip_sipmsg:parse(Msg, [content_type])),
    ok.

parse_invite_with_invalid_content_type_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 78"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314160 INVITE"
            ?crlf "Contact: <sip:alice-1@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp@1"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ok, _}, ersip_sipmsg:parse(Msg, [])),
    ?assertMatch({error, {header_error, {content_type, _}}}, ersip_sipmsg:parse(Msg, [content_type])),
    ok.

parse_notify_with_multiple_contact_test() ->
    Msg = <<"NOTIFY sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 78"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314160 NOTIFY"
            ?crlf "Contact: <sip:alice-1@pc33.atlanta.com>"
            ?crlf "Contact: <sip:alice-2@pc33.atlanta.com>"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ok, _}, ersip_sipmsg:parse(Msg, [])),
    ?assertMatch({error, {header_error, {contact, _}}}, ersip_sipmsg:parse(Msg, [contact])),
    ok.

parse_notify_response_with_multiple_contact_test() ->
    Msg = <<"SIP/2.0 200 OK"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>;tag=32134124"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314160 NOTIFY"
            ?crlf "Contact: <sip:alice-1@pc33.atlanta.com>"
            ?crlf "Contact: <sip:alice-2@pc33.atlanta.com>"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ok, _}, ersip_sipmsg:parse(Msg, [])),
    ?assertMatch({error, {header_error, {contact, _}}}, ersip_sipmsg:parse(Msg, [contact])),
    ok.

parse_on_demand_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, []),
    ?assertEqual(ersip_hdr_callid:make(CallId),        ersip_sipmsg:get(callid, SipMsg)),
    ?assertEqual(ersip_hdr_maxforwards:make(<<"70">>), ersip_sipmsg:get(maxforwards, SipMsg)),
    From = ersip_sipmsg:get(from, SipMsg),
    To   = ersip_sipmsg:get(to, SipMsg),
    ?assertEqual({tag, <<"1928301774">>},       ersip_hdr_fromto:tag(From)),
    ?assertEqual({display_name, [<<"Bob">>]}, ersip_hdr_fromto:display_name(To)),
    Via  = ersip_sipmsg:get(topmost_via, SipMsg),
    ?assertEqual({sent_by, {hostname, <<"pc33.atlanta.com">>}, 5060},
                 ersip_hdr_via:sent_by(Via)),
    ok.

parse_no_required_field_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({error, _}, ersip_sipmsg:parse(Msg, all)),
    ok.

parse_on_demand_parse_error_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: x"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, []),
    ?assertEqual(ersip_hdr_callid:make(CallId), ersip_sipmsg:get(callid, SipMsg)),
    ?assertError({error, _}, ersip_sipmsg:get(content_type, SipMsg)),
    ok.

reply_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ToTag = {tag, <<"4212312424">>},
    ReplyOpts = ersip_reply:new(404, [{to_tag, ToTag}]),
    RespSipMsg = ersip_sipmsg:reply(ReplyOpts, SipMsg),
    %% The From field of the response MUST equal the From header field of
    %% the request.
    ?assertEqual(ersip_sipmsg:get(from, SipMsg), ersip_sipmsg:get(from, RespSipMsg)),
    %% The Call-ID header field of the response MUST equal the
    %% Call-ID header field of the request.
    ?assertEqual(ersip_sipmsg:get(callid, SipMsg), ersip_sipmsg:get(callid, RespSipMsg)),
    %% The CSeq header field of the response MUST equal the CSeq field
    %% of the request.
    ?assertEqual(ersip_sipmsg:get(cseq, SipMsg), ersip_sipmsg:get(cseq, RespSipMsg)),
    %% The Via header field values in the response MUST equal the Via
    %% header field values in the request and MUST maintain the same
    %% ordering.
    ?assertEqual(ersip_sipmsg:raw_header(<<"via">>, SipMsg), ersip_sipmsg:raw_header(<<"via">>, RespSipMsg)),
    ToReq  = ersip_sipmsg:get(to, SipMsg),
    ToResp = ersip_sipmsg:get(to, RespSipMsg),
    %% However, if the To header field in the request did not contain
    %% a tag, the URI in the To header field in the response MUST
    %% equal the URI in the To header field
    ?assertEqual(ersip_hdr_fromto:uri(ToReq), ersip_hdr_fromto:uri(ToResp)),
    %% additionally, the UAS MUST add a tag to the To header field in
    %% the response
    ?assertEqual(ToTag, ersip_hdr_fromto:tag(ToResp)),
    ok.

reply_totag_autogenerated_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ReplyOpts = ersip_reply:new(404),
    RespMsg = ersip_sipmsg:reply(ReplyOpts, SipMsg),
    ToTag = ersip_hdr_fromto:tag(ersip_sipmsg:get(to, RespMsg)),
    ?assert(ersip_parser_aux:check_token(ersip_hdr_fromto:assemble(ToTag))),
    ok.

reply_indialog_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>;tag=1234421234"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ReplyOpts = ersip_reply:new(404, [{reason, <<"My Not Found">>}]),
    RespSipMsg = ersip_sipmsg:reply(ReplyOpts, SipMsg),
    %% The From field of the response MUST equal the From header field of
    %% the request.
    ?assertEqual(ersip_sipmsg:get(from, SipMsg), ersip_sipmsg:get(from, RespSipMsg)),
    %% The Call-ID header field of the response MUST equal the
    %% Call-ID header field of the request.
    ?assertEqual(ersip_sipmsg:get(callid, SipMsg), ersip_sipmsg:get(callid, RespSipMsg)),
    %% The CSeq header field of the response MUST equal the CSeq field
    %% of the request.
    ?assertEqual(ersip_sipmsg:get(cseq, SipMsg), ersip_sipmsg:get(cseq, RespSipMsg)),
    %% The Via header field values in the response MUST equal the Via
    %% header field values in the request and MUST maintain the same
    %% ordering.
    ?assertEqual(ersip_sipmsg:raw_header(<<"via">>, SipMsg), ersip_sipmsg:raw_header(<<"via">>, RespSipMsg)),
    %% If a request contained a To tag in the request, the To header
    %% field in the response MUST equal that of the request.
    ?assertEqual(ersip_sipmsg:get(to, SipMsg), ersip_sipmsg:get(to, RespSipMsg)),
    ok.

reply_100_trying_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ReplyOpts = ersip_reply:new(100),
    RespSipMsg = ersip_sipmsg:reply(ReplyOpts, SipMsg),
    %% The From field of the response MUST equal the From header field of
    %% the request.
    ?assertEqual(ersip_sipmsg:get(from, SipMsg), ersip_sipmsg:get(from, RespSipMsg)),
    %% The Call-ID header field of the response MUST equal the
    %% Call-ID header field of the request.
    ?assertEqual(ersip_sipmsg:get(callid, SipMsg), ersip_sipmsg:get(callid, RespSipMsg)),
    %% The CSeq header field of the response MUST equal the CSeq field
    %% of the request.
    ?assertEqual(ersip_sipmsg:get(cseq, SipMsg), ersip_sipmsg:get(cseq, RespSipMsg)),
    %% The Via header field values in the response MUST equal the Via
    %% header field values in the request and MUST maintain the same
    %% ordering.
    ?assertEqual(ersip_sipmsg:raw_header(<<"via">>, SipMsg), ersip_sipmsg:raw_header(<<"via">>, RespSipMsg)),
    ToReq  = ersip_sipmsg:get(to, SipMsg),
    ToResp = ersip_sipmsg:get(to, RespSipMsg),
    ?assertEqual(ToReq, ToResp),
    ok.

get_parts_test() ->
    CallId = <<"a84b4c76e66710@pc33.atlanta.com">>,
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: ", CallId/binary,
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ?assertEqual(undefined, ersip_sipmsg:status(SipMsg)),
    ?assertEqual(undefined, ersip_sipmsg:reason(SipMsg)),
    ?assertEqual({method, <<"INVITE">>}, ersip_sipmsg:method(SipMsg)),
    ?assertEqual(<<"INVITE">>, ersip_sipmsg:method_bin(SipMsg)),
    ok.


set_ruri_test() ->
    SipMsg0 = default_sipmsg(),
    RURIBin = <<"sip:alice@atlanta.com">>,
    RURI = ersip_uri:make(RURIBin),
    SipMsg1 = ersip_sipmsg:set_ruri(RURI, SipMsg0),
    ?assertEqual(RURI, ersip_sipmsg:ruri(SipMsg1)),
    SipMsg2 = rebuild_sipmsg(SipMsg1),
    ?assertEqual(RURI, ersip_sipmsg:ruri(SipMsg2)),
    ok.

userdata_test() ->
    SipMsg = default_sipmsg(),
    ?assertError({error, no_user_data}, ersip_sipmsg:user_data(SipMsg)),
    SipMsg1 = ersip_sipmsg:set_user_data(my_data, SipMsg),
    ?assertEqual(my_data, ersip_sipmsg:user_data(SipMsg1)),
    SipMsg2 = ersip_sipmsg:set_user_data(my_data_2, SipMsg1),
    ?assertEqual(my_data_2, ersip_sipmsg:user_data(SipMsg2)),
    SipMsg3 = ersip_sipmsg:clear_user_data(SipMsg2),
    ?assertError({error, no_user_data}, ersip_sipmsg:user_data(SipMsg3)),
    ok.

raw_ruri_manipulation_test() ->
    SipMsg = default_sipmsg(),
    RURI = ersip_sipmsg:ruri(SipMsg),
    SipMsg2 = ersip_sipmsg:set_ruri(RURI, SipMsg),
    RawMsg = ersip_sipmsg:raw_message(SipMsg2),
    {ok, _} = ersip_sipmsg:parse(RawMsg, all),
    ok.

set_raw_header_not_parsed_test() ->
    {ok, SipMsg0} = ersip_sipmsg:parse(default_msg(), []),
    NewContact0 = ersip_hdr:new(<<"Contact">>),
    NewContactBin = <<"Alice <sip:alice@pc34.atlanta.com>">>,
    NewContact  = ersip_hdr:add_value(NewContactBin, NewContact0),
    {ok, SipMsg1} = ersip_sipmsg:set_raw_header(NewContact, SipMsg0),
    {ok, SipMsg2} = ersip_sipmsg:parse(SipMsg1, [contact]),
    ParsedContactList = ersip_sipmsg:get(contact, SipMsg2),
    ?assertMatch([_], ParsedContactList),
    [NewAliceContact] = ParsedContactList,
    ?assertEqual(NewContactBin, iolist_to_binary(ersip_hdr_contact:assemble(NewAliceContact))),
    ok.

set_raw_header_parsed_test() ->
    {ok, SipMsg0} = ersip_sipmsg:parse(default_msg(), [contact]),
    NewContact0 = ersip_hdr:new(<<"Contact">>),
    NewContactBin = <<"Alice <sip:alice@pc34.atlanta.com>">>,
    NewContact  = ersip_hdr:add_value(NewContactBin, NewContact0),
    {ok, SipMsg1} = ersip_sipmsg:set_raw_header(NewContact, SipMsg0),
    ParsedContactList = ersip_sipmsg:get(contact, SipMsg1),
    ?assertMatch([_], ParsedContactList),
    [NewAliceContact] = ParsedContactList,
    ?assertEqual(NewContactBin, ersip_hdr_contact:assemble_bin(NewAliceContact)),
    ok.

set_raw_header_unknown_header_test() ->
    {ok, SipMsg0} = ersip_sipmsg:parse(default_msg(), []),
    MyHeader0 = ersip_hdr:new(<<"My-Header">>),
    MyHeaderVal = <<"My Header Value">>,
    MyHeader  = ersip_hdr:add_value(MyHeaderVal, MyHeader0),
    {ok, SipMsg} = ersip_sipmsg:set_raw_header(MyHeader, SipMsg0),
    SipMsgRB = rebuild_sipmsg(SipMsg),
    MyHeaderRB = ersip_sipmsg:raw_header(<<"my-header">>, SipMsgRB),
    ?assertEqual(MyHeader, MyHeaderRB),
    ok.

remove_header_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf ?crlf
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ?assertNotEqual(not_found, ersip_sipmsg:find(content_type, SipMsg)),
    lists:foreach(fun(HdrNameForm) ->
                          SipMsgNoCT = ersip_sipmsg:remove(HdrNameForm, SipMsg),
                          ?assertEqual(not_found, ersip_sipmsg:find(content_type, SipMsgNoCT)),
                          ?assertEqual(not_found, ersip_sipmsg:find(content_type, rebuild_sipmsg(SipMsgNoCT)))
                  end,
                  [content_type,
                   <<"Content-Type">>,
                   <<"cOnTeNt-TyPe">>,
                   ersip_hdr:make_key(<<"Content-Type">>),
                   <<"c">>]),
    ok.

remove_header_list_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf ?crlf
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    ?assertMatch({ok, _}, ersip_sipmsg:find(maxforwards, SipMsg)),
    ?assertMatch({ok, _}, ersip_sipmsg:find(contact, SipMsg)),
    SipMsg1 = ersip_sipmsg:remove_list([maxforwards, <<"contact">>], SipMsg),
    {ok, SipMsg2} = ersip_sipmsg:parse(ersip_sipmsg:serialize_bin(SipMsg1), all),
    ?assertEqual(not_found, ersip_sipmsg:find(maxforwards, SipMsg2)),
    ?assertEqual(not_found, ersip_sipmsg:find(contact, SipMsg2)),
    ok.

remove_custom_header_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "MyCustomHeader: Value"
            ?crlf ?crlf
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    RHdr = ersip_sipmsg:raw_header(<<"MyCustomHeader">>, SipMsg),
    ?assertEqual(false, ersip_hdr:is_empty(RHdr)),
    SipMsgNoCustom = ersip_sipmsg:remove(<<"MyCustomHeader">>, SipMsg),
    RHdrNoCustom = ersip_sipmsg:raw_header(<<"MyCustomHeader">>, SipMsgNoCustom),
    ?assertEqual(true, ersip_hdr:is_empty(RHdrNoCustom)),

    SipMsgNoCustomRB = rebuild_sipmsg(SipMsgNoCustom),
    RHdrNoCustomRB = ersip_sipmsg:raw_header(<<"MyCustomHeader">>, SipMsgNoCustomRB),
    ?assertEqual(true, ersip_hdr:is_empty(RHdrNoCustomRB)),
    ok.

remove_body_test() ->
    SipMsg = default_sipmsg(),
    SipMsgNoBody = ersip_sipmsg:remove_body(SipMsg),
    ?assertEqual(false, ersip_sipmsg:has_body(SipMsgNoBody)),
    SipMsgNoBodyNoCT = ersip_sipmsg:remove(<<"Content-Length">>, SipMsgNoBody),
    SipMsgRB = rebuild_sipmsg(SipMsgNoBodyNoCT),
    ?assertEqual(false, ersip_sipmsg:has_body(SipMsgRB)),
    ok.

cannot_set_status_of_request_test() ->
    ReqSipMsg = default_sipmsg(),
    ?assertEqual(request, ersip_sipmsg:type(ReqSipMsg)),
    ?assertError({api_error, _}, ersip_sipmsg:set_status(200, ReqSipMsg)),
    ok.

raw_parse_error_test() ->
    ?assertMatch({error, truncated_message}, ersip_sipmsg:parse(<<"@">>, all)),
    ?assertMatch({error, truncated_message}, ersip_sipmsg:parse(<<"SIP/2.0 200 OK" ?crlf>>, all)),
    ?assertMatch({error, truncated_message}, ersip_sipmsg:parse(<<"INVITE sip:alice@atlanta.com SIP/2.0" ?crlf>>, all)),
    ?assertMatch({error, truncated_message}, ersip_sipmsg:parse(<<"INVITE sip:alice@atlanta.com SIP/2.0" ?crlf>>, all)),
    ?assertMatch({error, {generic_parse_error, _}},
                 ersip_sipmsg:parse(<<"INVITE sip:alice@atlanta.com SIP/2.0" ?crlf
                                      "Content-Length: 100" ?crlf ?crlf>>, all)),
    ok.

header_keys_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "MyCustomHeader: Value"
            ?crlf ?crlf
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(Msg, all),
    Keys = ersip_sipmsg:header_keys(SipMsg),
    KeySet = gb_sets:from_list(Keys),
    [begin
         HdrKey = ersip_hnames:make_key(V),
         ?assertEqual(true, gb_sets:is_element(HdrKey, KeySet))
     end || V <- [<<"Via">>,
                  <<"Max-Forwards">>,
                  <<"to">>, %% intentional lower cased
                  <<"From">>,
                  <<"Call-ID">>,
                  <<"CSeq">>,
                  <<"Contact">>,
                  <<"Content-Type">>,
                  <<"MycustomHeader">>,
                  <<"MyCustomHeader">>]],
    ok.

copy_hdr_test() ->
    MsgBin
        = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "From: Bob <sip:bob@biloxi.com>"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "Call-ID: 1234",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf "My-Header: Header Value"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(MsgBin, [from, to, maxforwards]),
    MsgBin2
        = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 69"
            ?crlf "From: Alice <sip:alice@biloxi.com>"
            ?crlf "To: Alice <sip:alice@biloxi.com>"
            ?crlf "Call-ID: 4321",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg2} = ersip_sipmsg:parse(MsgBin2, [from, to, maxforwards]),
    SipMsg3 = ersip_sipmsg:copy_list([from, to, maxforwards, callid, <<"my-header">>],
                                     SipMsg, SipMsg2),
    MaxForwards = ersip_sipmsg:maxforwards(SipMsg3),
    ?assertEqual(70, ersip_hdr_maxforwards:value(MaxForwards)),
    From = ersip_sipmsg:from(SipMsg3),
    ?assertEqual(<<"Bob <sip:bob@biloxi.com>">>, ersip_hdr_fromto:assemble_bin(From)),
    MyHeader = ersip_sipmsg:raw_header(<<"my-header">>, SipMsg3),
    [HeaderValue] = ersip_hdr:raw_values(MyHeader),
    ?assertMatch(<<"Header Value">>, iolist_to_binary(HeaderValue)),
    ok.

set_method_test() ->
    MsgBin
        = <<"OPTIONS sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=zzzz"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "CSeq: 314159 OPTIONS"
            ?crlf "Call-ID: 1234",
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 0"
            ?crlf ?crlf ""
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(MsgBin, []),
    Reg = ersip_sipmsg:set_method(<<"REGISTER">>, SipMsg),
    Reg2 = ersip_sipmsg:set_method(ersip_method:register(), SipMsg),
    RegRebuild = rebuild_sipmsg(Reg),
    RegRebuild2 = rebuild_sipmsg(Reg2),
    ?assertEqual(<<"REGISTER">>, ersip_sipmsg:method_bin(RegRebuild)),
    ?assertEqual(<<"REGISTER">>, ersip_sipmsg:method_bin(RegRebuild2)),
    CSeq = ersip_sipmsg:get(cseq, RegRebuild),
    CSeq = ersip_sipmsg:get(cseq, RegRebuild2),
    %% CSeq also updated
    ?assertEqual(ersip_method:register(), ersip_hdr_cseq:method(CSeq)),
    ok.

set_method_nocseq_test() ->
    MsgBin
        = <<"OPTIONS sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=zzzz"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "Call-ID: 1234",
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 0"
            ?crlf ?crlf ""
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(MsgBin, []),
    Reg = ersip_sipmsg:set_method(<<"REGISTER">>, SipMsg),
    Reg2 = ersip_sipmsg:set_method(ersip_method:register(), SipMsg),
    RegRebuild = rebuild_sipmsg(Reg, []),
    RegRebuild2 = rebuild_sipmsg(Reg2, []),
    ?assertEqual(<<"REGISTER">>, ersip_sipmsg:method_bin(RegRebuild)),
    ?assertEqual(<<"REGISTER">>, ersip_sipmsg:method_bin(RegRebuild2)),
    ok.

set_reason_test() ->
    MsgBin
        = <<"SIP/2.0 200 OK"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=zzzz"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "CSeq: 314159 OPTIONS"
            ?crlf "Call-ID: 1234",
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 0"
            ?crlf ?crlf ""
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(MsgBin, []),
    Resp = ersip_sipmsg:set_reason(<<"Beautiful">>, SipMsg),
    RespRebuild = rebuild_sipmsg(Resp),
    ?assertEqual(<<"Beautiful">>, ersip_sipmsg:reason(RespRebuild)),
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================
rebuild_sipmsg(SipMsg) ->
    rebuild_sipmsg(SipMsg, all).

rebuild_sipmsg(SipMsg, Headers) ->
    SipMsgBin = ersip_sipmsg:serialize_bin(SipMsg),
    {ok, SipMsg1} = ersip_sipmsg:parse(SipMsgBin, Headers),
    SipMsg1.

default_msg() ->
    <<"INVITE sip:bob@biloxi.com SIP/2.0"
      ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
      ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
      ?crlf "Max-Forwards: 70"
      ?crlf "To: Bob <sip:bob@biloxi.com>"
      ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
      ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
      ?crlf "CSeq: 314159 INVITE"
      ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
      ?crlf "Content-Type: application/sdp"
      ?crlf "Content-Length: 4"
      ?crlf ?crlf "Test"
    >>.

default_sipmsg() ->
    {ok, SipMsg} = ersip_sipmsg:parse(default_msg(), all),
    SipMsg.

