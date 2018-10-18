%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message tests
%%

-module(ersip_siphdr_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

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
    SipMsg3 = ersip_siphdr:copy_headers([from, to, maxforwards, callid, <<"my-header">>],
                                        SipMsg, SipMsg2),

    MaxForwards = ersip_sipmsg:maxforwards(SipMsg3),
    ?assertEqual(70, ersip_hdr_maxforwards:value(MaxForwards)),
    From = ersip_sipmsg:from(SipMsg3),
    ?assertEqual(<<"Bob <sip:bob@biloxi.com>">>, ersip_hdr_fromto:assemble_bin(From)),
    MyHeader = ersip_sipmsg:raw_header(<<"my-header">>, SipMsg3),
    [HeaderValue] = ersip_hdr:raw_values(MyHeader),
    ?assertMatch(<<"Header Value">>, iolist_to_binary(HeaderValue)),
    ok.


set_hdr_test() ->
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
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(MsgBin, []),
    SipMsg2 = ersip_siphdr:set_header(maxforwards, ersip_hdr_maxforwards:make(<<"20">>), SipMsg),
    MaxForwards = ersip_sipmsg:maxforwards(SipMsg2),
    ?assertEqual(20, ersip_hdr_maxforwards:value(MaxForwards)).

spaces_before_colon_hdr_test() ->
    MsgBin
        = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via  : SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards  : 70"
            ?crlf "From  : sip:bob@biloxi.com"
            ?crlf "To  : sip:bob@biloxi.com"
            ?crlf "Call-ID  : 1234",
            ?crlf "CSeq  : 314159 INVITE"
            ?crlf "Contact  : <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type  : application/sdp"
            ?crlf "Content-Length  : 4"
            ?crlf ?crlf "Test"
          >>,
    {ok, SipMsg} = ersip_sipmsg:parse(MsgBin, all),
    ToHeader = ersip_sipmsg:to(SipMsg),
    ?assertEqual(ersip_hdr_fromto:make(<<"sip:bob@biloxi.com">>), ToHeader),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================
