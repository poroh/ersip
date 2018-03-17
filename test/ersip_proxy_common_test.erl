%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common proxy routinges tests 
%%

-module(ersip_proxy_common_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

request_validation_success_test() ->
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
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ ok, _ }, request_validation(raw_message(Msg))),
    ok.

request_validation_bad_maxforwards_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: xyz"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    { reply, BadMsg } = request_validation(raw_message(Msg)),
    ?assertEqual(400, ersip_sipmsg:status(BadMsg)),
    Reason = ersip_sipmsg:reason(BadMsg),
    ?assert(binary:match(ersip_bin:to_lower(Reason), <<"max-forwards">>) =/= nomatch),
    ok.

request_validation_unsupported_scheme_test() ->
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
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    { reply, BadMsg } = request_validation(raw_message(Msg), 
                                           #{ scheme_val_fun => fun(_) -> false end
                                            }),
    ?assertEqual(416, ersip_sipmsg:status(BadMsg)),
    Reason = ersip_sipmsg:reason(BadMsg),
    ?assertEqual(<<"Unsupported URI Scheme">>, Reason),
    ok.

request_validation_no_resp_to_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: xyz"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ error, _ }, request_validation(raw_message(Msg))),
    ok.

request_validation_no_resp_from_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: xyz"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ error, _ }, request_validation(raw_message(Msg))),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

raw_message(Bin) ->
    P  = ersip_parser:new_dgram(Bin),
    { {ok, PMsg}, _P2 } = ersip_parser:parse(P),
    PMsg.

request_validation(RawMsg, Opts) ->
    ersip_proxy_common:request_validation(RawMsg, Opts#{ to_tag => { tag, <<"12345">> } }).

request_validation(RawMsg) ->
    request_validation(RawMsg, #{}).
    
