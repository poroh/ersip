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

parse_test() ->
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
    P  = ersip_parser:new_dgram(Msg),
    { {ok, PMsg}, _P2 } = ersip_parser:parse(P),
    { ok, SipMsg } = ersip_sipmsg:parse(PMsg, all),
    ?assertEqual(ersip_hdr_callid:make(CallId),        ersip_sipmsg:get(callid, SipMsg)),
    ?assertEqual(ersip_hdr_maxforwards:make(<<"70">>), ersip_sipmsg:get(maxforwards, SipMsg)),
    ?debugFmt("~p", [ ersip_sipmsg:get(from, SipMsg) ]).

