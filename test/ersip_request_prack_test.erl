%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% PRACK tests
%%%

-module(ersip_request_prack_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

parse_prack_test() ->
    R = ersip_sipmsg:parse(prack_bin(), all),
    ?assertMatch({ok, _}, R),
    {ok, SipMsg} = R,
    RAck = ersip_sipmsg:rack(SipMsg),
    ?assertEqual(ersip_method:prack(), ersip_sipmsg:method(SipMsg)),
    ?assertEqual(776655, ersip_hdr_rseq:value(ersip_hdr_rack:rseq(RAck))),
    ?assertEqual(1, ersip_hdr_cseq:number(ersip_hdr_rack:cseq(RAck))),
    ?assertEqual(ersip_method:invite(), ersip_hdr_cseq:method(ersip_hdr_rack:cseq(RAck))),
    ok.


%%===================================================================
%% Helpers
%%===================================================================

-define(crlf, "\r\n").

prack_bin() ->
    << "PRACK sip:watson@mypc.bell-tel.com SIP/2.0" ?crlf
       "RAck: 776655 1 INVITE" ?crlf
       "Via: SIP/2.0/UDP saturn.bell-tel.com" ?crlf
       "From: sip:alexander@bell-tel.com;tag=736ad7789" ?crlf
       "To: sip:watson@bell-tel.com;tag=11" ?crlf
       "Call-ID: 70710@saturn.bell-tel.com" ?crlf
       "CSeq: 2 PRACK" ?crlf
       "Content-Type: application/sdp" ?crlf
       "" ?crlf
       "v=0" ?crlf
       "s=Let's talk" ?crlf
       "b=CT:128" ?crlf
       "c=IN IP4 machine.bell-tel.com" ?crlf
       "m=audio 3456 RTP/AVP 5 0 7" ?crlf
       "m=video 2232 RTP/AVP 31" ?crlf
       "" ?crlf>>.
