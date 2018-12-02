%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP origin (o=) test
%%

-module(ersip_sdp_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

parse_rfc4566_example_test() ->
    SDPBin
        = <<"v=0" ?crlf
            "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5" ?crlf
            "s=SDP Seminar" ?crlf
            "i=A Seminar on the session description protocol" ?crlf
            "u=http://www.example.com/seminars/sdp.pdf" ?crlf
            "e=j.doe@example.com (Jane Doe)" ?crlf
            "c=IN IP4 224.2.17.12/127" ?crlf
            "t=2873397496 2873404696" ?crlf
            "a=recvonly" ?crlf
            "m=audio 49170 RTP/AVP 0" ?crlf
            "m=video 51372 RTP/AVP 99" ?crlf
            "a=rtpmap:99 h263-1998/90000" ?crlf
            "">>,
    Result = ersip_sdp:parse(SDPBin),
    ?assertMatch({ok, _}, Result),
    {ok, SDP} = Result,
    Origin = ersip_sdp:origin(SDP),
    ?assertEqual(<<"jdoe">>, ersip_sdp_origin:username(Origin)),
    ?assertEqual(2890844526, ersip_sdp_origin:session_id(Origin)),
    ?assertEqual(2890842807, ersip_sdp_origin:session_version(Origin)),
    ?assertEqual(<<"SDP Seminar">>, ersip_sdp:session_name(SDP)),
    ?assertEqual(<<"A Seminar on the session description protocol">>, ersip_sdp:info(SDP)),
    ?assertEqual(<<"http://www.example.com/seminars/sdp.pdf">>, ersip_sdp:uri(SDP)),
    ?assertEqual([<<"j.doe@example.com (Jane Doe)">>], ersip_sdp:emails(SDP)),
    SessConn = ersip_sdp:conn(SDP),
    SessAddr = ersip_sdp_conn:addr(SessConn),
    ?assertEqual(ersip_sdp_addr:make(<<"IN IP4 224.2.17.12">>), SessAddr),
    ?assertEqual(127, ersip_sdp_conn:ttl(SessConn)),
    ?assertEqual(1,   ersip_sdp_conn:num_addrs(SessConn)),
    ok.
