%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP test
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
            "c=IN IP4 192.168.0.1" ?crlf
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
    ?assertMatch([_, _], ersip_sdp:medias(SDP)),
    [MAudio, MVideo] = ersip_sdp:medias(SDP),
    ?assertEqual(<<"audio">>, ersip_sdp_media:type(MAudio)),
    ?assertEqual(49170,       ersip_sdp_media:port(MAudio)),
    ?assertEqual(undefined,   ersip_sdp_media:conn(MAudio)),
    ?assertEqual(<<"video">>, ersip_sdp_media:type(MVideo)),
    ?assertEqual(51372,       ersip_sdp_media:port(MVideo)),
    VideoConn = ersip_sdp_media:conn(MVideo),
    ?assertEqual(ersip_sdp_addr:make(<<"IN IP4 192.168.0.1">>), ersip_sdp_conn:addr(VideoConn)),
    ok.

rebuild_test() ->
    SDPBin1
        = <<"v=0" ?crlf
            "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5" ?crlf
            "s=SDP Seminar" ?crlf
            "i=A Seminar on the session description protocol" ?crlf
            "u=http://www.example.com/seminars/sdp.pdf" ?crlf
            "e=j.doe@example.com (Jane Doe)" ?crlf
            "c=IN IP4 224.2.17.12/127" ?crlf
            "b=TIAS:512" ?crlf
            "t=0 0" ?crlf
            "t=3034423619 3042462419" ?crlf
            "r=604800 3600 0 90000" ?crlf
            "r=7d 1h 0 25h" ?crlf
            "z=2882844526 -1h 2898848070 0" ?crlf
            "a=recvonly" ?crlf
            "m=audio 49170 RTP/AVP 0" ?crlf
            "b=TIAS:64" ?crlf
            "m=video 51372 RTP/AVP 99" ?crlf
            "c=IN IP4 192.168.0.1" ?crlf
            "a=rtpmap:99 h263-1998/90000" ?crlf
            "">>,
    ?assertEqual(SDPBin1, reassemble(SDPBin1)),
    %% No Zone
    %% More than one addr
    %% No URI
    %% Phone number
    %% More than one port
    SDPBin2
        = <<"v=0" ?crlf
            "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5" ?crlf
            "s=SDP Seminar" ?crlf
            "i=A Seminar on the session description protocol" ?crlf
            "e=j.doe@example.com (Jane Doe)" ?crlf
            "p=+1 617 555-6011" ?crlf
            "c=IN IP4 224.2.17.12/127/2" ?crlf
            "b=TIAS:512" ?crlf
            "t=0 0" ?crlf
            "t=3034423619 3042462419" ?crlf
            "r=604800 3600 0 90000" ?crlf
            "r=7d 1h 0 25h" ?crlf
            "k=prompt" ?crlf
            "a=recvonly" ?crlf
            "m=audio 49170 RTP/AVP 0" ?crlf
            "b=TIAS:64" ?crlf
            "k=prompt" ?crlf
            "m=video 51372/2 RTP/AVP 99" ?crlf
            "c=IN IP4 192.168.0.1" ?crlf
            "a=rtpmap:99 h263-1998/90000" ?crlf
            "">>,
    ?assertEqual(SDPBin2, reassemble(SDPBin2)),
    ok.

set_conn_test() ->
    SDPBin
        = <<"v=0" ?crlf
            "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5" ?crlf
            "s=SDP Seminar" ?crlf
            "t=0 0" ?crlf
            "m=audio 49170 RTP/AVP 0" ?crlf
            "">>,
    Result = ersip_sdp:parse(SDPBin),
    ?assertMatch({ok, _}, Result),
    {ok, SDP} = Result,
    ?assertEqual(undefined, ersip_sdp:conn(SDP)),
    {ok, Conn, <<>>} = ersip_sdp_conn:parse(<<"c=IN IP4 224.2.17.12/127/2" ?crlf>>),
    SDP1 = ersip_sdp:set_conn(Conn, SDP),
    ExpectedSDP
        = <<"v=0" ?crlf
            "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5" ?crlf
            "s=SDP Seminar" ?crlf
            "c=IN IP4 224.2.17.12/127/2" ?crlf
            "t=0 0" ?crlf
            "m=audio 49170 RTP/AVP 0" ?crlf
            "">>,
    ?assertEqual(ExpectedSDP, ersip_sdp:assemble_bin(SDP1)),
    ?assertEqual(Conn, ersip_sdp:conn(SDP1)),
    ok.

set_media_test() ->
    SDPBin
        = <<"v=0" ?crlf
            "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5" ?crlf
            "s=SDP Seminar" ?crlf
            "t=0 0" ?crlf
            "m=audio 49170 RTP/AVP 0" ?crlf
            "">>,
    SDP = make(SDPBin),
    {ok, NewMedias, <<>>} = ersip_sdp_media:parse(<<"m=video 49172 RTP/AVP 0" ?crlf>>),
    NewSDP = ersip_sdp:set_medias(NewMedias, SDP),
    NewSDPBin = ersip_sdp:assemble_bin(NewSDP),
    ExpectedSDP
        = <<"v=0" ?crlf
            "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5" ?crlf
            "s=SDP Seminar" ?crlf
            "t=0 0" ?crlf
            "m=video 49172 RTP/AVP 0" ?crlf
            "">>,
    ?assertEqual(ExpectedSDP, NewSDPBin),
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================

reassemble(SDPBin) ->
    {ok, SDP0} = ersip_sdp:parse(SDPBin),
    SDPBin1 = ersip_sdp:assemble_bin(SDP0),
    {ok, SDP1} = ersip_sdp:parse(SDPBin1),
    ?assertEqual(SDP1, SDP0),
    SDPBin1.

make(SDPBin) ->
    Result = ersip_sdp:parse(SDPBin),
    ?assertMatch({ok, _}, Result),
    {ok, SDP} = Result,
    SDP.
