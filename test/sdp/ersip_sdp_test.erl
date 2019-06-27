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
    ?assertEqual([<<"recvonly">>],   ersip_sdp:attrs(SDP)),
    ?assertMatch([_, _], ersip_sdp:medias(SDP)),
    [MAudio, MVideo] = ersip_sdp:medias(SDP),
    ?assertEqual(<<"audio">>, ersip_sdp_media:type(MAudio)),
    ?assertEqual(49170,       ersip_sdp_media:port(MAudio)),
    ?assertEqual(undefined,   ersip_sdp_media:conn(MAudio)),
    ?assertEqual(<<"video">>, ersip_sdp_media:type(MVideo)),
    ?assertEqual(51372,       ersip_sdp_media:port(MVideo)),
    ?assertEqual([{<<"rtpmap">>, <<"99 h263-1998/90000">>}],       ersip_sdp_media:attrs(MVideo)),
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

set_attrs_test() ->
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
    ?assertEqual([], ersip_sdp:attrs(SDP)),
    {ok, Attrs, <<>>} = ersip_sdp_attr:parse(<<"a=group:BUNDLE audio video", ?crlf>>),
    SDP1 = ersip_sdp:set_attrs(Attrs, SDP),
    ExpectedSDP
        = <<"v=0" ?crlf
            "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5" ?crlf
            "s=SDP Seminar" ?crlf
            "t=0 0" ?crlf
            "a=group:BUNDLE audio video", ?crlf
            "m=audio 49170 RTP/AVP 0" ?crlf
            "">>,
    ?assertEqual(ExpectedSDP, ersip_sdp:assemble_bin(SDP1)),
    ?assertEqual(Attrs, ersip_sdp:attrs(SDP1)),
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
    {ok, [NewMedia], <<>>} = ersip_sdp_media:parse(<<"m=video 49172 RTP/AVP 0" ?crlf>>),
    {ok, Attrs, <<>>} = ersip_sdp_attr:parse(<<"a=rtpmap:111 opus/48000/2", ?crlf>>),
    NewMedia1 = ersip_sdp_media:set_attrs(Attrs, NewMedia),
    NewSDP = ersip_sdp:set_medias([NewMedia1], SDP),
    NewSDPBin = ersip_sdp:assemble_bin(NewSDP),
    ExpectedSDP
        = <<"v=0" ?crlf
            "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5" ?crlf
            "s=SDP Seminar" ?crlf
            "t=0 0" ?crlf
            "m=video 49172 RTP/AVP 0" ?crlf
            "a=rtpmap:111 opus/48000/2" ?crlf
            "">>,
    ?assertEqual(ExpectedSDP, NewSDPBin),
    ok.

bad_test() ->
    ?assertMatch({error, {invalid_sdp, <<"f=invalid:yes\r\n">>}}, ersip_sdp:parse(bad1())),
    ?assertMatch({error, {invalid_sdp, <<"c=IN IP4 192.0.2.156\r\n", _/binary>>}}, ersip_sdp:parse(bad2())),
    ?assertMatch({error, {invalid_sdp, {unsupported_version,<<"1">>}}}, ersip_sdp:parse(bad3())),
    ?assertMatch({error, {invalid_sdp, {unexpected_attribute_error, {session_name,<<"t=0 0">>}}}}, ersip_sdp:parse(bad4())),
    ?assertMatch({error, {invalid_sdp, {unexpected_attribute_error, {version, <<"o=", _/binary>>}}}}, ersip_sdp:parse(bad5())),
    ok.

bad_line_start_test() ->
    OrigLines = sdp_lines(),
    NumLines = length(OrigLines),
    Cases = [begin
                 {Head, [X | Tail]} = lists:split(SplitPos, OrigLines),
                 assemble_lines(Head ++ [[$% | X] | Tail])
             end
             || SplitPos <- lists:seq(0, NumLines-1)],
    [?assertMatch({error, {invalid_sdp, _}},
                  ersip_sdp:parse(L))
     || L <- Cases].


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


bad1() -> %%  unknown f=
    <<"v=0\r\n",
      "o=- 3710604898417546434 2 IN IP4 127.0.0.1\r\n",
      "s=-\r\n",
      "t=0 0\r\n",
      "m=audio 1 RTP/AVP 0\r\n",
      "c=IN IP4 0.0.0.0\r\n",
      "a=rtcp:1 IN IP7 X\r\n",
      "a=rtpmap:0 PCMU/8000\r\n",
      "a=goo:hithere\r\n",
      "f=invalid:yes\r\n">>.

bad2() -> %% bad position of c=
    <<"v=0\r\n",
      "o=alice 2362969037 2362969040 IN IP4 192.0.2.156\r\n",
      "s=Simulcast Enabled Client\r\n",
      "t=0 0\r\n",
      "c=IN IP4 192.0.2.156\r\n",
      "m=audio 49200 RTP/AVP 0\r\n",
      "a=rtpmap:0 PCMU/8000\r\n">>.

bad3() -> %% unsupported version
    <<"v=1\r\n",
      "o=alice 2362969037 2362969040 IN IP4 192.0.2.156\r\n",
      "s=Simulcast Enabled Client\r\n",
      "t=0 0\r\n",
      "m=audio 49200 RTP/AVP 0\r\n",
      "a=rtpmap:0 PCMU/8000\r\n">>.

bad4() -> %% no session
    <<"v=0\r\n",
      "o=alice 2362969037 2362969040 IN IP4 192.0.2.156\r\n",
      "t=0 0\r\n",
      "m=audio 49200 RTP/AVP 0\r\n",
      "a=rtpmap:0 PCMU/8000\r\n">>.

bad5() -> %%  no version
    <<"o=alice 2362969037 2362969040 IN IP4 192.0.2.156\r\n",
      "v=0\r\n",
      "s=Simulcast Enabled Client\r\n",
      "t=0 0\r\n",
      "m=audio 49200 RTP/AVP 0\r\n",
      "a=rtpmap:0 PCMU/8000\r\n">>.

sdp_lines() ->
    ["v=0",
     "o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5",
     "s=SDP Seminar",
     "i=A Seminar on the session description protocol",
     "u=http://www.example.com/seminars/sdp.pdf",
     "e=j.doe@example.com (Jane Doe)",
     "p=+1 617 555-6011",
     "c=IN IP4 224.2.17.12/127",
     "b=TIAS:512",
     "t=0 0",
     "t=3034423619 3042462419",
     "r=604800 3600 0 90000",
     "r=7d 1h 0 25h",
     "k=prompt",
     "a=recvonly",
     "m=audio 49170 RTP/AVP 0",
     "m=video 51372 RTP/AVP 99",
     "c=IN IP4 192.168.0.1",
     "a=rtpmap:99 h263-1998/90000"].

assemble_lines(Lines) ->
    iolist_to_binary([[L, ?crlf] || L <- Lines]).
