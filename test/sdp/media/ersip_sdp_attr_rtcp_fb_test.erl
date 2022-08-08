%%
%% Copyright (c) 2020 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media attribute rtcp_fb
%%

-module(ersip_sdp_attr_rtcp_fb_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_test() ->
    RtcpFb0 = <<"* x-message app send:src,x-pli recv:src,x-pli">>,
    {ok, Rtpmap0, <<>>} = ersip_sdp_attr_rtcp_fb:parse(RtcpFb0),

    ?assertEqual('*', ersip_sdp_attr_rtcp_fb:type(Rtpmap0)),
    ?assertEqual(<<"x-message">>, ersip_sdp_attr_rtcp_fb:val(Rtpmap0)),
    ?assertEqual(app, ersip_sdp_attr_rtcp_fb:param(Rtpmap0)),
    ?assertEqual(<<"send:src,x-pli recv:src,x-pli">>, ersip_sdp_attr_rtcp_fb:bytestring(Rtpmap0)),

    RtcpFb0 = ersip_sdp_attr_rtcp_fb:assemble_bin(Rtpmap0),

    RtcpFb1 = <<"111 transport-cc">>,
    {ok, Rtpmap1, <<>>} = ersip_sdp_attr_rtcp_fb:parse(RtcpFb1),

    ?assertEqual(111, ersip_sdp_attr_rtcp_fb:type(Rtpmap1)),
    ?assertEqual(<<"transport-cc">>, ersip_sdp_attr_rtcp_fb:val(Rtpmap1)),

    RtcpFb1 = ersip_sdp_attr_rtcp_fb:assemble_bin(Rtpmap1),

    RtcpFb2 = <<"96 nack pli">>,
    {ok, Rtpmap2, <<>>} = ersip_sdp_attr_rtcp_fb:parse(RtcpFb2),

    ?assertEqual(96, ersip_sdp_attr_rtcp_fb:type(Rtpmap2)),
    ?assertEqual(nack, ersip_sdp_attr_rtcp_fb:val(Rtpmap2)),
    ?assertEqual(pli, ersip_sdp_attr_rtcp_fb:param(Rtpmap2)),
    ?assertEqual(undefined, ersip_sdp_attr_rtcp_fb:bytestring(Rtpmap2)),

    RtcpFb2 = ersip_sdp_attr_rtcp_fb:assemble_bin(Rtpmap2),

    RtcpFb3 = <<"96 nack">>,
    {ok, Rtpmap3, <<>>} = ersip_sdp_attr_rtcp_fb:parse(RtcpFb3),

    ?assertEqual(96, ersip_sdp_attr_rtcp_fb:type(Rtpmap3)),
    ?assertEqual(nack, ersip_sdp_attr_rtcp_fb:val(Rtpmap3)),

    RtcpFb3 = ersip_sdp_attr_rtcp_fb:assemble_bin(Rtpmap3),


    RtcpFb4 = <<"96 nack sli">>,
    {ok, Rtpmap4, <<>>} = ersip_sdp_attr_rtcp_fb:parse(RtcpFb4),

    ?assertEqual(96, ersip_sdp_attr_rtcp_fb:type(Rtpmap4)),
    ?assertEqual(nack, ersip_sdp_attr_rtcp_fb:val(Rtpmap4)),
    ?assertEqual(sli, ersip_sdp_attr_rtcp_fb:param(Rtpmap4)),
    ?assertEqual(undefined, ersip_sdp_attr_rtcp_fb:bytestring(Rtpmap4)),

    RtcpFb4 = ersip_sdp_attr_rtcp_fb:assemble_bin(Rtpmap4),

    RtcpFb5 = <<"96 tok-en_1 token2 abc">>,
    {ok, Rtpmap5, <<>>} = ersip_sdp_attr_rtcp_fb:parse(RtcpFb5),

    ?assertEqual(96, ersip_sdp_attr_rtcp_fb:type(Rtpmap5)),
    ?assertEqual(<<"tok-en_1">>, ersip_sdp_attr_rtcp_fb:val(Rtpmap5)),
    ?assertEqual(<<"token2">>, ersip_sdp_attr_rtcp_fb:param(Rtpmap5)),
    ?assertEqual(<<"abc">>, ersip_sdp_attr_rtcp_fb:bytestring(Rtpmap5)),

    RtcpFb5 = ersip_sdp_attr_rtcp_fb:assemble_bin(Rtpmap5),

    ok.

parse_error_test() ->
    ?assertEqual(
        {error,{invalid_rtcp_fb,{invalid_integer,<<"asd">>}}},
        ersip_sdp_attr_rtcp_fb:parse(<<"* trr-int asd">>)
    ),
    ?assertEqual(
        {error,{invalid_rtcp_fb,{unexpected_input,<<"adfsdf">>}}},
        ersip_sdp_attr_rtcp_fb:parse(<<"96 nack pli adfsdf">>)
    ),
    ?assertEqual(
        {error,{invalid_rtcp_fb,{unexpected_input,<<"sli">>}}},
        ersip_sdp_attr_rtcp_fb:parse(<<"96 ack sli adfsdf">>)
    ),
    ?assertEqual(
        {error,{invalid_rtcp_fb,{unexpected_input,<<"adfsdf">>}}},
        ersip_sdp_attr_rtcp_fb:parse(<<"96 nack sli adfsdf">>)
    ),
    ?assertEqual(
        {error,{invalid_rtcp_fb,{invalid_id_char,<<"+">>}}},
        ersip_sdp_attr_rtcp_fb:parse(<<"96 tok+en_1 token2 ab">>)
    ),
    ok.


