%%
%% Copyright (c) 2020 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media attribute rtpmap
%%

-module(ersip_sdp_attr_rtpmap_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_test() ->
    RtpmapBin0 = <<"99 AMR-WB/16000">>,
    {ok, Rtpmap0} = ersip_sdp_attr_rtpmap:parse(RtpmapBin0),
    ?assertEqual(99,                ersip_sdp_attr_rtpmap:payload_type(Rtpmap0)),
    ?assertEqual(<<"AMR-WB">>,      ersip_sdp_attr_rtpmap:encoding_name(Rtpmap0)),
    ?assertEqual(16000,             ersip_sdp_attr_rtpmap:clock_rate(Rtpmap0)),
    ?assertEqual(undefined,         ersip_sdp_attr_rtpmap:encoding_params(Rtpmap0)),
    ?assertEqual(RtpmapBin0,        ersip_sdp_attr_rtpmap:assemble_bin(Rtpmap0)),

    RtpmapBin1 = <<"100 H264/90000/2">>,
    {ok, Rtpmap1} = ersip_sdp_attr_rtpmap:parse(RtpmapBin1),
    ?assertEqual(100,               ersip_sdp_attr_rtpmap:payload_type(Rtpmap1)),
    ?assertEqual(<<"H264">>,        ersip_sdp_attr_rtpmap:encoding_name(Rtpmap1)),
    ?assertEqual(90000,             ersip_sdp_attr_rtpmap:clock_rate(Rtpmap1)),
    ?assertEqual(2,                 ersip_sdp_attr_rtpmap:encoding_params(Rtpmap1)),
    ?assertEqual(RtpmapBin1,        ersip_sdp_attr_rtpmap:assemble_bin(Rtpmap1)),

    {ok, Rtpmap2} = ersip_sdp_attr_rtpmap:parse(<<"101 telephone-event/8000">>),
    ?assertEqual(101,                   ersip_sdp_attr_rtpmap:payload_type(Rtpmap2)),
    ?assertEqual(<<"telephone-event">>, ersip_sdp_attr_rtpmap:encoding_name(Rtpmap2)),
    ?assertEqual(8000,                  ersip_sdp_attr_rtpmap:clock_rate(Rtpmap2)),
    ?assertEqual(undefined,             ersip_sdp_attr_rtpmap:encoding_params(Rtpmap2)),

    ok.

parse_error_test() ->
    ?assertEqual(
        {error, {invalid_rtpmap, {no_separator, $/, <<"A">>}}},
        ersip_sdp_attr_rtpmap:parse(<<"99 AMR-WB/16000A">>)
    ),
    ?assertEqual(
        {error, {invalid_rtpmap, {invalid_integer, <<" AMR-WB/16000">>}}},
        ersip_sdp_attr_rtpmap:parse(<<" AMR-WB/16000">>)
    ),
    ?assertEqual(
        {error, {invalid_rtpmap, {invalid_integer, <<"A">>}}},
        ersip_sdp_attr_rtpmap:parse(<<"0 AMR-WB/16000/A">>)
    ),
    ?assertEqual(
        {error, {invalid_rtpmap, {no_separator, $/, <<" Garbage">>}}},
        ersip_sdp_attr_rtpmap:parse(<<"99 AMR-WB/16000 Garbage">>)
    ),
    ?assertEqual(
        {error, {invalid_rtpmap, {invalid_encoding_params, <<" Garbage">>}}},
        ersip_sdp_attr_rtpmap:parse(<<"99 AMR-WB/16000/2 Garbage">>)
    ),
    ok.


