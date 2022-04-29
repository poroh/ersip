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
    {ok, Rtpmap0, _} = ersip_sdp_attr_rtpmap:parse(<<"99 AMR-WB/16000">>),
    ?assertEqual(<<"99">>,          ersip_sdp_attr_rtpmap:payload_type(Rtpmap0)),
    ?assertEqual(<<"AMR-WB">>,      ersip_sdp_attr_rtpmap:encoding_name(Rtpmap0)),
    ?assertEqual(16000,             ersip_sdp_attr_rtpmap:clock_rate(Rtpmap0)),
    ?assertEqual(undefined,         ersip_sdp_attr_rtpmap:encoding_params(Rtpmap0)),

    {ok, Rtpmap1, _} = ersip_sdp_attr_rtpmap:parse(<<"100 H264/90000/2">>),
    ?assertEqual(<<"100">>,         ersip_sdp_attr_rtpmap:payload_type(Rtpmap1)),
    ?assertEqual(<<"H264">>,        ersip_sdp_attr_rtpmap:encoding_name(Rtpmap1)),
    ?assertEqual(90000,             ersip_sdp_attr_rtpmap:clock_rate(Rtpmap1)),
    ?assertEqual(2,                 ersip_sdp_attr_rtpmap:encoding_params(Rtpmap1)),

    {ok, Rtpmap2, _} = ersip_sdp_attr_rtpmap:parse(<<"101 telephone-event/8000">>),
    ?assertEqual(<<"101">>,             ersip_sdp_attr_rtpmap:payload_type(Rtpmap2)),
    ?assertEqual(<<"telephone-event">>, ersip_sdp_attr_rtpmap:encoding_name(Rtpmap2)),
    ?assertEqual(8000,                  ersip_sdp_attr_rtpmap:clock_rate(Rtpmap2)),
    ?assertEqual(undefined,             ersip_sdp_attr_rtpmap:encoding_params(Rtpmap2)),
    ok.

parse_error_test() ->
    ?assertEqual({error, {invalid_rtpmap_candidate, invalid_clock_rate}},  ersip_sdp_attr_rtpmap:parse(<<"99 AMR-WB/16000A">>)),
    ?assertEqual({error, {invalid_rtpmap_candidate, invalid_payload_type}},  ersip_sdp_attr_rtpmap:parse(<<" AMR-WB/16000">>)),
    ?assertEqual({error, {invalid_rtpmap_candidate, invalid_encoding_params}},  ersip_sdp_attr_rtpmap:parse(<<"0 AMR-WB/16000/A">>)),
    ok.


