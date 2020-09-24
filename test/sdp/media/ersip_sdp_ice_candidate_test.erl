%%
%% Copyright (c) 2020 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media ice layers // ice candidate
%%

-module(ersip_sdp_ice_candidate_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

parse_test() ->
    {ok, IceCandidate0} = parse(<<"1 1 UDP 1694498815 192.0.2.3 45664 typ srflx raddr 10.0.1.1 rport 8998">>),
    ?assertEqual(<<"1">>,               ersip_sdp_ice_candidate:foundation(IceCandidate0)),
    ?assertEqual(1,                     ersip_sdp_ice_candidate:component_id(IceCandidate0)),
    ?assertEqual(<<"UDP">>,             ersip_sdp_ice_candidate:transport(IceCandidate0)),
    ?assertEqual(1694498815,            ersip_sdp_ice_candidate:priority(IceCandidate0)),
    ?assertEqual({ip4, {192, 0, 2, 3}}, ersip_sdp_ice_candidate:connection_address(IceCandidate0)),
    ?assertEqual(45664,                 ersip_sdp_ice_candidate:port(IceCandidate0)),
    ?assertEqual(<<"srflx">>,           ersip_sdp_ice_candidate:cand_type(IceCandidate0)),
    ?assertEqual({ip4, {10, 0, 1, 1}},  ersip_sdp_ice_candidate:rel_addr(IceCandidate0)),
    ?assertEqual(8998,                  ersip_sdp_ice_candidate:rel_port(IceCandidate0)),
    ?assertEqual([],                    ersip_sdp_ice_candidate:attrs(IceCandidate0)),

    {ok, IceCandidate1} = parse(<<"1 2 UDP 2130706431 fe80::6676:baff:fe9c:ee4a 8998 typ host">>),
    ?assertEqual(<<"1">>,                                             ersip_sdp_ice_candidate:foundation(IceCandidate1)),
    ?assertEqual(2,                                                   ersip_sdp_ice_candidate:component_id(IceCandidate1)),
    ?assertEqual(<<"UDP">>,                                           ersip_sdp_ice_candidate:transport(IceCandidate1)),
    ?assertEqual(2130706431,                                          ersip_sdp_ice_candidate:priority(IceCandidate1)),
    ?assertEqual({ip6, {65152, 0, 0, 0, 26230, 47871, 65180, 61002}}, ersip_sdp_ice_candidate:connection_address(IceCandidate1)),
    ?assertEqual(8998,                                                ersip_sdp_ice_candidate:port(IceCandidate1)),
    ?assertEqual(<<"host">>,                                          ersip_sdp_ice_candidate:cand_type(IceCandidate1)),
    ?assertEqual([],                                                  ersip_sdp_ice_candidate:attrs(IceCandidate1)),

    {ok, IceCandidate2} = parse(<<"2 1 UDP 1694498815 2001:420:c0e0:1005::61 45664 typ srflx raddr fe80::6676:baff:fe9c:ee4a rport 8998 generation 0 time 12">>),
    ?assertEqual(<<"2">>,                                               ersip_sdp_ice_candidate:foundation(IceCandidate2)),
    ?assertEqual(1,                                                     ersip_sdp_ice_candidate:component_id(IceCandidate2)),
    ?assertEqual(<<"UDP">>,                                             ersip_sdp_ice_candidate:transport(IceCandidate2)),
    ?assertEqual(1694498815,                                            ersip_sdp_ice_candidate:priority(IceCandidate2)),
    ?assertEqual({ip6, {8193, 1056, 49376, 4101, 0, 0, 0, 97}},         ersip_sdp_ice_candidate:connection_address(IceCandidate2)),
    ?assertEqual(45664,                                                 ersip_sdp_ice_candidate:port(IceCandidate2)),
    ?assertEqual(<<"srflx">>,                                           ersip_sdp_ice_candidate:cand_type(IceCandidate2)),
    ?assertEqual({ip6, {65152, 0, 0, 0, 26230, 47871, 65180, 61002}},   ersip_sdp_ice_candidate:rel_addr(IceCandidate2)),
    ?assertEqual(8998,                                                  ersip_sdp_ice_candidate:rel_port(IceCandidate2)),
    ?assertEqual([{<<"generation">>, <<"0">>}, {<<"time">>, <<"12">>}], ersip_sdp_ice_candidate:attrs(IceCandidate2)),

    {ok, IceCandidate3} = parse(<<"1467250027 1 udp 2122260223 192.168.0.196 46243 typ host generation 0">>),
    ?assertEqual(<<"1467250027">>,              ersip_sdp_ice_candidate:foundation(IceCandidate3)),
    ?assertEqual(1,                             ersip_sdp_ice_candidate:component_id(IceCandidate3)),
    ?assertEqual(<<"udp">>,                     ersip_sdp_ice_candidate:transport(IceCandidate3)),
    ?assertEqual(2122260223,                    ersip_sdp_ice_candidate:priority(IceCandidate3)),
    ?assertEqual({ip4, {192, 168, 0, 196}},     ersip_sdp_ice_candidate:connection_address(IceCandidate3)),
    ?assertEqual(46243,                         ersip_sdp_ice_candidate:port(IceCandidate3)),
    ?assertEqual(<<"host">>,                    ersip_sdp_ice_candidate:cand_type(IceCandidate3)),
    ?assertEqual(undefined,                     ersip_sdp_ice_candidate:rel_addr(IceCandidate3)),
    ?assertEqual(undefined,                     ersip_sdp_ice_candidate:rel_port(IceCandidate3)),
    ?assertEqual([{<<"generation">>, <<"0">>}], ersip_sdp_ice_candidate:attrs(IceCandidate3)),

    ok.

parse_error_test() ->
    ?assertMatch({error, {invalid_ice_candidate, _}}, parse(<<"2 1 UDP 1694498815 2001:420:c0e0:1005::61">>)),
    ?assertMatch({error, {invalid_ice_candidate, _}}, parse(<<"2 1 UDP 1694498815 2001:420:c0e0:1005::61 45664 typ srflx raddr fe80::6676:baff:fe9c:ee4a key_no_value">>)),
    ?assertMatch({error, {invalid_ice_candidate, _}}, parse(<<"2 1 UDP 0 2001:420:c0e0:1005::61 45664 typ srflx">>)),

    ok.

assemble_test() ->
    IceCandidateBin = <<"2 1 UDP 1694498815 2001:420:c0e0:1005::61 45664 typ srflx raddr fe80::6676:baff:fe9c:ee4a rport 8998 generation 0 time 12">>,
    ?assertEqual(ersip_bin:to_lower(IceCandidateBin), ersip_bin:to_lower(reassemble(IceCandidateBin))),

    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

parse(Bin) ->
    case ersip_sdp_ice_candidate:parse(Bin) of
        {ok, IceCandidate, <<>>} ->
            {ok, IceCandidate};
        {ok, IceCandidate, <<?crlf>>} ->
            {ok, IceCandidate};
        {error, _} = Error ->
            Error
    end.

reassemble(Bin) ->
    {ok, IceCandidate} = parse(Bin),
    ersip_sdp_ice_candidate:assemble_bin(IceCandidate).

