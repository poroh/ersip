%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP address
%%

-module(ersip_sdp_addr_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_ip4_address_test() ->
    ?assertEqual({ok, {ip4, {127, 0, 0, 1}}},  parse_in_ip4(<<"127.0.0.1">>)),
    ?assertEqual({ok, {ip4, {10, 47, 16, 5}}}, parse_in_ip4(<<"10.47.16.5">>)),
    ?assertEqual({ok, {ip4_host, <<"pc33.atlanta.com">>}}, parse_in_ip4(<<"pc33.atlanta.com">>)),
    ok.

parse_ip6_address_test() ->
    ?assertEqual({ok, {ip6, {16#ff15, 0, 0,      0,
                             0,       0, 0, 16#101}}},  parse_in_ip6(<<"FF15::101">>)),
    Bin1 = <<"2001:2345:6789:ABCD:EF01:2345:6789:ABCD">>,
    IP6  = {16#2001, 16#2345, 16#6789, 16#ABCD,
            16#EF01, 16#2345, 16#6789, 16#ABCD},
    ?assertEqual({ok, {ip6, IP6}},  parse_in_ip6(Bin1)),
    ?assertEqual({ok, {ip6_host, <<"pc33.atlanta.com">>}}, parse_in_ip6(<<"pc33.atlanta.com">>)),
    ok.

parse_error_ip4_test() ->
    ?assertMatch({error, _}, parse_in_ip4(<<"256.0.0.1">>)),
    ?assertMatch({error, _}, parse_in_ip4(<<"1.0.0.256">>)),
    ?assertMatch({error, _}, parse_in_ip4(<<".">>)),
    ?assertMatch({error, _}, parse_in_ip4(<<"abc.1com.">>)),
    ok.

parse_error_ip6_test() ->
    ?assertMatch({error, _}, parse_in_ip6(<<"2001::GGGG">>)),
    ?assertMatch({error, _}, parse_in_ip6(<<"2001::10000">>)),
    ?assertMatch({error, _}, parse_in_ip6(<<".">>)),
    ?assertMatch({error, _}, parse_in_ip6(<<"abc.1com.">>)),
    ok.

parse_unknown_addr_type_test() ->
    ?assertEqual({ok, {unknown_addr, <<"ATM">>,
                       <<"NSAP">>, <<"47.0091.8100.0000.0060.3E64.FD01.0060.3E64.FD01.00">>}},
                 ersip_sdp_addr:parse(<<"ATM">>, <<"NSAP">>, <<"47.0091.8100.0000.0060.3E64.FD01.0060.3E64.FD01.00">>)),
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================

parse_in_ip4(Bin) ->
    ersip_sdp_addr:parse(<<"in">>, <<"ip4">>, Bin).

parse_in_ip6(Bin) ->
    ersip_sdp_addr:parse(<<"in">>, <<"ip6">>, Bin).
