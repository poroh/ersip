%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP address
%%

-module(ersip_sdp_conn_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_ip4_multicast_test() ->
    %% for example:
    %%
    %%    c=IN IP4 224.2.1.1/127/3
    %%
    %% would state that addresses 224.2.1.1, 224.2.1.2, and 224.2.1.3 are to
    %% be used at a TTL of 127.  This is semantically identical to including
    %% multiple "c=" lines in a media description:
    %%
    %%    c=IN IP4 224.2.1.1/127
    %%    c=IN IP4 224.2.1.2/127
    %%    c=IN IP4 224.2.1.3/127
    {ok, Conn1} = parse(<<"c=IN IP4 224.2.1.1/127/3">>), %% RFC 4566
    ?assertEqual({ip4, {224, 2, 1, 1}}, ersip_sdp_conn:addr(Conn1)),
    ?assertEqual(127, ersip_sdp_conn:ttl(Conn1)),
    ?assertEqual(3,   ersip_sdp_conn:num_addrs(Conn1)),

    {ok, Conn2} = parse(<<"c=IN IP4 224.2.1.1/127">>), %% RFC 4566
    ?assertEqual({ip4, {224, 2, 1, 1}}, ersip_sdp_conn:addr(Conn2)),
    ?assertEqual(127, ersip_sdp_conn:ttl(Conn2)),
    ?assertEqual(1,   ersip_sdp_conn:num_addrs(Conn2)),
    ok.

parse_ip4_unicast_test() ->
    {ok, Conn2} = parse(<<"c=IN IP4 127.0.0.1">>),
    ?assertEqual({ip4, {127, 0, 0, 1}}, ersip_sdp_conn:addr(Conn2)),
    ?assertEqual(undefined, ersip_sdp_conn:ttl(Conn2)),
    ?assertEqual(1, ersip_sdp_conn:num_addrs(Conn2)),
    ok.

parse_ip6_multicast_test() ->
    %% Similarly, an IPv6 example would be:
    %%
    %%    c=IN IP6 FF15::101/3
    %%
    %% which is semantically equivalent to:
    %%
    %%    c=IN IP6 FF15::101
    %%    c=IN IP6 FF15::102
    %%    c=IN IP6 FF15::103
    {ok, Conn3} = parse(<<"c=IN IP6 FF15::101/3">>),
    ?assertEqual({ip6, {16#FF15, 0, 0, 0, 0, 0, 0, 16#101}}, ersip_sdp_conn:addr(Conn3)),
    ?assertEqual(undefined, ersip_sdp_conn:ttl(Conn3)),
    ?assertEqual(3, ersip_sdp_conn:num_addrs(Conn3)),
    ok.

parse_ip6_unicast_test() ->
    {ok, Conn3} = parse(<<"c=IN IP6 ::1">>),
    ?assertEqual({ip6, {0, 0, 0, 0, 0, 0, 0, 1}}, ersip_sdp_conn:addr(Conn3)),
    ?assertEqual(undefined, ersip_sdp_conn:ttl(Conn3)),
    ?assertEqual(1, ersip_sdp_conn:num_addrs(Conn3)),
    ok.

parse_ip4_host_test() ->
    {ok, Conn} = parse(<<"c=IN IP4 pc33.atlanta.com">>),
    ?assertEqual({ip4_host, <<"pc33.atlanta.com">>}, ersip_sdp_conn:addr(Conn)),
    ?assertEqual(undefined, ersip_sdp_conn:ttl(Conn)),
    ?assertEqual(1, ersip_sdp_conn:num_addrs(Conn)),
    ok.

parse_ip6_host_test() ->
    {ok, Conn} = parse(<<"c=IN IP6 pc33.atlanta.com">>),
    ?assertEqual({ip6_host, <<"pc33.atlanta.com">>}, ersip_sdp_conn:addr(Conn)),
    ?assertEqual(undefined, ersip_sdp_conn:ttl(Conn)),
    ?assertEqual(1, ersip_sdp_conn:num_addrs(Conn)),
    ok.

parse_unknown_addr_test() ->
    ATMAddr = <<"ATM NSAP 47.0091.8100.0000.0060.3E64.FD01.0060.3E64.FD01.00">>,
    {ok, Conn} = parse(<<"c=", ATMAddr/binary>>),
    ?assertMatch({unknown_addr, <<"ATM">>, _, _}, ersip_sdp_conn:addr(Conn)),
    ?assertEqual(undefined, ersip_sdp_conn:ttl(Conn)),
    ?assertEqual(1, ersip_sdp_conn:num_addrs(Conn)),
    ok.

parse_error_test() ->
    ?assertMatch({error, {invalid_conn, {invalid_ttl, _}}}, parse(<<"c=IN IP4 224.2.1.1/-1">>)),
    ?assertMatch({error, {invalid_conn, {invalid_ttl, _}}}, parse(<<"c=IN IP4 224.2.1.1/256">>)),
    ?assertMatch({error, {invalid_conn, {invalid_ttl, _}}}, parse(<<"c=IN IP4 224.2.1.1/abcd">>)),
    ?assertMatch({error, {invalid_conn, {invalid_num_addrs, _}}}, parse(<<"c=IN IP4 224.2.1.1/127/abcd">>)),
    ?assertMatch({error, {invalid_conn, ip4_multicast_ttl_expected}}, parse(<<"c=IN IP4 224.2.1.1">>)),
    ?assertMatch({error, {invalid_conn, _}}, parse(<<"c=IN IP4 pc33.atlanta.com ">>)),
    ?assertMatch({error, {invalid_conn, {invalid_num_addrs, _}}}, parse(<<"c=IN IP6 FF00::110/abcd">>)),
    ?assertMatch({error, {invalid_conn, {invalid_addr, _}}}, parse(<<"c=@">>)),
    ok.



%%%===================================================================
%%% Helpers
%%%===================================================================

parse(Bin) ->
    case ersip_sdp_conn:parse(<<Bin/binary, "\r\n">>) of
        {ok, V, <<>>} ->
            {ok, V};
        {error, _} = Error ->
            Error
    end.

