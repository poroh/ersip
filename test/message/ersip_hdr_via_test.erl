%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% iolist tests
%%

-module(ersip_hdr_via_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

topmost_via_test() ->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1">>,
                <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>
               ],
               HVia@0),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    {sent_protocol, Protocol, Version, Transport} = ersip_hdr_via:sent_protocol(Via),
    ?assertEqual(<<"SIP">>, Protocol),
    ?assertEqual(<<"2.0">>, Version),
    ?assertEqual(ersip_transport:make(udp), Transport),
    {sent_by, Host, Port} = ersip_hdr_via:sent_by(Via),
    ?assertEqual({hostname, <<"bigbox3.site3.atlanta.com">>}, Host),
    ?assertEqual(5060, Port).

topmost_via_ipport_test() ->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [<<"SIP/2.0/TCP 192.168.1.1:5090;branch=z9hG4bK77ef4c2312983.1">>,
                <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>
               ],
               HVia@0),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    {sent_protocol, Protocol, Version, Transport} = ersip_hdr_via:sent_protocol(Via),
    ?assertEqual(<<"SIP">>, Protocol),
    ?assertEqual(<<"2.0">>, Version),
    ?assertEqual(ersip_transport:make(tcp), Transport),
    {sent_by, Host, Port} = ersip_hdr_via:sent_by(Via),
    ?assertEqual({ipv4, {192, 168, 1, 1}}, Host),
    ?assertEqual(5090, Port).

check_params_test() ->
    {ok, Via} = ersip_hdr_via:parse((<<"SIP/2.0/TCP 192.168.1.1:5090;branch=z9hG4bK77ef4c2312983.1;rport;x=1;some">>)),
    ?assertEqual({ok, ersip_branch:make(<<"z9hG4bK77ef4c2312983.1">>)}, ersip_hdr_via:branch(Via)),
    ?assertEqual({ok, true}, ersip_hdr_via:rport(Via)),
    ?assertEqual({ok, <<"1">>}, ersip_hdr_via:raw_param(<<"x">>, Via)),
    ?assertEqual({ok, <<>>},    ersip_hdr_via:raw_param(<<"some">>, Via)),
    ?assertEqual({ok, <<>>},    ersip_hdr_via:raw_param(<<"rport">>, Via)),
    ok.

topmost_via_via_params_test() ->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com">>],
               HVia@0),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    ?assertEqual({ok, ersip_branch:make(<<"branch_v">>)}, ersip_hdr_via:branch(Via)),
    ?assertEqual({ok, 200}, ersip_hdr_via:ttl(Via)),
    ?assertEqual({ok, {ipv4, {1, 1, 1, 1}}}, ersip_hdr_via:received(Via)),
    ?assertEqual({ok, {hostname, <<"x.com">>}}, ersip_hdr_via:maddr(Via)),
    ok.

topmost_via_via_params_ipv6_test() ->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=[::1];maddr=[::1]">>],
               HVia@0),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    ?assertEqual({ok, {ipv6, {0, 0, 0, 0, 0, 0, 0, 1}}}, ersip_hdr_via:received(Via)),
    ?assertEqual({ok, {ipv6, {0, 0, 0, 0, 0, 0, 0, 1}}}, ersip_hdr_via:maddr(Via)),
    ok.

topmost_via_via_gen_params_test() ->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;my_param=abc">>],
               HVia@0),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    ?assertEqual({ok, <<"abc">>}, ersip_hdr_via:raw_param(<<"my_param">>, Via)),
    ok.

topmost_via_with_spaces_test()->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [<<"SIP / 2.0 / UDP first.example.com: 4000;ttl=16 ;maddr=224.2.0.1 ;branch=z9hG4bKa7c6a8dlze.1">>],
               HVia@0),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),

    {sent_protocol, Protocol, Version, Transport} = ersip_hdr_via:sent_protocol(Via),
    ?assertEqual(<<"SIP">>, Protocol),
    ?assertEqual(<<"2.0">>, Version),
    ?assertEqual(ersip_transport:make(udp), Transport),

    MaddrHost = ersip_host:make(<<"224.2.0.1">>),
    Branch = ersip_branch:make(<<"z9hG4bKa7c6a8dlze.1">>),
    ?assertEqual({ok, Branch},    ersip_hdr_via:branch(Via)),
    ?assertEqual({ok, MaddrHost}, ersip_hdr_via:maddr(Via)),
    ?assertEqual({ok, 16},        ersip_hdr_via:ttl(Via)),
    {sent_by, Host, Port} = ersip_hdr_via:sent_by(Via),
    ?assertEqual({hostname, <<"first.example.com">>}, Host),
    ?assertEqual(4000, Port),
    ok.


topmost_via_negative_test() ->
    HEmptyVia = ersip_hdr:new(<<"Via">>),
    ?assertMatch({error, _}, ersip_hdr_via:topmost_via(HEmptyVia)),
    bad_topmost_via(<<"SIP+2.0/UDP bigbox3.site3.atlanta.com;branch=z">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com::">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com::5060">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com:0">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com:65536">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com:-1">>),
    bad_topmost_via(<<"SIP/2.0/UDP [:5060">>),
    bad_topmost_via(<<"SIP/2.0/UDP []:5060">>),
    bad_topmost_via(<<"SIP/2.0/UDP []">>),
    bad_topmost_via(<<"SIP/2.0/UDP [">>),
    bad_topmost_via(<<"SIP/2.0/UDP -1.-1.-1.-1:5060">>),
    bad_topmost_via(<<"SIP/2.0+UDP 1.1.1.1:5060">>),
    bad_topmost_via(<<"SIP/2.0/$   1.1.1.1:5060">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=256">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=-1">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=a">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;received=a.b.c.d">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;maddr=?">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=\"xyz\"">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;my_param=\"x">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=0">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=-0">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=65536">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=A">>),
    bad_topmost_via(<<"SIP/3.0/UDP bigbox3.site3.atlanta.com;rport=A">>),
    ok.

via_branch_test() ->
    BranchValue = <<"z9hG4bK776asdhds">>,
    ViaHdr = create_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=", BranchValue/binary>>),
    Branch = ersip_branch:make(BranchValue),
    {ok, Via} = ersip_hdr_via:topmost_via(ViaHdr),
    {ok, ViaBranch} = ersip_hdr_via:branch(Via),
    ?assertEqual(ersip_branch:make_key(Branch), ersip_branch:make_key(ViaBranch)).

via_compare_test() ->
    via_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1">>),
    via_equal(<<"SIP/2.0/UDP BIGBOX3.SITE3.ATLANTA.COM;BRANCH=Z9HG4BK77EF4C2312983.1">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1">>),
    via_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=1">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;TTL=1">>),
    via_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;maddr=x.com">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;maddr=X.COM">>),
    via_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;received=1.1.1.1">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;received=1.1.1.1">>),
    via_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=10">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=10">>),
    via_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport">>),
    via_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;some=1">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;SOMe=1">>),
    via_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;some">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;SOMe">>),
    via_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com:5060">>),
    via_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com.">>,
              <<"SIP/2.0/UDP bigbox3.site3.atlanta.com">>),

    via_not_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com">>,
                  <<"SIP/2.0/TCP bigbox3.site3.atlanta.com">>),
    via_not_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=2">>,
                  <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=1">>),
    via_not_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=2">>,
                  <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=1">>),
    via_not_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport">>,
                  <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=1">>),
    via_not_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com">>,
                  <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport">>),
    via_not_equal(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com">>,
                  <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=1">>),
    ok.


via_sent_by_key_test() ->
    ViaHdr1 = create_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=x">>),
    ViaHdr2 = create_via(<<"SIP/2.0/UDP BIGBOX3.SITE3.ATLANTA.COM;branch=y">>),
    {ok, Via1} = ersip_hdr_via:topmost_via(ViaHdr1),
    {ok, Via2} = ersip_hdr_via:topmost_via(ViaHdr2),
    SentBy1 = ersip_hdr_via:sent_by_key(Via1),
    SentBy2 = ersip_hdr_via:sent_by_key(Via2),
    ?assertEqual(SentBy1, SentBy2).

assemle_test() ->
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com:500;ttl=1">>),
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1">>),
    check_reassemble(<<"SIP/2.0/UDP BIGBOX3.SITE3.ATLANTA.COM;BRANCH=Z9HG4BK77EF4C2312983.1">>),
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=1">>),
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;maddr=x.com">>),
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;received=1.1.1.1">>),
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport">>),
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;rport=1234">>),
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;some=1">>),
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;some">>),
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com:5060">>),
    check_reassemble(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com">>),
    check_reassemble(<<"SIP/2.0/WS [FF02:0:EDF5:1241:F919:AFF1:97C:97D2]:12207">>),
    ok.

set_param_received_ipv4_binary_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    Via1 = ersip_hdr_via:set_param(received, <<"2.2.2.2">>, Via),
    ?assertEqual(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=2.2.2.2;maddr=x.com">>,
                 ersip_hdr_via:assemble_bin(Via1)),
    ?assertEqual({ok, {ipv4, {2, 2, 2, 2}}}, ersip_hdr_via:received(Via1)),
    ok.

set_param_received_ipv6_binary_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    Via1 = ersip_hdr_via:set_param(received, <<"[::1]">>, Via),
    ?assertEqual(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=[::1];maddr=x.com">>,
                  ersip_hdr_via:assemble_bin(Via1)),
    ?assertEqual({ok, {ipv6, {0, 0, 0, 0, 0, 0, 0, 1}}}, ersip_hdr_via:received(Via1)),
    ok.

set_param_received_ipv4_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    Via1 = ersip_hdr_via:set_param(received, {ipv4, {2, 2, 2, 2}}, Via),
    ?assertEqual(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=2.2.2.2;maddr=x.com">>,
                 ersip_hdr_via:assemble_bin(Via1)),
    ?assertEqual({ok, {ipv4, {2, 2, 2, 2}}}, ersip_hdr_via:received(Via1)),
    ok.

set_param_received_ipv6_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    Via1 = ersip_hdr_via:set_param(received, {ipv6, {0, 0, 0, 0, 0, 0, 0, 1}}, Via),
    ?assertEqual(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=[::1];maddr=x.com">>,
                  ersip_hdr_via:assemble_bin(Via1)),
    ?assertEqual({ok, {ipv6, {0, 0, 0, 0, 0, 0, 0, 1}}}, ersip_hdr_via:received(Via1)),
    ok.

set_param_received_error_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    ?assertError({error, _}, ersip_hdr_via:set_param(received, x, Via)),
    ?assertError({error, _}, ersip_hdr_via:set_param(received, <<".">>, Via)).

set_param_rport_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;rport=1234;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    Via1 = ersip_hdr_via:set_rport(4321, Via),
    ?assertEqual(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;rport=4321;maddr=x.com">>,
                 ersip_hdr_via:assemble_bin(Via1)),
    ?assertEqual({ok, 4321}, ersip_hdr_via:rport(Via1)),
    ok.

set_param_rport_to_true_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;rport=1234;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    Via1 = ersip_hdr_via:set_rport(true, Via),
    ?assertEqual(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;rport;maddr=x.com">>,
                 ersip_hdr_via:assemble_bin(Via1)),
    ?assertEqual({ok, true}, ersip_hdr_via:rport(Via1)),
    ok.

set_param_rport_error_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;rport=4321;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    ?assertError({error, _}, ersip_hdr_via:set_param(rport, false, Via)),
    ?assertError({error, _}, ersip_hdr_via:set_param(rport, <<"aaaa">>, Via)),
    ?assertError({error, _}, ersip_hdr_via:set_param(rport, 65536, Via)),
    ?assertError({error, _}, ersip_hdr_via:set_param(rport, -1, Via)),
    ?assertError({error, _}, ersip_hdr_via:set_param(rport, 0, Via)),
    ok.

set_ttl_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    Via1 = ersip_hdr_via:set_ttl(99, Via),
    ?assertEqual(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=99;maddr=x.com">>,
                 ersip_hdr_via:assemble_bin(Via1)),
    ?assertEqual({ok, 99}, ersip_hdr_via:ttl(Via1)),
    ok.

set_maddr_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    Via1 = ersip_hdr_via:set_maddr(ersip_host:make(<<"[::1]">>), Via),
    ?assertEqual(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;maddr=[::1]">>,
                 ersip_hdr_via:assemble_bin(Via1)),
    ?assertEqual({ok, ersip_host:make(<<"[::1]">>)}, ersip_hdr_via:maddr(Via1)),
    ok.

set_branch_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    Via1 = ersip_hdr_via:set_branch(ersip_branch:make(<<"branch_v2">>), Via),
    ?assertEqual(<<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v2;ttl=200;maddr=x.com">>,
                 ersip_hdr_via:assemble_bin(Via1)),
    ?assertEqual({ok, ersip_branch:make(<<"branch_v2">>)}, ersip_hdr_via:branch(Via1)),
    ok.

all_raw_params_test() ->
    HVia@1 = create_via(<<"SIP/2.0/TCP 192.168.1.1:5090;Branch=branch_v;ttl=200;x;maddr=x.com">>),
    {ok, Via} = ersip_hdr_via:topmost_via(HVia@1),
    ?assertEqual([{<<"Branch">>, <<"branch_v">>},
                  {<<"ttl">>, <<"200">>},
                  <<"x">>,
                  {<<"maddr">>, <<"x.com">>}], ersip_hdr_via:all_raw_params(Via)),
    ok.

%%%===================================================================
%%% Implementation
%%%===================================================================

create_via(Bin) ->
    V@0 = ersip_hdr:new(<<"Via">>),
    ersip_hdr:add_values([Bin], V@0).

bad_topmost_via(Bin) ->
    ?assertMatch({error, _}, ersip_hdr_via:topmost_via(create_via(Bin))).

via_equal(ViaBin1, ViaBin2) ->
    {ok, Via1} = ersip_hdr_via:topmost_via(create_via(ViaBin1)),
    {ok, Via2} = ersip_hdr_via:topmost_via(create_via(ViaBin2)),
    ?assertEqual(ersip_hdr_via:make_key(Via1), ersip_hdr_via:make_key(Via2)).

via_not_equal(ViaBin1, ViaBin2) ->
    {ok, Via1} = ersip_hdr_via:topmost_via(create_via(ViaBin1)),
    {ok, Via2} = ersip_hdr_via:topmost_via(create_via(ViaBin2)),
    ?assertNotEqual(ersip_hdr_via:make_key(Via1), ersip_hdr_via:make_key(Via2)).

check_reassemble(Binary) ->
    {ok, Via1} = ersip_hdr_via:topmost_via(create_via(Binary)),
    Via1Bin = ersip_hdr_via:assemble(Via1),
    {ok, Via2} = ersip_hdr_via:topmost_via(create_via(Via1Bin)),
    ?assertEqual(ersip_hdr_via:make_key(Via1), ersip_hdr_via:make_key(Via2)).
