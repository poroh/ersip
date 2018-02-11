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

topmost_via_test() ->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [ <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1">>,
                 <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>
               ],
               HVia@0),
    { ok, Via } = ersip_hdr_via:topmost_via(HVia@1),
    { sent_protocol, Protocol, Version, Transport } = ersip_hdr_via:sent_protocol(Via),
    ?assertEqual(<<"SIP">>, Protocol),
    ?assertEqual(<<"2.0">>, Version),
    ?assertEqual(ersip_transport:make(udp), Transport),
    { sent_by, Host, Port } = ersip_hdr_via:sent_by(Via),
    ?assertEqual({ hostname, <<"bigbox3.site3.atlanta.com">> }, Host),
    ?assertEqual(5060, Port).

topmost_via_ipport_test() ->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [ <<"SIP/2.0/TCP 192.168.1.1:5090;branch=z9hG4bK77ef4c2312983.1">>,
                 <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>
               ],
               HVia@0),
    { ok, Via } = ersip_hdr_via:topmost_via(HVia@1),
    { sent_protocol, Protocol, Version, Transport } = ersip_hdr_via:sent_protocol(Via),
    ?assertEqual(<<"SIP">>, Protocol),
    ?assertEqual(<<"2.0">>, Version),
    ?assertEqual(ersip_transport:make(tcp), Transport),
    { sent_by, Host, Port } = ersip_hdr_via:sent_by(Via),
    ?assertEqual({ ipv4, {192, 168, 1, 1} }, Host),
    ?assertEqual(5090, Port).

topmost_via_via_params_test() ->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [ <<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=1.1.1.1;maddr=x.com">> ],
               HVia@0),
    { ok, Via } = ersip_hdr_via:topmost_via(HVia@1),
    ?assertMatch(
       #{ branch   := <<"branch_v">>,
          ttl      := 200,
          received := { ipv4, { 1, 1, 1, 1 } },
          maddr    := { hostname, <<"x.com">> } },
       ersip_hdr_via:params(Via)).

topmost_via_via_params_ipv6_test() ->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [ <<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;ttl=200;received=[::1];maddr=[::1]">> ],
               HVia@0),
    { ok, Via } = ersip_hdr_via:topmost_via(HVia@1),
    ?assertMatch(
       #{ branch   := <<"branch_v">>,
          ttl      := 200,
          received := { ipv6, { 0, 0, 0, 0, 0, 0, 0, 1 } },
          maddr    := { ipv6, { 0, 0, 0, 0, 0, 0, 0, 1 } } },
       ersip_hdr_via:params(Via)).

topmost_via_via_gen_params_test() ->
    HVia@0 = ersip_hdr:new(<<"Via">>),
    HVia@1 = ersip_hdr:add_values(
               [ <<"SIP/2.0/TCP 192.168.1.1:5090;branch=branch_v;my_param=abc">> ],
               HVia@0),
    { ok, Via } = ersip_hdr_via:topmost_via(HVia@1),
    ?assertMatch(
       #{ branch   := <<"branch_v">>,
          <<"my_param">> := <<"abc">> },
       ersip_hdr_via:params(Via)).

topmost_via_negative_test() ->
    HEmptyVia = ersip_hdr:new(<<"Via">>),
    ?assertMatch({ error, _ }, ersip_hdr_via:topmost_via(HEmptyVia)),
    bad_topmost_via(<<"SIP+2.0/UDP bigbox3.site3.atlanta.com;branch=z">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com::">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com::5060">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com:0">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com:65536">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com:-1">>),
    bad_topmost_via(<<"SIP/2.0/UDP -1.-1.-1.-1:5060">>),
    bad_topmost_via(<<"SIP/2.0+UDP 1.1.1.1:5060">>),
    bad_topmost_via(<<"SIP/2.0/$   1.1.1.1:5060">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=256">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=-1">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;ttl=a">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;received=a.b.c.d">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;maddr=?">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=\"xyz\"">>),
    bad_topmost_via(<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;my_param=\"x">>).




%%%===================================================================
%%% Implementation
%%%===================================================================

create_via(Bin) ->
    V@0 = ersip_hdr:new(<<"Via">>),
    ersip_hdr:add_values([ Bin ], V@0).

bad_topmost_via(Bin) ->
    ?assertMatch({error, _}, ersip_hdr_via:topmost_via(create_via(Bin))).
