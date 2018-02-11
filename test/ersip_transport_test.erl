%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common SIP transaction interface tests
%%

-module(ersip_transport_test).

-include_lib("eunit/include/eunit.hrl").

parse_port_number_test() ->
    ?assertEqual({ ok, 5060, <<>>},      ersip_transport:parse_port_number(<<"5060">>)),
    ?assertEqual({ ok, 80, <<" ">>},     ersip_transport:parse_port_number(<<"80 ">>)),
    ?assertEqual({ ok, 5061, <<"foo">>}, ersip_transport:parse_port_number(<<"5061foo">>)),
    ?assertMatch({ error, _ },           ersip_transport:parse_port_number(<<"0">>)),
    ?assertMatch({ error, _ },           ersip_transport:parse_port_number(<<"100000">>)),
    ?assertMatch({ error, _ },           ersip_transport:parse_port_number(<<"-1">>)).

default_port_test() ->
    ?assertEqual(5060, ersip_transport:default_port(make_transport(udp))),
    ?assertEqual(5060, ersip_transport:default_port(make_transport(tcp))),
    ?assertEqual(5061, ersip_transport:default_port(make_transport(tls))),
    ?assertEqual(80,   ersip_transport:default_port(make_transport(ws))),
    ?assertEqual(443,  ersip_transport:default_port(make_transport(wss))),
    CustomTransport = make_transport(<<"unknowntranport">>),
    ?assertEqual({default_port, CustomTransport}, ersip_transport:default_port(CustomTransport)).

make_test() ->
    ?assertEqual({transport, udp}, ersip_transport:make(udp)),
    ?assertError(badarg, ersip_transport:make(x)).

make_transport(V) ->
    ersip_transport:make(V).
