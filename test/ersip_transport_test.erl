%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common SIP transaction interface tests
%%

-module(ersip_transport_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_port_number_test() ->
    ?assertEqual({ok, 5060, <<>>},      ersip_transport:parse_port_number(<<"5060">>)),
    ?assertEqual({ok, 80, <<" ">>},     ersip_transport:parse_port_number(<<"80 ">>)),
    ?assertEqual({ok, 5061, <<"foo">>}, ersip_transport:parse_port_number(<<"5061foo">>)),
    ?assertMatch({error, _},           ersip_transport:parse_port_number(<<"0">>)),
    ?assertMatch({error, _},           ersip_transport:parse_port_number(<<"100000">>)),
    ?assertMatch({error, _},           ersip_transport:parse_port_number(<<"-1">>)).

default_port_test() ->
    ?assertEqual(5060, ersip_transport:default_port(make_transport(udp))),
    ?assertEqual(5060, ersip_transport:default_port(make_transport(tcp))),
    ?assertEqual(5061, ersip_transport:default_port(make_transport(tls))),
    ?assertEqual(80,   ersip_transport:default_port(make_transport(ws))),
    ?assertEqual(443,  ersip_transport:default_port(make_transport(wss))),
    CustomTransport = make_transport(<<"unknowntranport">>),
    ?assertEqual({default_port, CustomTransport}, ersip_transport:default_port(CustomTransport)).

is_datagram_test() ->
    ?assertEqual(true,  ersip_transport:is_datagram(make_transport(udp))),
    ?assertEqual(true,  ersip_transport:is_datagram(make_transport(ws))),
    ?assertEqual(true,  ersip_transport:is_datagram(make_transport(wss))),
    ?assertEqual(false, ersip_transport:is_datagram(make_transport(tls))),
    ?assertEqual(false, ersip_transport:is_datagram(make_transport(tcp))),
    ?assertError({error, _}, ersip_transport:is_datagram(make_transport(<<"unknowntranport">>))).

is_tls_test() ->
    ?assertEqual(false, ersip_transport:is_tls(make_transport(udp))),
    ?assertEqual(false, ersip_transport:is_tls(make_transport(ws))),
    ?assertEqual(true,  ersip_transport:is_tls(make_transport(wss))),
    ?assertEqual(true,  ersip_transport:is_tls(make_transport(tls))),
    ?assertEqual(false, ersip_transport:is_tls(make_transport(tcp))),
    ?assertError({error, _}, ersip_transport:is_tls(make_transport(<<"unknowntranport">>))).

assemble_test() ->
    ?assertEqual(<<"UDP">>, ersip_transport:assemble_upper(make_transport(udp))),
    ?assertEqual(<<"WS">>,  ersip_transport:assemble_upper(make_transport(ws))),
    ?assertEqual(<<"WSS">>, ersip_transport:assemble_upper(make_transport(wss))),
    ?assertEqual(<<"TLS">>, ersip_transport:assemble_upper(make_transport(tls))),
    ?assertEqual(<<"TCP">>, ersip_transport:assemble_upper(make_transport(tcp))),
    ?assertEqual(<<"SOME">>, ersip_transport:assemble_upper(make_transport(<<"some">>))),

    ?assertEqual(<<"udp">>, ersip_transport:assemble(make_transport(udp))),
    ?assertEqual(<<"ws">>,  ersip_transport:assemble(make_transport(ws))),
    ?assertEqual(<<"wss">>, ersip_transport:assemble(make_transport(wss))),
    ?assertEqual(<<"tls">>, ersip_transport:assemble(make_transport(tls))),
    ?assertEqual(<<"tcp">>, ersip_transport:assemble(make_transport(tcp))),
    ?assertEqual(<<"some">>, ersip_transport:assemble(make_transport(<<"Some">>))).

make_test() ->
    ?assertEqual({transport, udp}, ersip_transport:make(udp)),
    ?assertError(badarg, ersip_transport:make(x)).

%%%===================================================================
%%% Helpers
%%%===================================================================

make_transport(V) ->
    ersip_transport:make(V).
