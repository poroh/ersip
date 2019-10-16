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
    ?assertEqual(5060,  ersip_transport:default_port(make_transport(sctp))),
    ?assertEqual(5061, ersip_transport:default_port(make_transport(tls))),
    ?assertEqual(80,   ersip_transport:default_port(make_transport(ws))),
    ?assertEqual(443,  ersip_transport:default_port(make_transport(wss))),
    CustomTransport = make_transport(<<"unknowntranport">>),
    ?assertEqual({default_port, CustomTransport}, ersip_transport:default_port(CustomTransport)).

is_datagram_test() ->
    ?assertEqual(true,  ersip_transport:is_datagram(make_transport(udp))),
    ?assertEqual(true,  ersip_transport:is_datagram(make_transport(ws))),
    ?assertEqual(true,  ersip_transport:is_datagram(make_transport(wss))),
    ?assertEqual(true,  ersip_transport:is_datagram(make_transport(sctp))),
    ?assertEqual(false, ersip_transport:is_datagram(make_transport(tls))),
    ?assertEqual(false, ersip_transport:is_datagram(make_transport(tcp))),
    ?assertError({error, _}, ersip_transport:is_datagram(make_transport(<<"unknowntranport">>))).

is_tls_test() ->
    ?assertEqual(false, ersip_transport:is_tls(make_transport(udp))),
    ?assertEqual(false, ersip_transport:is_tls(make_transport(ws))),
    ?assertEqual(true,  ersip_transport:is_tls(make_transport(wss))),
    ?assertEqual(true,  ersip_transport:is_tls(make_transport(tls))),
    ?assertEqual(false, ersip_transport:is_tls(make_transport(tcp))),
    ?assertEqual(false, ersip_transport:is_tls(make_transport(sctp))),
    ?assertError({error, _}, ersip_transport:is_tls(make_transport(<<"unknowntranport">>))).

is_reliable_test() ->
    ?assertEqual(false,  ersip_transport:is_reliable(make_transport(udp))),
    ?assertEqual(true,   ersip_transport:is_reliable(make_transport(ws))),
    ?assertEqual(true,   ersip_transport:is_reliable(make_transport(wss))),
    ?assertEqual(true,   ersip_transport:is_reliable(make_transport(tls))),
    ?assertEqual(true,   ersip_transport:is_reliable(make_transport(tcp))),
    ?assertEqual(true,   ersip_transport:is_reliable(make_transport(sctp))),
    ?assertError({error, _}, ersip_transport:is_reliable(make_transport(<<"unknowntranport">>))).


assemble_test() ->
    ?assertEqual(<<"UDP">>, ersip_transport:assemble_upper(make_transport(udp))),
    ?assertEqual(<<"WS">>,  ersip_transport:assemble_upper(make_transport(ws))),
    ?assertEqual(<<"WSS">>, ersip_transport:assemble_upper(make_transport(wss))),
    ?assertEqual(<<"TLS">>, ersip_transport:assemble_upper(make_transport(tls))),
    ?assertEqual(<<"TCP">>, ersip_transport:assemble_upper(make_transport(tcp))),
    ?assertEqual(<<"SCTP">>, ersip_transport:assemble_upper(make_transport(sctp))),
    ?assertEqual(<<"SOME">>, ersip_transport:assemble_upper(make_transport(<<"some">>))),

    ?assertEqual(<<"udp">>, ersip_transport:assemble(make_transport(udp))),
    ?assertEqual(<<"ws">>,  ersip_transport:assemble(make_transport(ws))),
    ?assertEqual(<<"wss">>, ersip_transport:assemble(make_transport(wss))),
    ?assertEqual(<<"tls">>, ersip_transport:assemble(make_transport(tls))),
    ?assertEqual(<<"tcp">>, ersip_transport:assemble(make_transport(tcp))),
    ?assertEqual(<<"sctp">>, ersip_transport:assemble(make_transport(sctp))),
    ?assertEqual(<<"some">>, ersip_transport:assemble(make_transport(<<"Some">>))).

make_test() ->
    ?assertEqual({transport, udp}, ersip_transport:make(udp)),
    ?assertError(badarg, ersip_transport:make(x)).

make_by_uri_test() ->
    ?assertEqual({transport, udp}, ersip_transport:make_by_uri(ersip_uri:make(<<"sip:a@b">>))),
    ?assertEqual({transport, ws},  ersip_transport:make_by_uri(ersip_uri:make(<<"sip:a@b;transport=ws">>))),
    ?assertEqual({transport, udp}, ersip_transport:make_by_uri(ersip_uri:make(<<"sip:a@b;transport=udp">>))),
    ?assertEqual({transport, tcp}, ersip_transport:make_by_uri(ersip_uri:make(<<"sip:a@b;transport=tcp">>))),
    ?assertEqual({transport, sctp}, ersip_transport:make_by_uri(ersip_uri:make(<<"sip:a@b;transport=sctp">>))),
    ?assertEqual(make_transport(<<"some">>), ersip_transport:make_by_uri(ersip_uri:make(<<"sip:a@b;transport=some">>))),
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================

make_transport(V) ->
    ersip_transport:make(V).
