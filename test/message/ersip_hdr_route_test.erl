%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Route header test
%%%

-module(ersip_hdr_route_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

parse_test() ->
    ?assertMatch({ok, _}, ersip_hdr_route:parse(<<"<sip:services.example.com;lr;unknownwith=value;unknown-no-value>">>)),
    ?assertMatch({ok, _}, ersip_hdr_route:parse(<<"sip:services.example.com;lr">>)),

    ?assertMatch({error, {invalid_route, _}}, ersip_hdr_route:parse(<<"sip:a@b,">>)),
    ?assertMatch({error, {invalid_route, _}}, ersip_hdr_route:parse(<<"sip:">>)),
    ?assertMatch({error, {invalid_route, _}}, ersip_hdr_route:parse(<<"Bob">>)),
    ok.

make_from_raw_test() ->
    ?assertEqual(<<"<sip:a@b>">>, assemble(ersip_hdr_route:make(#{uri => <<"sip:a@b">>}))),
    ?assertEqual(<<"<sip:a@b;lr>">>, assemble(ersip_hdr_route:make(#{uri => <<"sip:a@b;lr">>}))),
    ?assertEqual(<<"<sip:a@b;lr>;ext=1">>,
                 assemble(ersip_hdr_route:make(#{uri => <<"sip:a@b;lr">>,
                                                 params => #{<<"ext">> => <<"1">>}}))),
    ?assertEqual(<<"<sip:a@b;lr>;extNoArg">>,
                 assemble(ersip_hdr_route:make(#{uri => <<"sip:a@b;lr">>,
                                                 params => #{<<"extNoArg">> => <<>>}}))),
    ok.

make_error_test() ->
    ?assertError({invalid_route, _}, ersip_hdr_route:make(<<"Bob">>)),
    ?assertError({invalid_route, _}, ersip_hdr_route:make(<<"sip:a@b,">>)),
    ?assertError({invalid_route, _}, ersip_hdr_route:make(<<"sip:">>)),
    ok.

is_loose_route_test() ->
    ?assertEqual(true,  ersip_hdr_route:is_loose_route(make(<<"<sip:a@b;lr>">>))),
    ?assertEqual(true,  ersip_hdr_route:is_loose_route(make(<<"<sip:a@b;lr=on>">>))),
    ?assertEqual(false, ersip_hdr_route:is_loose_route(make(<<"<sip:a@b>;lr">>))),
    ?assertEqual(false, ersip_hdr_route:is_loose_route(make(<<"<sip:a@b>">>))),
    ?assertEqual(false, ersip_hdr_route:is_loose_route(make(<<"sip:a@b;lr">>))),
    ?assertEqual(false, ersip_hdr_route:is_loose_route(make(<<"sip:a@b">>))),
    ok.

param_test() ->
    ?assertEqual(<<"<sip:a@b>;ext=1">>, assemble(ersip_hdr_route:set_param(<<"ext">>, <<"1">>, make(<<"sip:a@b">>)))),
    ?assertEqual(<<"<sip:a@b>;extNoArg">>, assemble(ersip_hdr_route:set_param(<<"extNoArg">>, <<>>, make(<<"sip:a@b">>)))),

    ?assertEqual({ok, <<"1">>}, ersip_hdr_route:param(<<"ext">>, make(<<"sip:a@b;ext=1">>))),
    ?assertEqual({ok, <<"1">>}, ersip_hdr_route:param(<<"Ext">>, make(<<"sip:a@b;ext=1">>))),
    ?assertEqual({ok, <<"1">>}, ersip_hdr_route:param(<<"ext">>, make(<<"sip:a@b;Ext=1">>))),
    ?assertEqual({ok, <<>>}, ersip_hdr_route:param(<<"extNoArg">>, make(<<"sip:a@b;ExtNoArg">>))),
    ok.

uri_modification_test() ->
    BiloxiURI = uri(<<"sip:biloxi.com;lr">>),
    BiloxiRt1 = ersip_hdr_route:set_uri(BiloxiURI, make(<<"Alice Proxy <sip:alice-proxy.atlanta.com>;ext=1">>)),
    ?assertEqual(<<"Alice Proxy <sip:biloxi.com;lr>;ext=1">>, assemble(BiloxiRt1)),
    BiloxiRt2 = ersip_hdr_route:set_uri(BiloxiURI, make(<<"Alice Proxy <sip:alice-proxy.atlanta.com>">>)),
    ?assertEqual(<<"Alice Proxy <sip:biloxi.com;lr>">>, assemble(BiloxiRt2)),
    BiloxiRt3 = ersip_hdr_route:set_uri(BiloxiURI, make(<<"sip:alice-proxy.atlanta.com;ext=1">>)),
    ?assertEqual(<<"<sip:biloxi.com;lr>;ext=1">>, assemble(BiloxiRt3)),

    ?assertEqual(BiloxiURI, ersip_hdr_route:uri(make(assemble(BiloxiRt1)))),
    ?assertEqual(BiloxiURI, ersip_hdr_route:uri(make(assemble(BiloxiRt2)))),
    ?assertEqual(BiloxiURI, ersip_hdr_route:uri(make(assemble(BiloxiRt3)))),
    ok.

display_name_modification_test() ->
    BobRt1 = ersip_hdr_route:set_display_name(display_name(<<"Bob">>), make(<<"Alice Proxy <sip:alice-proxy.atlanta.com>;ext=1">>)),
    ?assertEqual(<<"Bob <sip:alice-proxy.atlanta.com>;ext=1">>, assemble(BobRt1)),
    ?assertEqual(display_name(<<"Bob">>), ersip_hdr_route:display_name(make(assemble(BobRt1)))),

    BobRt2 = ersip_hdr_route:set_display_name(display_name(<<"Bob \"Marley\" Proxy">>), make(<<"Alice Proxy <sip:alice-proxy.atlanta.com>;ext=1">>)),
    ?assertEqual(<<"\"Bob \\\"Marley\\\" Proxy\" <sip:alice-proxy.atlanta.com>;ext=1">>, assemble(BobRt2)),
    ?assertEqual(display_name(<<"Bob \"Marley\" Proxy">>), ersip_hdr_route:display_name(make(assemble(BobRt2)))),
    ok.

raw_test() ->
    ?assertMatch(#{uri := #{sip := #{user := <<"alice">>}}}, ersip_hdr_route:raw(make(<<"sip:alice@atlanta.com">>))),
    ?assertMatch(#{display_name := <<"Alice">>},             ersip_hdr_route:raw(make(<<"Alice <sip:alice@atlanta.com>">>))),
    ?assertMatch(#{params := #{<<"ext">> := <<"1">>}},       ersip_hdr_route:raw(make(<<"Alice <sip:alice@atlanta.com>;ext=1">>))),
    ok.


%%===================================================================
%% Helpers
%%===================================================================

uri(Bin) ->
    ersip_uri:make(Bin).

display_name(Bin) ->
    ersip_display_name:make(Bin).

make(Bin) ->
    ersip_hdr_route:make(Bin).

assemble(Route) ->
    ersip_hdr_route:assemble_bin(Route).
