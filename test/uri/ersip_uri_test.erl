%%%
%%% Copyright (c) 2017, 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% URI tests
%%%

-module(ersip_uri_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

%-------------------------------------------------------------------
% Parse tests
%-------------------------------------------------------------------

basic_parse_test() ->
    parse_sip_ok(<<"sip:b">>,      #{host => <<"b">>}),
    parse_sip_ok(<<"sip:b:5090">>, #{host => <<"b">>, port => 5090}),

    parse_sip_ok(<<"sip:a@b:5090">>,       #{user => <<"a">>,     host => <<"b">>,       port => 5090}),
    parse_sip_ok(<<"sip:a:b@b:5090">>,     #{user => <<"a:b">>,   host => <<"b">>,       port => 5090}),
    parse_sip_ok(<<"sip:a@1.2.3.4:5090">>, #{user => <<"a">>,     host => <<"1.2.3.4">>, port => 5090}),
    parse_sip_ok(<<"sip:a@[::1]:5090">>,   #{user => <<"a">>,     host => <<"[::1]">>,   port => 5090}),
    parse_sip_ok(<<"sip:a:%20@b:5090">>,   #{user => <<"a:%20">>, host => <<"b">>,       port => 5090}),
    parse_sip_ok(<<"sip:%20@b:5090">>,     #{user => <<"%20">>,   host => <<"b">>,       port => 5090}),

    parse_sips_ok(<<"sips:a@b:5090">>,     #{user => <<"a">>,   host => <<"b">>, port => 5090}),
    parse_sips_ok(<<"sips:a:b@b:5090">>,   #{user => <<"a:b">>, host => <<"b">>, port => 5090}),

    parse_tel_ok(<<"TEL:+16505550505">>, #{user => <<"+16505550505">>}),
    parse_ok(<<"cid:+16505550505">>, #{scheme => <<"cid">>, data => <<"+16505550505">>}),
    ok.

parse_fail_test() ->
    ?assertMatch({error, {invalid_scheme, _}}, ersip_uri:parse(<<"?:a@b:5090">>)),
    ?assertMatch({error, {invalid_scheme, _}}, ersip_uri:parse(<<"a@b">>)),
    ?assertMatch({error, {invalid_sip_uri, empty_username}},    ersip_uri:parse(<<"sip:@b:5090">>)),
    ?assertMatch({error, {invalid_sip_uri, empty_username}},    ersip_uri:parse(<<"sip::a@b:5090">>)),
    ?assertMatch({error, {invalid_sip_uri, {bad_password, _}}}, ersip_uri:parse(<<"sip:a:%@b:5090">>)),

    ?assertMatch({error, {invalid_sip_uri, {invalid_host, _}}}, ersip_uri:parse(<<"sip:%:5090">>)),
    ?assertMatch({error, {invalid_sip_uri, {invalid_host, _}}}, ersip_uri:parse(<<"sip:%">>)),
    ?assertMatch({error, {invalid_sip_uri, {invalid_host, _}}}, ersip_uri:parse(<<"sip:a.-">>)),
    ?assertMatch({error, {invalid_sip_uri, {invalid_port, _}}}, ersip_uri:parse(<<"sip:b:x">>)),
    ?assertMatch({error, {invalid_sip_uri, {invalid_ipv6_reference, _}}}, ersip_uri:parse(<<"sip:[::1">>)),
    ?assertMatch({error, {invalid_sip_uri, {invalid_port, _}}}, ersip_uri:parse(<<"sip:[::1]:">>)),
    ?assertMatch({error, {invalid_sip_uri, {invalid_port, _}}}, ersip_uri:parse(<<"sip:[::1]x">>)),

    ?assertMatch({error, {invalid_sip_uri, {invalid_port, _}}}, ersip_uri:parse(<<"sips:[::1]x">>)),
    ?assertMatch({error, {invalid_sip_uri, {invalid_parameter, _}}}, ersip_uri:parse(<<"sips:[::1];=b">>)),
    ok.

permissive_parsing_test() ->
    parse_sip_ok(<<"sip:%@b:5090">>, #{user => <<"%25">>, host => <<"b">>, port => 5090}),
    ok.

%-------------------------------------------------------------------
% transport parameter
%-------------------------------------------------------------------

transport_parse_test() ->
    check_transport(<<"sip:b;transport=tcp">>,  tcp),
    check_transport(<<"sip:b;transport=sctp">>, sctp),
    check_transport(<<"sip:b;transport=udp">>,  udp),
    check_transport(<<"sip:b;transport=tls">>,  tls),
    check_transport(<<"sip:b;transport=ws">>,   ws),
    check_transport(<<"sip:b;transport=wss">>,  wss),
    check_other_transport(<<"sip:b;transport=wssnew">>, <<"wssnew">>),
    check_transport(<<"tel:+16505550505;transport=tcp">>,  tcp),

    ?assertEqual(undefined, ersip_uri:transport(ersip_uri:make(<<"sip:b">>))),
    ?assertEqual(undefined, ersip_uri:transport(ersip_uri:make(<<"tel:+16505550505">>))),
    ok.

transport_parse_fail_test() ->
    ?assertMatch({error, {invalid_sip_uri, {invalid_transport, _}}}, ersip_uri:parse(<<"sip:b;transport=&">>)),
    ?assertMatch({error, {invalid_sip_uri, {invalid_transport, _}}}, ersip_uri:parse(<<"sip:b;transport=&;user=phone">>)),
    ok.

set_transport_test() ->
    ?assertEqual(ersip_uri:make(<<"sip:carol@chicago.com;transport=udp">>),
                 ersip_uri:set_transport(ersip_transport:make(udp),
                                         ersip_uri:make(<<"sip:carol@chicago.com;transport=tcp">>))),
    ?assertEqual(ersip_uri:make(<<"sip:carol@chicago.com;transport=tls">>),
                 ersip_uri:set_transport(ersip_transport:make(tls),
                                         ersip_uri:make(<<"sip:carol@chicago.com;TRANSPORT=tcp">>))),
    ?assertEqual(ersip_uri:make(<<"tel:+16505550505;transport=udp">>),
                ersip_uri:set_transport(ersip_transport:make(udp),
                                        ersip_uri:make(<<"tel:+16505550505">>))),
    ok.

clear_transport_test() ->
    ?assertEqual(ersip_uri:make(<<"sip:carol@chicago.com">>),
                 ersip_uri:clear_transport(ersip_uri:make(<<"sip:carol@chicago.com;transport=tcp">>))),
    ?assertEqual(ersip_uri:make(<<"sip:carol@chicago.com">>),
                 ersip_uri:clear_transport(ersip_uri:make(<<"sip:carol@chicago.com;TRANSPORT=tcp">>))),
    ?assertEqual(ersip_uri:make(<<"tel:+16505550505">>),
                 ersip_uri:clear_transport(ersip_uri:make(<<"tel:+16505550505;transport=udp">>))),
    ok.

%-------------------------------------------------------------------
% loose router (lr)
%-------------------------------------------------------------------

loose_router_test() ->
    ?assertEqual(true,  ersip_uri:loose_router(ersip_uri:make(<<"sip:b;lr">>))),
    ?assertEqual(false, ersip_uri:loose_router(ersip_uri:make(<<"sip:b">>))),
    ?assertEqual(true,  ersip_uri:loose_router(ersip_uri:make(<<"tel:+16505550505;lr">>))),
    ?assertEqual(false, ersip_uri:loose_router(ersip_uri:make(<<"tel:+16505550505">>))),
    ok.

set_loose_router_test() ->
    URI0  = ersip_uri:make(<<"sip:b">>),
    ?assertEqual(false,  ersip_uri:loose_router(URI0)),
    URI1 = ersip_uri:set_loose_router(true, URI0),
    ?assertEqual(true,  ersip_uri:loose_router(URI1)),

    %% Check that set remains after serialization.
    URI2 = ersip_uri:make(ersip_uri:assemble_bin(URI1)),
    ?assertEqual(true,  ersip_uri:loose_router(URI2)),

    URI3 = ersip_uri:set_loose_router(false, URI2),
    ?assertEqual(false, ersip_uri:loose_router(URI3)),

    %% Check that clear remains after serialization.
    URI4 = ersip_uri:make(ersip_uri:assemble_bin(URI3)),
    ?assertEqual(false, ersip_uri:loose_router(URI4)),

    TELURI0  = ersip_uri:make(<<"tel:+16505550505">>),
    ?assertEqual(false,  ersip_uri:loose_router(TELURI0)),
    TELURI1 = ersip_uri:set_loose_router(true, TELURI0),
    ?assertEqual(true,  ersip_uri:loose_router(TELURI1)),

    %% Check that set remains after serialization.
    TELURI2 = ersip_uri:make(ersip_uri:assemble_bin(TELURI1)),
    ?assertEqual(true,  ersip_uri:loose_router(TELURI2)),

    TELURI3 = ersip_uri:set_loose_router(false, TELURI2),
    ?assertEqual(false, ersip_uri:loose_router(TELURI3)),

    %% Check that clear remains after serialization.
    TELURI4 = ersip_uri:make(ersip_uri:assemble_bin(TELURI3)),
    ?assertEqual(false, ersip_uri:loose_router(TELURI4)),

    %% Check error on non-SIP URI:
    ?assertError({sip_uri_expected, _}, ersip_uri:set_loose_router(false, ersip_uri:make(<<"cid:+16505550505">>))),
    ?assertError({sip_uri_expected, _}, ersip_uri:set_loose_router(true, ersip_uri:make(<<"cid:+16505550505">>))),
    ok.

%-------------------------------------------------------------------
% maddr
%-------------------------------------------------------------------

maddr_test() ->
    ?assertEqual(ersip_host:make(<<"1.1.1.1">>), ersip_uri:maddr(ersip_uri:make(<<"sip:b;maddr=1.1.1.1">>))),
    ?assertEqual(ersip_host:make(<<"[::1]">>), ersip_uri:maddr(ersip_uri:make(<<"sip:b;maddr=[::1]">>))),
    ?assertEqual(ersip_host:make(<<"atlanta.com">>), ersip_uri:maddr(ersip_uri:make(<<"sip:b;maddr=atlanta.com">>))),
    ?assertMatch({error, {invalid_sip_uri, {invalid_maddr, _}}}, ersip_uri:parse(<<"sip:b;maddr=&">>)),

    ?assertEqual(undefined, ersip_uri:maddr(ersip_uri:make(<<"sip:b">>))),
    ?assertEqual(ersip_host:make(<<"1.1.1.1">>), ersip_uri:maddr(ersip_uri:make(<<"tel:+16505550505;maddr=1.1.1.1">>))),
    ok.

set_maddr_test() ->
    ?assertEqual(<<"sip:b;maddr=1.1.1.1">>, ersip_uri:assemble_bin(ersip_uri:set_maddr(ersip_host:make(<<"1.1.1.1">>), ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;maddr=[::1]">>, ersip_uri:assemble_bin(ersip_uri:set_maddr(ersip_host:make(<<"[::1]">>), ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;maddr=atlanta.com">>, ersip_uri:assemble_bin(ersip_uri:set_maddr(ersip_host:make(<<"atlanta.com">>), ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;maddr=1.1.1.1">>, ersip_uri:assemble_bin(ersip_uri:set_maddr(ersip_host:make(<<"1.1.1.1">>), ersip_uri:make(<<"sip:b;maddr=atlanta.com">>)))),

    ?assertEqual(<<"tel:+16505550505;maddr=1.1.1.1">>, ersip_uri:assemble_bin(ersip_uri:set_maddr(ersip_host:make(<<"1.1.1.1">>), ersip_uri:make(<<"tel:+16505550505;maddr=atlanta.com">>)))),

    ?assertError({sip_uri_expected, _}, ersip_uri:set_maddr(ersip_host:make(<<"1.1.1.1">>), ersip_uri:make(<<"cid:+16505550505">>))),
    ?assertError({host_expected, _},    ersip_uri:set_maddr(<<"1.1.1.1">>, ersip_uri:make(<<"sip:b">>))),
    ok.

clear_maddr_test() ->
    ?assertEqual(<<"sip:b">>, ersip_uri:assemble_bin(ersip_uri:clear_maddr(ersip_uri:make(<<"sip:b;maddr=1.1.1.1">>)))),
    ?assertEqual(<<"sip:b">>, ersip_uri:assemble_bin(ersip_uri:clear_maddr(ersip_uri:make(<<"sip:b;maddr=[::1]">>)))),
    ?assertEqual(<<"sip:b">>, ersip_uri:assemble_bin(ersip_uri:clear_maddr(ersip_uri:make(<<"sip:b;maddr=atlanta.com">>)))),
    ?assertEqual(<<"tel:+16505550505">>, ersip_uri:assemble_bin(ersip_uri:clear_maddr(ersip_uri:make(<<"tel:+16505550505;maddr=atlanta.com">>)))),

    ?assertError({sip_uri_expected, _}, ersip_uri:clear_maddr(ersip_uri:make(<<"cid:+16505550505">>))),
    ok.

%-------------------------------------------------------------------
% user parameter of URI
%-------------------------------------------------------------------

user_param_test() ->
    ?assertEqual(phone,           ersip_uri:user_param(ersip_uri:make(<<"sip:b;user=phone">>))),
    ?assertEqual(ip,              ersip_uri:user_param(ersip_uri:make(<<"sip:b;user=ip">>))),
    ?assertEqual(<<"something">>, ersip_uri:user_param(ersip_uri:make(<<"sip:b;user=something">>))),
    ?assertEqual(undefined,       ersip_uri:user_param(ersip_uri:make(<<"sip:b">>))),

    ?assertEqual(<<"something">>, ersip_uri:user_param(ersip_uri:make(<<"tel:+16505550505;user=something">>))),

    ?assertError({sip_uri_expected, _}, ersip_uri:user_param(ersip_uri:make(<<"cid:+16505550505">>))),

    ?assertMatch({error, {invalid_sip_uri, _}}, ersip_uri:parse(<<"sip:b;user=&">>)),
    ?assertMatch({error, {invalid_sip_uri, _}}, ersip_uri:parse(<<"sip:b;user=">>)),
    ok.

set_user_param_test() ->
    ?assertEqual(<<"sip:b;user=ip">>, ersip_uri:assemble_bin(ersip_uri:set_user_param(ip, ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;user=phone">>, ersip_uri:assemble_bin(ersip_uri:set_user_param(phone, ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;user=something">>, ersip_uri:assemble_bin(ersip_uri:set_user_param(<<"something">>, ersip_uri:make(<<"sip:b">>)))),

    ?assertEqual(<<"tel:+16505550505;user=something">>, ersip_uri:assemble_bin(ersip_uri:set_user_param(<<"something">>, ersip_uri:make(<<"tel:+16505550505">>)))),

    ?assertError({sip_uri_expected, _}, ersip_uri:set_user_param(phone, ersip_uri:make(<<"cid:+16505550505">>))),
    ?assertError({user_param_expected, _}, ersip_uri:set_user_param(undefined, ersip_uri:make(<<"sip:b">>))),
    ?assertError({user_param_expected, _}, ersip_uri:set_user_param(99, ersip_uri:make(<<"sip:b">>))),
    ok.

clear_user_param_test() ->
    ?assertEqual(<<"sip:b">>, ersip_uri:assemble_bin(ersip_uri:clear_user_param(ersip_uri:make(<<"sip:b;user=ip">>)))),
    ?assertEqual(<<"sip:b">>, ersip_uri:assemble_bin(ersip_uri:clear_user_param(ersip_uri:make(<<"sip:b;user=phone">>)))),
    ?assertEqual(<<"sip:b">>, ersip_uri:assemble_bin(ersip_uri:clear_user_param(ersip_uri:make(<<"sip:b;user=something">>)))),

    ?assertEqual(<<"tel:+16505550505">>, ersip_uri:assemble_bin(ersip_uri:clear_user_param(ersip_uri:make(<<"tel:+16505550505;user=something">>)))),

    ?assertError({sip_uri_expected, _}, ersip_uri:clear_user_param(ersip_uri:make(<<"cid:+16505550505">>))),
    ok.

%-------------------------------------------------------------------
% ttl
%-------------------------------------------------------------------

ttl_test() ->
    ?assertEqual(1, ersip_uri:ttl(ersip_uri:make(<<"sip:b;ttl=1">>))),
    ?assertEqual(1, ersip_uri:ttl(ersip_uri:make(<<"sip:b;tt%6C=1">>))),
    ?assertEqual(1, ersip_uri:ttl(ersip_uri:make(<<"sip:b;t%74%6C=1">>))),
    ?assertEqual(1, ersip_uri:ttl(ersip_uri:make(<<"sip:b;t%74l=1">>))),

    ?assertEqual(undefined, ersip_uri:ttl(ersip_uri:make(<<"sip:b">>))),

    ?assertMatch({error, {invalid_sip_uri, _}}, ersip_uri:parse(<<"sip:b;ttl=a">>)),
    ?assertMatch({error, {invalid_sip_uri, _}}, ersip_uri:parse(<<"sip:b;ttl=-1">>)),
    ?assertMatch({error, {invalid_sip_uri, _}}, ersip_uri:parse(<<"sip:b;ttl=256">>)),

    ?assertEqual(1, ersip_uri:ttl(ersip_uri:make(<<"tel:+16505550505;ttl=1">>))),

    ?assertError({sip_uri_expected, _}, ersip_uri:ttl(ersip_uri:make(<<"cid:+16505550505">>))),
    ok.

set_ttl_test() ->
    ?assertEqual(<<"sip:b;ttl=5">>,   ersip_uri:assemble_bin(ersip_uri:set_ttl(5,   ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;ttl=255">>, ersip_uri:assemble_bin(ersip_uri:set_ttl(255, ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;ttl=0">>,   ersip_uri:assemble_bin(ersip_uri:set_ttl(0,   ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;ttl=3">>,   ersip_uri:assemble_bin(ersip_uri:set_ttl(3,   ersip_uri:make(<<"sip:b;ttl=5">>)))),

    ?assertEqual(<<"tel:+16505550505;ttl=3">>,   ersip_uri:assemble_bin(ersip_uri:set_ttl(3,   ersip_uri:make(<<"tel:+16505550505;ttl=5">>)))),

    ?assertError({sip_uri_expected, _}, ersip_uri:set_ttl(5, ersip_uri:make(<<"cid:+16505550505">>))),
    ?assertError({ttl_expected, _},     ersip_uri:set_ttl(undefined, ersip_uri:make(<<"sip:b">>))),
    ?assertError({ttl_expected, _},     ersip_uri:set_ttl(-1, ersip_uri:make(<<"sip:b">>))),
    ?assertError({ttl_expected, _},     ersip_uri:set_ttl(256, ersip_uri:make(<<"sip:b">>))),
    ok.

clear_ttl_test() ->
    ?assertEqual(<<"sip:b">>, ersip_uri:assemble_bin(ersip_uri:clear_ttl(ersip_uri:make(<<"sip:b;ttl=1">>)))),
    ?assertEqual(<<"tel:+16505550505">>, ersip_uri:assemble_bin(ersip_uri:clear_ttl(ersip_uri:make(<<"tel:+16505550505;ttl=1">>)))),

    ?assertError({sip_uri_expected, _}, ersip_uri:clear_ttl(ersip_uri:make(<<"cid:+16505550505">>))),
    ok.

%-------------------------------------------------------------------
% generic URI parameters
%-------------------------------------------------------------------

gen_param_test() ->
    ?assertEqual(<<"tcp">>,   ersip_uri:gen_param(<<"transport">>, ersip_uri:make(<<"sip:b;transport=tcp">>))),
    ?assertEqual(<<"udp">>,   ersip_uri:gen_param(<<"transport">>, ersip_uri:make(<<"sip:b;transport=udp">>))),
    ?assertEqual(<<"tls">>,   ersip_uri:gen_param(<<"transport">>, ersip_uri:make(<<"sip:b;transport=tls">>))),
    ?assertEqual(<<"sctp">>,  ersip_uri:gen_param(<<"transport">>, ersip_uri:make(<<"sip:b;transport=sctp">>))),
    ?assertEqual(<<"ws">>,    ersip_uri:gen_param(<<"transport">>, ersip_uri:make(<<"sip:b;transport=ws">>))),
    ?assertEqual(<<"wss">>,   ersip_uri:gen_param(<<"transport">>, ersip_uri:make(<<"sip:b;transport=wss">>))),
    ?assertEqual(<<"some">>,  ersip_uri:gen_param(<<"transport">>, ersip_uri:make(<<"sip:b;transport=some">>))),
    ?assertEqual(undefined,   ersip_uri:gen_param(<<"transport">>, ersip_uri:make(<<"sip:b">>))),

    ?assertEqual(<<"ip">>,        ersip_uri:gen_param(<<"user">>, ersip_uri:make(<<"sip:b;user=ip">>))),
    ?assertEqual(<<"phone">>,     ersip_uri:gen_param(<<"user">>, ersip_uri:make(<<"sip:b;user=phone">>))),
    ?assertEqual(<<"something">>, ersip_uri:gen_param(<<"user">>, ersip_uri:make(<<"sip:b;user=something">>))),
    ?assertEqual(undefined,       ersip_uri:gen_param(<<"user">>, ersip_uri:make(<<"sip:b">>))),

    ?assertEqual(<<"1">>,   ersip_uri:gen_param(<<"ttl">>, ersip_uri:make(<<"sip:b;ttl=1">>))),
    ?assertEqual(<<"1">>,   ersip_uri:gen_param(<<"ttl">>, ersip_uri:make(<<"sip:b;tt%6C=1">>))),
    ?assertEqual(<<"1">>,   ersip_uri:gen_param(<<"ttl">>, ersip_uri:make(<<"sip:b;t%74%6C=1">>))),
    ?assertEqual(<<"1">>,   ersip_uri:gen_param(<<"ttl">>, ersip_uri:make(<<"sip:b;t%74l=1">>))),
    ?assertEqual(undefined, ersip_uri:gen_param(<<"ttl">>, ersip_uri:make(<<"sip:b">>))),

    ?assertEqual(<<"1.1.1.1">>,     ersip_uri:gen_param(<<"maddr">>, ersip_uri:make(<<"sip:b;maddr=1.1.1.1">>))),
    ?assertEqual(<<"[::1]">>,       ersip_uri:gen_param(<<"maddr">>, ersip_uri:make(<<"sip:b;maddr=[::1]">>))),
    ?assertEqual(<<"atlanta.com">>, ersip_uri:gen_param(<<"maddr">>, ersip_uri:make(<<"sip:b;maddr=atlanta.com">>))),
    ?assertEqual(undefined,         ersip_uri:gen_param(<<"maddr">>, ersip_uri:make(<<"sip:b">>))),

    ?assertEqual(true, ersip_uri:gen_param(<<"lr">>, ersip_uri:make(<<"sip:b;lr">>))),

    ?assertEqual(<<"1">>,   ersip_uri:gen_param(<<"Some">>,    ersip_uri:make(<<"sip:b;Some=1">>))),
    ?assertEqual(<<"2">>,   ersip_uri:gen_param(<<"Another">>, ersip_uri:make(<<"sip:b;Some=1;Another=2">>))),
    ?assertEqual(undefined, ersip_uri:gen_param(<<"Another">>, ersip_uri:make(<<"sip:b;Some=1">>))),

    ?assertEqual(<<"tcp">>,   ersip_uri:gen_param(<<"transport">>, ersip_uri:make(<<"tel:+16505550505;transport=tcp">>))),

    ?assertError({sip_uri_expected, _}, ersip_uri:gen_param(<<"ttl">>, ersip_uri:make(<<"cid:+16505550505">>))),
    ?assertError({sip_uri_expected, _}, ersip_uri:gen_param(<<"some">>, ersip_uri:make(<<"cid:+16505550505">>))),
    ok.

set_gen_param_test() ->
    ?assertEqual(ersip_uri:make(<<"sip:carol@chicago.com;transport=udp">>),
                 ersip_uri:set_gen_param(<<"transport">>, <<"udp">>, ersip_uri:make(<<"sip:carol@chicago.com;transport=tcp">>))),
    ?assertEqual(ersip_uri:make(<<"sip:carol@chicago.com;transport=tls">>),
                 ersip_uri:set_gen_param(<<"transport">>, <<"tls">>, ersip_uri:make(<<"sip:carol@chicago.com;TRANSPORT=tcp">>))),

    ?assertEqual(ersip_uri:make(<<"sip:carol@chicago.com;lr">>),
                 ersip_uri:set_gen_param(<<"lr">>, true, ersip_uri:make(<<"sip:carol@chicago.com">>))),

    ?assertEqual(<<"sip:b;ttl=5">>,   ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"ttl">>, <<"5">>,   ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;ttl=255">>, ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"ttl">>, <<"255">>, ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;ttl=0">>,   ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"ttl">>, <<"0">>,   ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;ttl=3">>,   ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"ttl">>, <<"3">>,   ersip_uri:make(<<"sip:b;ttl=5">>)))),
    ?assertEqual(3, ersip_uri:ttl(ersip_uri:set_gen_param(<<"ttl">>, <<"3">>, ersip_uri:make(<<"sip:b;ttl=6">>)))),
    ?assertError({invalid_value, _},  ersip_uri:set_gen_param(<<"ttl">>, <<"256">>, ersip_uri:make(<<"sip:b">>))),

    ?assertEqual(<<"sip:b;user=ip">>,        ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"user">>, <<"ip">>,        ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;user=phone">>,     ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"user">>, <<"phone">>,     ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;user=something">>, ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"user">>, <<"something">>, ersip_uri:make(<<"sip:b">>)))),

    ?assertEqual(ip,    ersip_uri:user_param(ersip_uri:set_gen_param(<<"user">>, <<"ip">>,    ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(phone, ersip_uri:user_param(ersip_uri:set_gen_param(<<"user">>, <<"phone">>, ersip_uri:make(<<"sip:b">>)))),

    ?assertEqual(<<"sip:b;maddr=1.1.1.1">>,     ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"maddr">>, <<"1.1.1.1">>,     ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;maddr=[::1]">>,       ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"maddr">>, <<"[::1]">>,       ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;maddr=atlanta.com">>, ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"maddr">>, <<"atlanta.com">>, ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;maddr=1.1.1.1">>,     ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"maddr">>, <<"1.1.1.1">>,     ersip_uri:make(<<"sip:b;maddr=atlanta.com">>)))),

    ?assertEqual(<<"sip:b;some=1">>, ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"Some">>, <<"1">>, ersip_uri:make(<<"sip:b">>)))),

    ?assertEqual(<<"sip:b;lr">>, ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"lr">>, true, ersip_uri:make(<<"sip:b">>)))),
    ?assertEqual(<<"sip:b;some">>, ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"Some">>, true, ersip_uri:make(<<"sip:b">>)))),

    ?assertEqual(<<"tel:+16505550505;ttl=5">>,   ersip_uri:assemble_bin(ersip_uri:set_gen_param(<<"ttl">>, <<"5">>,   ersip_uri:make(<<"tel:+16505550505">>)))),

    ?assertError({sip_uri_expected, _}, ersip_uri:set_gen_param(<<"some">>, <<"value">>, ersip_uri:make(<<"cid:+16505550505">>))),
    ok.

clear_gen_param_test() ->
    ?assertEqual(undefined, ersip_uri:maddr     (ersip_uri:clear_gen_param(<<"maddr">>, ersip_uri:make(<<"sip:b;maddr=1.1.1.1">>)))),
    ?assertEqual(undefined, ersip_uri:ttl       (ersip_uri:clear_gen_param(<<"ttl">>, ersip_uri:make(<<"sip:b;ttl=12">>)))),
    ?assertEqual(undefined, ersip_uri:user_param(ersip_uri:clear_gen_param(<<"user">>, ersip_uri:make(<<"sip:b;user=phone">>)))),
    ?assertEqual(undefined, ersip_uri:transport (ersip_uri:clear_gen_param(<<"transport">>, ersip_uri:make(<<"sip:b;transport=tcp">>)))),

    ?assertEqual(undefined, ersip_uri:gen_param(<<"some">>, ersip_uri:clear_gen_param(<<"some">>, ersip_uri:make(<<"sip:b;some=1">>)))),

    ?assertEqual(undefined, ersip_uri:maddr     (ersip_uri:clear_gen_param(<<"maddr">>, ersip_uri:make(<<"tel:+16505550505;maddr=1.1.1.1">>)))),
    ?assertEqual(undefined, ersip_uri:ttl       (ersip_uri:clear_gen_param(<<"ttl">>, ersip_uri:make(<<"tel:+16505550505;ttl=12">>)))),
    ?assertEqual(undefined, ersip_uri:user_param(ersip_uri:clear_gen_param(<<"user">>, ersip_uri:make(<<"tel:+16505550505;user=phone">>)))),
    ?assertEqual(undefined, ersip_uri:transport (ersip_uri:clear_gen_param(<<"transport">>, ersip_uri:make(<<"tel:+16505550505;transport=tcp">>)))),

    ?assertEqual(undefined, ersip_uri:gen_param(<<"some">>, ersip_uri:clear_gen_param(<<"some">>, ersip_uri:make(<<"tel:+16505550505;some=1">>)))),

    ?assertError({sip_uri_expected, _}, ersip_uri:clear_gen_param(<<"ttl">>, ersip_uri:make(<<"cid:+16505550505">>))),
    ok.

%-------------------------------------------------------------------
% Work with headers in URI
%-------------------------------------------------------------------

rebuild_headers_lowercase_hexdigit_test() ->
    Uri = ersip_uri:make(<<"sip:carol@chicago.com?Replaces=1234123@pc99.chicago.com%3bfrom-tag=1%3bto-tag=1">>),
    RawHeaders = ersip_uri:raw_headers(Uri),
    ExpectedHeaders = [{<<"Replaces">>, <<"1234123@pc99.chicago.com%3bfrom-tag=1%3bto-tag=1">>}],
    ?assertEqual(ExpectedHeaders, RawHeaders),
    UriBin = ersip_uri:assemble_bin(ersip_uri:rebuild_header_values(Uri)),
    ?assertEqual(<<"sip:carol@chicago.com?Replaces=1234123%40pc99.chicago.com%3Bfrom-tag%3D1%3Bto-tag%3D1">>,
                 UriBin),
    ok.

raw_headers_test() ->
    Uri = ersip_uri:make(<<"sip:carol@chicago.com?a=b&c=d&e=f">>),
    RawHeaders = lists:sort(ersip_uri:raw_headers(Uri)),
    ExpectedHeaders = lists:sort([{<<"a">>, <<"b">>},
                                  {<<"c">>, <<"d">>},
                                  {<<"e">>, <<"f">>}]),
    ?assertEqual(ExpectedHeaders, RawHeaders),
    ok.

set_raw_headers_test() ->
    ?assertEqual(ersip_uri:raw_headers(ersip_uri:make(<<"sip:a@b?H=V">>)),
                 ersip_uri:raw_headers(ersip_uri:set_raw_headers([{<<"H">>, <<"V">>}], ersip_uri:make(<<"sip:a@b">>)))),
    ok.

rebuild_headers_value_test() ->
    Uri = ersip_uri:make(<<"sip:carol@chicago.com?Replaces=1234123@pc99.chicago.com%3Bfrom-tag=1%3Bto-tag=1">>),
    RawHeaders = ersip_uri:raw_headers(Uri),
    ExpectedHeaders = [{<<"Replaces">>, <<"1234123@pc99.chicago.com%3Bfrom-tag=1%3Bto-tag=1">>}],
    ?assertEqual(ExpectedHeaders, RawHeaders),
    UriBin = ersip_uri:assemble_bin(ersip_uri:rebuild_header_values(Uri)),
    ?assertEqual(<<"sip:carol@chicago.com?Replaces=1234123%40pc99.chicago.com%3Bfrom-tag%3D1%3Bto-tag%3D1">>,
                 UriBin),

    TelUri = ersip_uri:make(<<"tel:1234">>),
    TelUriBin = ersip_uri:assemble_bin(ersip_uri:rebuild_header_values(TelUri)),
    ?assertEqual(<<"tel:1234">>, TelUriBin),

    NoHdrUri = ersip_uri:make(<<"sip:carol@chicago.com?a">>),
    NoHdrUriBin = ersip_uri:assemble_bin(ersip_uri:rebuild_header_values(NoHdrUri)),
    ?assertEqual(<<"sip:carol@chicago.com?a=">>, NoHdrUriBin),
    ok.

%-------------------------------------------------------------------
% other tests
%-------------------------------------------------------------------

make_test() ->
    ?assertError({invalid_host, _},   ersip_uri:make([{host, {hostname, <<"-ab">>}}])),
    ?assertError({invalid_part, _},   ersip_uri:make([{x, {user, <<"a-b">>}}])),
    ?assertError({invalid_scheme, _}, ersip_uri:make(<<"x">>)),
    ok.

uri_make_key_test() ->
    {ok, ExpectedURI} = ersip_uri:parse(<<"sips:Alice@atlanta.com:8083">>),
    ?assertEqual(ersip_uri:make_key(ExpectedURI),
                 ersip_uri:make([{scheme, sips},
                                 {user, <<"Alice">>},
                                 {host, {hostname, <<"atlanta.com">>}},
                                 {port, 8083}])),
    {ok, ExpectedURI2} = ersip_uri:parse(<<"sip:Alice@atlanta.com:5061">>),
    ?assertEqual(ersip_uri:make_key(ExpectedURI2),
                 ersip_uri:make([{scheme, sip},
                                 {user, <<"Alice">>},
                                 {host, {hostname, <<"atlanta.com">>}},
                                 {port, 5061}])),
    {ok, ExpectedURI3} = ersip_uri:parse(<<"tel:+16505550505">>),
    ?assertEqual(ersip_uri:make_key(ExpectedURI3),
        ersip_uri:make([{scheme, tel}, {user, <<"+16505550505">>}])),

    ?assertError({cannot_make_key, _}, ersip_uri:make_key(ersip_uri:make(<<"cid:+16505550505">>))),
    ok.

uri_compare_test() ->
    %% RFC 3261 test cases:

    %% The URIs within each of the following sets are equivalent:
    %%
    %% sip:%61lice@atlanta.com;transport=TCP
    %% sip:alice@AtLanTa.CoM;Transport=tcp
    ?assertEqual(make_key(<<"sip:%61lice@atlanta.com;transport=TCP">>),
                 make_key(<<"sip:alice@AtLanTa.CoM;Transport=tcp">>)),

    %% sip:carol@chicago.com
    %% sip:carol@chicago.com;newparam=5
    %% sip:carol@chicago.com;security=on
    ?assertEqual(make_key(<<"sip:carol@chicago.com">>),
                 make_key(<<"sip:carol@chicago.com;newparam=5">>)),
    ?assertEqual(make_key(<<"sip:carol@chicago.com">>),
                 make_key(<<"sip:carol@chicago.com;security=on">>)),
    ?assertEqual(make_key(<<"sip:carol@chicago.com;newparam=5">>),
                 make_key(<<"sip:carol@chicago.com;security=on">>)),

    %% sip:biloxi.com;transport=tcp;method=REGISTER?to=sip:bob%40biloxi.com
    %% sip:biloxi.com;method=REGISTER;transport=tcp?to=sip:bob%40biloxi.com
    ?assertEqual(make_key(<<"sip:biloxi.com;transport=tcp;method=REGISTER?to=sip:bob%40biloxi.com">>),
                 make_key(<<"sip:biloxi.com;method=REGISTER;transport=tcp?to=sip:bob%40biloxi.com">>)),

    %% sip:alice@atlanta.com?subject=project%20x&priority=urgent
    %% sip:alice@atlanta.com?priority=urgent&subject=project%20x
    ?assertEqual(make_key(<<"sip:alice@atlanta.com?subject=project%20x&priority=urgent">>),
                 make_key(<<"sip:alice@atlanta.com?priority=urgent&subject=project%20x">>)),

    ?assertEqual(make_key(<<"sip:1.1.1.1;transport=tcp">>),
                 make_key(<<"sip:1.1.1.1;transport=tcp">>)),
    ?assertEqual(make_key(<<"sip:[::1];transport=tcp">>),
                 make_key(<<"sip:[::1];transport=tcp">>)),
    ?assertEqual(make_key(<<"sip:[::1]:5060;transport=tcp">>),
                 make_key(<<"sip:[::1]:5060;transport=tcp">>)),

    %% The URIs within each of the following sets are not equivalent:

    %% SIP:ALICE@AtLanTa.CoM;Transport=udp             (different usernames)
    %% sip:alice@AtLanTa.CoM;Transport=UDP
    ?assertNotEqual(make_key(<<"SIP:ALICE@AtLanTa.CoM;Transport=udp">>),
                    make_key(<<"sip:alice@AtLanTa.CoM;Transport=UDP">>)),

    %% sip:bob@biloxi.com                   (can resolve to different ports)
    %% sip:bob@biloxi.com:5060
    ?assertNotEqual(make_key(<<"sip:bob@biloxi.com">>),
                    make_key(<<"sip:bob@biloxi.com:5060">>)),


    %% sip:bob@biloxi.com              (can resolve to different transports)
    %% sip:bob@biloxi.com;transport=udp
    ?assertNotEqual(make_key(<<"sip:bob@biloxi.com">>),
                    make_key(<<"sip:bob@biloxi.com;transport=udp">>)),

    %% sip:bob@biloxi.com     (can resolve to different port and transports)
    %% sip:bob@biloxi.com:6000;transport=tcp
    ?assertNotEqual(make_key(<<"sip:bob@biloxi.com">>),
                    make_key(<<"sip:bob@biloxi.com:6000;transport=tcp">>)),

    %% sip:carol@chicago.com                    (different header component)
    %% sip:carol@chicago.com?Subject=next%20meeting
    ?assertNotEqual(make_key(<<"sip:carol@chicago.com">>),
                    make_key(<<"sip:carol@chicago.com?Subject=next%20meeting">>)),

    %% sip:bob@phone21.boxesbybob.com   (even though that's what
    %% sip:bob@192.0.2.4                 phone21.boxesbybob.com resolves to)

    ?assertEqual(make_key(<<"sip:biloxi.com;transport=tcp;method=REGISTER?to">>),
                 make_key(<<"sip:biloxi.com;method=REGISTER;transport=tcp?to">>)),
    ok.

uri_assemeble_test() ->
    reassemble_check(<<"sip:1.1.1.1;transport=tcp">>),
    reassemble_check(<<"sips:%20@b:5090">>),
    reassemble_check(<<"sip:b;transport=wss">>),
    reassemble_check(<<"sip:b;maddr=1.1.1.1">>),
    reassemble_check(<<"sip:b;ttl=1">>),
    reassemble_check(<<"sip:b;lr">>),
    reassemble_check(<<"sip:b;user=phone">>),
    reassemble_check(<<"sip:b;user=ip">>),
    reassemble_check(<<"sip:b;user=Some">>),
    reassemble_check(<<"sip:b;myparam=Param">>),
    reassemble_check(<<"sip:b;myparam">>),
    reassemble_check(<<"tel:b;myparam">>),
    reassemble_check(<<"sip:b?a=b">>),
    reassemble_check(<<"sip:b;a=b?a=b">>),
    reassemble_check(<<"sip:b;a?a=">>),
    reassemble_check(<<"sip:B">>),
    reassemble_check(<<"sip:[::1]">>),
    reassemble_check(<<"sip:[FDFB:6E63:7442:92B6:CC1A:DBE6:D33B:DE78]">>),
    reassemble_check(<<"sip:[fdfb:6E63:7442:92b6:CC1A:dbe6:D33B:de78]">>),
    ok.

uri_set_host_test() ->
    URI0 = ersip_uri:make(<<"sip:[FDFB:6E63:7442:92B6:CC1A:DBE6:D33B:DE78]">>),
    URI1 = ersip_uri:set_host(ersip_host:make(<<"biloxi.com">>), URI0),
    ?assertEqual(<<"sip:biloxi.com">>, ersip_uri:assemble_bin(URI1)),
    URI1 = ersip_uri:set_host(ersip_host:make(<<"biloxi.com">>), URI1),
    ok.

host_bin_test() ->
    URI0 = ersip_uri:make(<<"sip:[FDFB:6E63:7442:92B6:CC1A:DBE6:D33B:DE78]">>),
    URI1 = ersip_uri:set_host(ersip_host:make(<<"biloxi.com">>), URI0),
    ?assertEqual(<<"[FDFB:6E63:7442:92B6:CC1A:DBE6:D33B:DE78]">>, ersip_uri:host_bin(URI0)),
    ?assertEqual(<<"biloxi.com">>, ersip_uri:host_bin(URI1)),
    ?assertError({sip_uri_expected, _}, ersip_uri:host_bin(ersip_uri:make(<<"tel:+16505550505">>))),
    ok.

get_parts_test() ->
    URI = ersip_uri:make(<<"sip:bob@1.1.1.1:5091">>),
    Scheme = {scheme, sip},
    User   = {user, <<"bob">>},
    Host   = {host, ersip_host:make(<<"1.1.1.1">>)},
    Port   = {port, 5091},
    ?assertEqual(Scheme, ersip_uri:get(scheme, URI)),
    ?assertEqual(User,   ersip_uri:get(user, URI)),
    ?assertEqual(Host,   ersip_uri:get(host, URI)),
    ?assertEqual(Port,   ersip_uri:get(port, URI)),
    ?assertEqual([Scheme, Port, Host], ersip_uri:get([scheme, port, host], URI)),
    ok.

user_manip_test() ->
    URIBob = ersip_uri:make(<<"sip:bob@1.1.1.1:5091">>),
    URIAlice = ersip_uri:set_user(<<"alice">>, URIBob),
    ?assertEqual(<<"alice">>, ersip_uri:user(URIAlice)),
    ?assertEqual(<<"sip:alice@1.1.1.1:5091">>, ersip_uri:assemble_bin(URIAlice)),
    ok.

user_manip_tel_test() ->
    URITel1 = ersip_uri:make(<<"tel:+16505550505">>),
    URITel2 = ersip_uri:set_user(<<"+16505550506">>, URITel1),
    ?assertEqual(<<"+16505550506">>, ersip_uri:user(URITel2)),
    ?assertEqual(<<"tel:+16505550506">>, ersip_uri:assemble_bin(URITel2)),
    ok.

host_manip_test() ->
    URIBob = ersip_uri:make(<<"sip:bob@1.1.1.1:5091">>),
    Biloxi = ersip_host:make(<<"biloxi.com">>),
    URIBob@Biloxi = ersip_uri:set_host(Biloxi, URIBob),
    ?assertEqual(Biloxi, ersip_uri:host(URIBob@Biloxi)),
    ?assertEqual(<<"sip:bob@biloxi.com:5091">>, ersip_uri:assemble_bin(URIBob@Biloxi)),
    ok.

host_only_for_sip_test() ->
    ?assertError({sip_uri_expected, _}, ersip_uri:host(ersip_uri:make(<<"tel:+16505550505">>))),
    ?assertError({sip_uri_expected, _}, ersip_uri:set_host(ersip_host:make(<<"biloxi.com">>),
                                                           ersip_uri:make(<<"tel:+16505550505">>))),
    ok.

port_manip_test() ->
    URIBob5091 = ersip_uri:make(<<"sip:bob@1.1.1.1:5091">>),
    URIBob5092 = ersip_uri:set_port(5092, URIBob5091),
    ?assertEqual(5092, ersip_uri:port(URIBob5092)),
    ?assertEqual(<<"sip:bob@1.1.1.1:5092">>, ersip_uri:assemble_bin(URIBob5092)),
    URIBobDef = ersip_uri:set_port(undefined, URIBob5091),
    ?assertEqual(undefined, ersip_uri:port(URIBobDef)),
    ?assertEqual(<<"sip:bob@1.1.1.1">>, ersip_uri:assemble_bin(URIBobDef)),
    ok.

port_only_for_sip_test() ->
    ?assertError({sip_uri_expected, _}, ersip_uri:port(ersip_uri:make(<<"tel:+16505550505">>))),
    ?assertError({sip_uri_expected, _}, ersip_uri:set_port(5060, ersip_uri:make(<<"tel:+16505550505">>))),
    ok.

parse_three_params_test() ->
    Uri = ersip_uri:make(<<"sip:carol@chicago.com;param1=value1;param2=value2;param3=value3">>),
    Params = #{<<"param1">> => <<"value1">>,
               <<"param2">> => <<"value2">>,
               <<"param3">> => <<"value3">>},
    ?assertEqual(Params, ersip_uri:params(Uri)).

params_on_non_sip_test() ->
    Uri = ersip_uri:make(<<"tel:+16505550505">>),
    ?assertEqual(#{}, ersip_uri:params(Uri)),
    ok.

raw_params_test() ->
    Uri = ersip_uri:make(<<"sip:carol@chicago.com;ttl=1;lr;c;param2=value2;param3=value3;maddr=1.1.1.1">>),
    RawParams = lists:sort(ersip_uri:raw_params(Uri)),
    ExpectedParams = lists:sort([{<<"ttl">>, <<"1">>},
                                 <<"lr">>, <<"c">>,
                                 {<<"param2">>, <<"value2">>},
                                 {<<"maddr">>, <<"1.1.1.1">>},
                                 {<<"param3">>, <<"value3">>}]),
    ?assertEqual(ExpectedParams, RawParams),
    ok.


data_test() ->
    ?assertEqual(<<"carol@chicago.com?a=b">>,    ersip_uri:data(ersip_uri:make(<<"sip:carol@chicago.com?a=b">>))),
    ?assertEqual(<<"carol@chicago.com;newparam=5">>, ersip_uri:data(ersip_uri:make(<<"sip:carol@chicago.com;newparam=5">>))),
    ?assertEqual(<<"12345">>, ersip_uri:data(ersip_uri:make(<<"tel:12345">>))),
    ok.


is_sip_test() ->
    ?assertEqual(true, ersip_uri:is_sip(ersip_uri:make(<<"sip:a@b">>))),
    ?assertEqual(false, ersip_uri:is_sip(ersip_uri:make(<<"tel:11234">>))),
    ok.

raw_test() ->
    ?assertMatch(#{scheme := <<"sip">>,
                   data := <<"a@b">>,
                   sip := #{user := <<"a">>,
                            host := <<"b">>}
                  }, ersip_uri:raw(ersip_uri:make(<<"sip:a@b">>))),
    ?assertMatch(#{sip := #{params := #{<<"ttl">> := <<"255">>}}}, ersip_uri:raw(ersip_uri:make(<<"sip:a@b;ttl=255">>))),
    ?assertMatch(#{sip := #{params := #{<<"lr">> := _}}}, ersip_uri:raw(ersip_uri:make(<<"sip:b;lr">>))),
    ?assertMatch(#{sip := #{headers := [{<<"Replaces">>, <<"1234123%40pc99.chicago.com%3Bfrom-tag%3D1%3Bto-tag%3D1">>}]}},
                 ersip_uri:raw(ersip_uri:make(<<"sip:carol@chicago.com?Replaces=1234123%40pc99.chicago.com%3Bfrom-tag%3D1%3Bto-tag%3D1">>))),

    ?assertMatch(#{scheme := <<"tel">>, data := <<"+16505550505">>}, ersip_uri:raw(ersip_uri:make(<<"tel:+16505550505">>))),
    ok.

make_from_raw_test() ->
    SIPURI = ersip_uri:make(<<"sip:a@b">>),
    ?assertEqual(SIPURI, ersip_uri:make(#{scheme => <<"sip">>, data => <<"a@b">>})),
    TelURI = ersip_uri:make(<<"tel:+16505550505">>),
    ?assertEqual(TelURI, ersip_uri:make(#{scheme => <<"tel">>, data => <<"+16505550505">>})),
    ok.

clear_not_allowed_parts_test() ->
    ?assertEqual(<<"sip:a@b">>, clear_not_allowed_parts(ruri, <<"sip:a@b?a=b">>)),
    ?assertEqual(<<"sip:a@b">>, clear_not_allowed_parts(ruri, <<"sip:a@b;method=INFO">>)),
    ?assertEqual(<<"tel:+16505550505">>, clear_not_allowed_parts(ruri, <<"tel:+16505550505">>)),

    ?assertEqual(<<"sip:a@b">>, clear_not_allowed_parts(record_route, <<"sip:a@b?a=b">>)),
    ?assertEqual(<<"sip:a@b">>, clear_not_allowed_parts(record_route, <<"sip:a@b;method=INFO">>)),
    ?assertEqual(<<"sip:a@b">>, clear_not_allowed_parts(record_route, <<"sip:a@b;ttl=10">>)),
    ?assertEqual(<<"tel:+16505550505">>, clear_not_allowed_parts(record_route, <<"tel:+16505550505">>)),
    ok.

clear_params_test() ->
    ?assertEqual(<<"sip:a@b">>, clear_params(<<"sip:a@b;a=b">>)),
    ?assertEqual(<<"sip:a@b">>, clear_params(<<"sip:a@b;a=b;c=d">>)),
    ?assertEqual(<<"sip:a@b">>, clear_params(<<"sip:a@b;ttl=1">>)),
    ?assertEqual(<<"tel:+16505550505">>, clear_params(<<"tel:+16505550505">>)),
    ok.


%%===================================================================
%% Helpers
%%===================================================================

make_key(Bin) ->
    {ok, URI} = ersip_uri:parse(Bin),
    ersip_uri:make_key(URI).

reassemble_check(Bin) ->
    {ok, URI} = ersip_uri:parse(Bin),
    BinAssembled = iolist_to_binary(ersip_uri:assemble(URI)),
    {ok, _} = ersip_uri:parse(BinAssembled),
    ?assertEqual(Bin, BinAssembled).

parse_ok(URIBin, Raw) ->
    {ok, URI} = ersip_uri:parse(URIBin),
    ?assertEqual(Raw, ersip_uri:raw(URI)).

parse_sip_ok(URIBin, ExpectedRawSIP) ->
    {ok, URI} = ersip_uri:parse(URIBin),
    #{scheme := <<"sip">>, sip := ResultSIP} = ersip_uri:raw(URI),
    ?assertEqual(ExpectedRawSIP, ResultSIP).

parse_sips_ok(URIBin, ExpectedRawSIP) ->
    {ok, URI} = ersip_uri:parse(URIBin),
    #{scheme := <<"sips">>, sip := ResultSIP} = ersip_uri:raw(URI),
    ?assertEqual(ExpectedRawSIP, ResultSIP).

parse_tel_ok(URIBin, ExpectedRawSIP) ->
    {ok, URI} = ersip_uri:parse(URIBin),
    #{scheme := <<"tel">>, sip := ResultSIP} = ersip_uri:raw(URI),
    ?assertEqual(ExpectedRawSIP, ResultSIP).

check_transport(URIBin, Transport) ->
    ?assertMatch({transport, Transport}, ersip_uri:transport(ersip_uri:make(URIBin))).

check_other_transport(URIBin, Transport) ->
    ?assertMatch({other_transport, Transport}, ersip_uri:transport(ersip_uri:make(URIBin))).

clear_not_allowed_parts(Type, UriBIN) ->
    ersip_uri:assemble_bin(ersip_uri:clear_not_allowed_parts(Type, ersip_uri:make(UriBIN))).

clear_params(UriBIN) ->
    ersip_uri:assemble_bin(ersip_uri:clear_params(ersip_uri:make(UriBIN))).



