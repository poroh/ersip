%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message parser tests
%%

-module(ersip_uri_test).

-include_lib("eunit/include/eunit.hrl").
-include("ersip_uri.hrl").

uri_test() ->
    ?assertEqual(
       { ok, #uri{ user = { user, <<"a">> },
                   host = { hostname, <<"b">> },
                   port = 5090 } },
       ersip_uri:parse(<<"sip:a@b:5090">>)),

    ?assertEqual(
       { ok, #uri{ scheme = sips,
                   user = { user, <<"a">> },
                   host = { hostname, <<"b">> },
                   port = 5090 } },
       ersip_uri:parse(<<"sips:a@b:5090">>)),

    ?assertEqual(
       { ok, #uri{ scheme = sips,
                   user = { user, <<"a:b">> },
                   host = { hostname, <<"b">> },
                   port = 5090 } },
       ersip_uri:parse(<<"sips:a:b@b:5090">>)),

    ?assertEqual(
       { ok, #uri{ scheme = sips,
                   user = { user, <<"a:%20">> },
                   host = { hostname, <<"b">> },
                   port = 5090 } },
       ersip_uri:parse(<<"sips:a:%20@b:5090">>)),

    ?assertEqual(
       { ok, #uri{ scheme = sips,
                   user = { user, <<"%20">> },
                   host = { hostname, <<"b">> },
                   port = 5090 } },
       ersip_uri:parse(<<"sips:%20@b:5090">>)),

    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"?:a@b:5090">>)),

    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:%@b:5090">>)),
    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:@b:5090">>)),
    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip::a@b:5090">>)),
    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:a:%@b:5090">>)),

    ?assertEqual(
       { ok, #uri{ user = undefined,
                   host = { hostname, <<"b">> },
                   port = 5090 } },
       ersip_uri:parse(<<"sip:b:5090">>)),


    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> } } },
       ersip_uri:parse(<<"sip:b">>)),

    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:%:5090">>)),
    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:%">>)),
    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:a.-">>)),
    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:b:x">>)),

    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ transport => { transport, tcp } } } },
       ersip_uri:parse(<<"sip:b;transport=tcp">>)),
    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ transport => { transport, udp } } } },
       ersip_uri:parse(<<"sip:b;transport=udp">>)),
    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ transport => { transport, tls } } } },
       ersip_uri:parse(<<"sip:b;transport=tls">>)),
    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ transport => { transport, ws } } } },
       ersip_uri:parse(<<"sip:b;transport=ws">>)),
    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ transport => { transport, wss } } } },
       ersip_uri:parse(<<"sip:b;transport=wss">>)),
    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ transport => { other_transport, <<"wssnew">> } } } },
       ersip_uri:parse(<<"sip:b;transport=wssnew">>)),

    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:b;transport=&">>)),
    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:b;transport=&;user=phone">>)),

    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ lr => true } } },
       ersip_uri:parse(<<"sip:b;lr">>)),

    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ maddr => {ipv4, {1,1,1,1} } } }},
       ersip_uri:parse(<<"sip:b;maddr=1.1.1.1">>)),

    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:b;maddr=&">>)),

    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ user => phone } }},
       ersip_uri:parse(<<"sip:b;user=phone">>)),

    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ user => ip } }},
       ersip_uri:parse(<<"sip:b;user=ip">>)),

    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ user => <<"something">> } }},
       ersip_uri:parse(<<"sip:b;user=something">>)),

    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:b;user=&">>)),
    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:b;user=">>)),

    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ ttl => 1 } }},
       ersip_uri:parse(<<"sip:b;ttl=1">>)),

    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:b;ttl=a">>)),
    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:b;ttl=-1">>)),
    ?assertMatch({ error, { einval, _ } }, ersip_uri:parse(<<"sip:b;ttl=256">>)),

    ?assertEqual(
       { ok, #uri{ host = { hostname, <<"b">> }, params = #{ <<"some">> => <<"1">> } }},
       ersip_uri:parse(<<"sip:b;Some=1">>)),

    ok.

uri_make_test() ->
    { ok, ExpectedURI } = ersip_uri:parse(<<"sips:Alice@atlanta.com:8083">>),
    ?assertEqual(ExpectedURI,
                 ersip_uri:make([ sips,
                                  { user, <<"Alice">> },
                                  { host, { hostname, <<"atlanta.com">> } },
                                  { port, 8083 } ])),
    { ok, ExpectedURI2 } = ersip_uri:parse(<<"sip:Alice@atlanta.com:5061">>),
    ?assertEqual(ExpectedURI2,
                 ersip_uri:make([ sip,
                                  { user, <<"Alice">> },
                                  { host, { hostname, <<"atlanta.com">> } },
                                  { port, 5061 } ])),
    ?assertError(badarg, ersip_uri:make([ { host, { hostname, <<"a-b">> } } ])),
    ?assertError(badarg, ersip_uri:make([ { x, { user, <<"a-b">> } } ])).
    
