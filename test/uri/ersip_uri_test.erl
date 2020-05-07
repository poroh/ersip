%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message parser tests
%%

-module(ersip_uri_test).

-include_lib("eunit/include/eunit.hrl").
-include("ersip_uri.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

uri_test() ->
    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           user = {user, <<"a">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}
       },
       ersip_uri:parse(<<"sip:a@b:5090">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           user = {user, <<"a">>},
                           host = {ipv4, {1, 2, 3, 4}},
                           port = 5090}}},
       ersip_uri:parse(<<"sip:a@1.2.3.4:5090">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           user = {user, <<"a">>},
                           host = {ipv6, {0, 0, 0, 0, 0, 0, 0, 1}},
                           port = 5090}}},
        ersip_uri:parse(<<"sip:a@[::1]:5090">>)),

    ?assertMatch(
       {ok, #uri{scheme = {scheme, sips},
                 data = #sip_uri_data{
                           user = {user, <<"a">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sips:a@b:5090">>)),

    ?assertMatch(
       {ok, #uri{scheme = {scheme, sips},
                 data = #sip_uri_data{
                           user = {user, <<"a:b">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sips:a:b@b:5090">>)),

    ?assertMatch(
       {ok, #uri{scheme = {scheme, sips},
                 data = #sip_uri_data{
                           user = {user, <<"a:%20">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sips:a:%20@b:5090">>)),

    ?assertMatch(
       {ok, #uri{scheme =  {scheme, sips},
                 data = #sip_uri_data{
                           user = {user, <<"%20">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sips:%20@b:5090">>)),

    ?assertMatch(
       {ok, #uri{scheme =  {scheme, <<"TEL">>},
                 data = #absolute_uri_data{opaque = <<"%20@b:5090">>}}},
       ersip_uri:parse(<<"TEL:%20@b:5090">>)),



    ?assertMatch(
       {ok, #uri{scheme = {scheme, sip},
                 data = #sip_uri_data{
                           user = {user, <<"%25">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sip:%@b:5090">>)),

    ?assertMatch({error, {invalid_scheme, _}}, ersip_uri:parse(<<"?:a@b:5090">>)),

    ?assertMatch({error, empty_username}, ersip_uri:parse(<<"sip:@b:5090">>)),
    ?assertMatch({error, empty_username}, ersip_uri:parse(<<"sip::a@b:5090">>)),
    ?assertMatch({error, {bad_password, _}}, ersip_uri:parse(<<"sip:a:%@b:5090">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           user = undefined,
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sip:b:5090">>)),


    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{host = {hostname, <<"b">>}}}},
       ersip_uri:parse(<<"sip:b">>)),

    ?assertMatch({error, {invalid_hostport, _}}, ersip_uri:parse(<<"sip:%:5090">>)),
    ?assertMatch({error, {invalid_hostport, _}}, ersip_uri:parse(<<"sip:%">>)),
    ?assertMatch({error, {invalid_hostport, _}}, ersip_uri:parse(<<"sip:a.-">>)),
    ?assertMatch({error, {invalid_hostport, _}}, ersip_uri:parse(<<"sip:b:x">>)),
    ?assertMatch({error, {invalid_ipv6_reference, _}}, ersip_uri:parse(<<"sip:[::1">>)),
    ?assertMatch({error, {invalid_port, _}}, ersip_uri:parse(<<"sip:[::1]:">>)),
    ?assertMatch({error, {invalid_port, _}}, ersip_uri:parse(<<"sip:[::1]x">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport := {transport, tcp}}}}},
       ersip_uri:parse(<<"sip:b;transport=tcp">>)),
   ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport := {transport, sctp}}}}},
       ersip_uri:parse(<<"sip:b;transport=sctp">>)),
    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport := {transport, udp}}}}},
       ersip_uri:parse(<<"sip:b;transport=udp">>)),
    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport := {transport, tls}}}}},
       ersip_uri:parse(<<"sip:b;transport=tls">>)),
    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport := {transport, ws}}}}},
       ersip_uri:parse(<<"sip:b;transport=ws">>)),
    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport := {transport, wss}}}}},
       ersip_uri:parse(<<"sip:b;transport=wss">>)),
    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport := {other_transport, <<"wssnew">>}}}}},
       ersip_uri:parse(<<"sip:b;transport=wssnew">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;transport=&">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;transport=&;user=phone">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{lr := true}}}},
       ersip_uri:parse(<<"sip:b;lr">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{maddr := {ipv4, {1,1,1,1}}}}}},
       ersip_uri:parse(<<"sip:b;maddr=1.1.1.1">>)),

    ?assertMatch({error, {invalid_maddr, _}}, ersip_uri:parse(<<"sip:b;maddr=&">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
               host = {hostname, <<"b">>},
               params = #{user := phone}}}},
       ersip_uri:parse(<<"sip:b;user=phone">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{user := ip}}}},
       ersip_uri:parse(<<"sip:b;user=ip">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{user := <<"something">>}}}},
       ersip_uri:parse(<<"sip:b;user=something">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;user=&">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;user=">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{ttl := 1}}}},
       ersip_uri:parse(<<"sip:b;ttl=1">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{ttl := 1}}}},
       ersip_uri:parse(<<"sip:b;tt%6C=1">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{ttl := 1}}}},
       ersip_uri:parse(<<"sip:b;t%74%6C=1">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{ttl := 1}}}},
       ersip_uri:parse(<<"sip:b;t%74l=1">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;ttl=a">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;ttl=-1">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;ttl=256">>)),

    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
               host = {hostname, <<"b">>},
               params = #{<<"some">> := <<"1">>}}}},
       ersip_uri:parse(<<"sip:b;Some=1">>)),
    ?assertMatch(
       {ok, #uri{data = #sip_uri_data{
               host = {hostname, <<"b">>},
               headers = #{<<"Some">> := <<"1">>,
                           <<"Another">> := <<"2">>}}}},
       ersip_uri:parse(<<"sip:b?Some=1&Another=2">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"a@b">>)),
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
    ?assertError({invalid_host, _}, ersip_uri:make([{host, {hostname, <<"-ab">>}}])),
    ?assertError({invalid_part, _}, ersip_uri:make([{x, {user, <<"a-b">>}}])),
    ?assertError({error, _}, ersip_uri:make(<<"x">>)).

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
    ok.

host_bin_test() ->
    URI0 = ersip_uri:make(<<"sip:[FDFB:6E63:7442:92B6:CC1A:DBE6:D33B:DE78]">>),
    URI1 = ersip_uri:set_host(ersip_host:make(<<"biloxi.com">>), URI0),
    ?assertEqual(<<"[FDFB:6E63:7442:92B6:CC1A:DBE6:D33B:DE78]">>, ersip_uri:host_bin(URI0)),
    ?assertEqual(<<"biloxi.com">>, ersip_uri:host_bin(URI1)),
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

host_manip_test() ->
    URIBob = ersip_uri:make(<<"sip:bob@1.1.1.1:5091">>),
    Biloxi = ersip_host:make(<<"biloxi.com">>),
    URIBob@Biloxi = ersip_uri:set_host(Biloxi, URIBob),
    ?assertEqual(Biloxi, ersip_uri:host(URIBob@Biloxi)),
    ?assertEqual(<<"sip:bob@biloxi.com:5091">>, ersip_uri:assemble_bin(URIBob@Biloxi)),
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

parse_three_params_test() ->
    Uri = ersip_uri:make(<<"sip:carol@chicago.com;param1=value1;param2=value2;param3=value3">>),
    Params = #{<<"param1">> => <<"value1">>,
               <<"param2">> => <<"value2">>,
               <<"param3">> => <<"value3">>},
    ?assertEqual(Params, ersip_uri:params(Uri)).

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

raw_headers_test() ->
    Uri = ersip_uri:make(<<"sip:carol@chicago.com?a=b&c=d&e=f">>),
    RawHeaders = lists:sort(ersip_uri:raw_headers(Uri)),
    ExpectedHeaders = lists:sort([{<<"a">>, <<"b">>},
                                  {<<"c">>, <<"d">>},
                                  {<<"e">>, <<"f">>}]),
    ?assertEqual(ExpectedHeaders, RawHeaders),
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

rebuild_headers_lowercase_hexdigit_test() ->
    Uri = ersip_uri:make(<<"sip:carol@chicago.com?Replaces=1234123@pc99.chicago.com%3bfrom-tag=1%3bto-tag=1">>),
    RawHeaders = ersip_uri:raw_headers(Uri),
    ExpectedHeaders = [{<<"Replaces">>, <<"1234123@pc99.chicago.com%3bfrom-tag=1%3bto-tag=1">>}],
    ?assertEqual(ExpectedHeaders, RawHeaders),
    UriBin = ersip_uri:assemble_bin(ersip_uri:rebuild_header_values(Uri)),
    ?assertEqual(<<"sip:carol@chicago.com?Replaces=1234123%40pc99.chicago.com%3Bfrom-tag%3D1%3Bto-tag%3D1">>,
                 UriBin),
    ok.

data_test() ->
    ?assertEqual(<<"carol@chicago.com?a=b">>,    ersip_uri:data(ersip_uri:make(<<"sip:carol@chicago.com?a=b">>))),
    ?assertEqual(<<"carol@chicago.com;newparam=5">>, ersip_uri:data(ersip_uri:make(<<"sip:carol@chicago.com;newparam=5">>))),
    ?assertEqual(<<"12345">>, ersip_uri:data(ersip_uri:make(<<"tel:12345">>))),
    ok.

clear_transport_test() ->
    ?assertEqual(ersip_uri:make(<<"sip:carol@chicago.com">>),
                 ersip_uri:clear_transport(ersip_uri:make(<<"sip:carol@chicago.com;transport=tcp">>))),
    ?assertEqual(ersip_uri:make(<<"sip:carol@chicago.com">>),
                 ersip_uri:clear_transport(ersip_uri:make(<<"sip:carol@chicago.com;TRANSPORT=tcp">>))),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================
make_key(Bin) ->
    {ok, URI} = ersip_uri:parse(Bin),
    ersip_uri:make_key(URI).

reassemble_check(Bin) ->
    {ok, URI} = ersip_uri:parse(Bin),
    BinAssembled = iolist_to_binary(ersip_uri:assemble(URI)),
    {ok, _} = ersip_uri:parse(BinAssembled),
    ?assertEqual(Bin, BinAssembled).
