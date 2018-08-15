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
    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           user = {user, <<"a">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}
       },
       ersip_uri:parse(<<"sip:a@b:5090">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           user = {user, <<"a">>},
                           host = {ipv4, {1, 2, 3, 4}},
                           port = 5090}}},
       ersip_uri:parse(<<"sip:a@1.2.3.4:5090">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           user = {user, <<"a">>},
                           host = {ipv6, {0, 0, 0, 0, 0, 0, 0, 1}},
                           port = 5090}}},
        ersip_uri:parse(<<"sip:a@[::1]:5090">>)),

    ?assertEqual(
       {ok, #uri{scheme = {scheme, sips},
                 data = #sip_uri_data{
                           user = {user, <<"a">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sips:a@b:5090">>)),

    ?assertEqual(
       {ok, #uri{scheme = {scheme, sips},
                 data = #sip_uri_data{
                           user = {user, <<"a:b">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sips:a:b@b:5090">>)),

    ?assertEqual(
       {ok, #uri{scheme = {scheme, sips},
                 data = #sip_uri_data{
                           user = {user, <<"a:%20">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sips:a:%20@b:5090">>)),

    ?assertEqual(
       {ok, #uri{scheme =  {scheme, sips},
                 data = #sip_uri_data{
                           user = {user, <<"%20">>},
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sips:%20@b:5090">>)),

    ?assertEqual(
       {ok, #uri{scheme =  {scheme, <<"TEL">>},
                 data = #absolute_uri_data{opaque = <<"%20@b:5090">>}}},
       ersip_uri:parse(<<"TEL:%20@b:5090">>)),

    ?assertMatch({error, {invalid_scheme, _}}, ersip_uri:parse(<<"?:a@b:5090">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:%@b:5090">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:@b:5090">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip::a@b:5090">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:a:%@b:5090">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           user = undefined,
                           host = {hostname, <<"b">>},
                           port = 5090}}},
       ersip_uri:parse(<<"sip:b:5090">>)),


    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{host = {hostname, <<"b">>}}}},
       ersip_uri:parse(<<"sip:b">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:%:5090">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:%">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:a.-">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b:x">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:[::1">>)),
    ?assertMatch({error, {invalid_port, _}}, ersip_uri:parse(<<"sip:[::1]:">>)),
    ?assertMatch({error, {invalid_port, _}}, ersip_uri:parse(<<"sip:[::1]x">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport => {transport, tcp}}}}},
       ersip_uri:parse(<<"sip:b;transport=tcp">>)),
    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport => {transport, udp}}}}},
       ersip_uri:parse(<<"sip:b;transport=udp">>)),
    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport => {transport, tls}}}}},
       ersip_uri:parse(<<"sip:b;transport=tls">>)),
    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport => {transport, ws}}}}},
       ersip_uri:parse(<<"sip:b;transport=ws">>)),
    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport => {transport, wss}}}}},
       ersip_uri:parse(<<"sip:b;transport=wss">>)),
    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{transport => {other_transport, <<"wssnew">>}}}}},
       ersip_uri:parse(<<"sip:b;transport=wssnew">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;transport=&">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;transport=&;user=phone">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{lr => true}}}},
       ersip_uri:parse(<<"sip:b;lr">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{maddr => {ipv4, {1,1,1,1}}}}}},
       ersip_uri:parse(<<"sip:b;maddr=1.1.1.1">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;maddr=&">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
               host = {hostname, <<"b">>},
               params = #{user => phone}}}},
       ersip_uri:parse(<<"sip:b;user=phone">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{user => ip}}}},
       ersip_uri:parse(<<"sip:b;user=ip">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{user => <<"something">>}}}},
       ersip_uri:parse(<<"sip:b;user=something">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;user=&">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;user=">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
                           host = {hostname, <<"b">>},
                           params = #{ttl => 1}}}},
       ersip_uri:parse(<<"sip:b;ttl=1">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;ttl=a">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;ttl=-1">>)),
    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"sip:b;ttl=256">>)),

    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
               host = {hostname, <<"b">>},
               params = #{<<"some">> => <<"1">>}}}},
       ersip_uri:parse(<<"sip:b;Some=1">>)),
    ?assertEqual(
       {ok, #uri{data = #sip_uri_data{
               host = {hostname, <<"b">>},
               headers = #{<<"some">> => <<"1">>,
                           <<"another">> => <<"2">>}}}},
       ersip_uri:parse(<<"sip:b?Some=1&Another=2">>)),

    ?assertMatch({error, {einval, _}}, ersip_uri:parse(<<"a@b">>)),
    ok.

uri_make_test() ->
    {ok, ExpectedURI} = ersip_uri:parse(<<"sips:Alice@atlanta.com:8083">>),
    ?assertEqual(ExpectedURI,
                 ersip_uri:make([{scheme, sips},
                                 {user, <<"Alice">>},
                                 {host, {hostname, <<"atlanta.com">>}},
                                 {port, 8083}])),
    {ok, ExpectedURI2} = ersip_uri:parse(<<"sip:Alice@atlanta.com:5061">>),
    ?assertEqual(ExpectedURI2,
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
