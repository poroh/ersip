%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Subscription-State tests
%%%

-module(ersip_hdr_refer_to_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

rebuild_test() ->
    %% RFC 3515:
    rebuild(<<"sip:alice@atlanta.example.com">>),
    rebuild(<<"<sip:bob@biloxi.example.net?Accept-Contact=sip:bobsdesk."
              "biloxi.example.net&Call-ID%3D55432%40alicepc.atlanta.example.com>">>),
    rebuild(<<"<sip:dave@denver.example.org?Replaces=12345%40192.168.118.3%3B"
              "to-tag%3D12345%3Bfrom-tag%3D5FFE-3994>">>),
    rebuild(<<"<sip:carol@cleveland.example.org;method=SUBSCRIBE>">>),
    rebuild(<<"http://www.ietf.org">>),
    rebuild(<<"<sip:carol@cleveland.example.org;method=SUBSCRIBE>;x-some-param;x-some-param=1;X">>),
    ok.

parse_error_test() ->
    parse_error(<<"">>),
    parse_error(<<"invalid_uri">>),
    parse_error(<<"sip:a@b, sip:b@d">>),
    parse_error(<<"sip:a@b;@=1">>),
    parse_error(<<"sip:a@b;a=@">>),
    ok.

make_error_test() ->
    ?assertError({invalid_refer_to, _}, ersip_hdr_refer_to:make(<<"invalid_uri">>)),
    ?assertError({invalid_refer_to, _}, ersip_hdr_refer_to:make(<<"sip:a@b;@=1">>)),
    ok.

build_test() ->
    ReferToH = create(<<"<sip:alice@atlanta.example.com>">>),
    {ok, ReferTo} = ersip_hdr_refer_to:parse(ReferToH),
    ReferToHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(ReferToH)],
    BuiltReferToH = ersip_hdr_refer_to:build(<<"Refer-To">>, ReferTo),
    BuiltReferToHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(BuiltReferToH)],
    ?assertEqual(ReferToHValues, BuiltReferToHValues),

    EmptyH = ersip_hdr:new(<<"Refer-To">>),
    ?assertEqual({error, no_refer_to}, ersip_hdr_refer_to:parse(EmptyH)),
    ok.

new_test() ->
    URI = ersip_uri:make(<<"sip:alice@atlanta.com">>),
    ReferTo = ersip_hdr_refer_to:new(URI),
    ?assertEqual(URI, ersip_hdr_refer_to:uri(ReferTo)),
    ok.

getters_test() ->
    ReferTo = ersip_hdr_refer_to:make(<<"Alice <sip:alice@pc33.atlanta.com;uri-param=1>;header-param=1;header-param2">>),
    ?assertEqual({display_name, [<<"Alice">>]}, ersip_hdr_refer_to:display_name(ReferTo)),
    ?assertEqual(ersip_uri:make(<<"sip:alice@pc33.atlanta.com;uri-param=1">>), ersip_hdr_refer_to:uri(ReferTo)),
    ?assertEqual([{<<"header-param">>, <<"1">>}, <<"header-param2">>], ersip_hdr_refer_to:all_raw_params(ReferTo)),
    ok.

setters_test() ->
    ReferTo = ersip_hdr_refer_to:make(<<"Alice <sip:alice@pc33.atlanta.com;uri-param=1>;header-param=1;header-param2">>),
    ReferTo1 = ersip_hdr_refer_to:set_uri(ersip_uri:make(<<"sip:bob@biloxi.com">>), ReferTo),
    ?assertEqual(<<"Alice <sip:bob@biloxi.com>;header-param=1;header-param2">>, ersip_hdr_refer_to:assemble_bin(ReferTo1)),
    ReferTo2 = ersip_hdr_refer_to:set_display_name({display_name, [<<"Bob">>]}, ReferTo1),
    ?assertEqual(<<"Bob <sip:bob@biloxi.com>;header-param=1;header-param2">>, ersip_hdr_refer_to:assemble_bin(ReferTo2)),
    ok.

%%===================================================================
%% Helpers
%%===================================================================

create(Bin) ->
    H = ersip_hdr:new(<<"Refer-To">>),
    ersip_hdr:add_value(Bin, H).

rebuild(Bin) ->
    ReferTo = ersip_hdr_refer_to:make(Bin),
    ReferToBin = ersip_hdr_refer_to:assemble_bin(ReferTo),
    {ok, ReferTo1} = ersip_hdr_refer_to:parse(ReferToBin),
    ?assertEqual(ReferTo, ReferTo1).

parse_error(Bin) ->
    ?assertMatch({error, {invalid_refer_to, _}}, ersip_hdr_refer_to:parse(Bin)).
