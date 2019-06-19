%%
%% Copyright (c) 2018, 2019 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP From/to header tests
%%

-module(ersip_hdr_fromto_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_test() ->
    URIBin = <<"sip:alice@atlanta.com">>,
    {ok, URI} = ersip_uri:parse(URIBin),

    To = success_parse_fromto(<<"Alice <", URIBin/binary, ">;tag=1928301774">>),
    ?assertEqual({display_name,  [<<"Alice">>]}, ersip_hdr_fromto:display_name(To)),
    ?assertEqual({tag, <<"1928301774">>}, ersip_hdr_fromto:tag(To)),
    ?assertEqual(URI, ersip_hdr_fromto:uri(To)),

    To1 = success_parse_fromto(<<"Alice <", URIBin/binary, ">">>),
    ?assertEqual({display_name,  [<<"Alice">>]}, ersip_hdr_fromto:display_name(To1)),
    ?assertEqual(undefined, ersip_hdr_fromto:tag(To1)),
    ?assertEqual(URI, ersip_hdr_fromto:uri(To1)),

    To2 = success_parse_fromto(<<"<", URIBin/binary, ">;Some=y">>),
    ?assertEqual({display_name, []}, ersip_hdr_fromto:display_name(To2)),
    ?assertEqual(#{<<"some">> => <<"y">>}, ersip_hdr_fromto:params(To2)),

    To3 = success_parse_fromto(<<URIBin/binary, ";tag=1928301774">>),
    ?assertEqual({display_name,  []}, ersip_hdr_fromto:display_name(To3)),
    ?assertEqual({tag, <<"1928301774">>}, ersip_hdr_fromto:tag(To3)),
    ?assertEqual(URI, ersip_hdr_fromto:uri(To3)),

    To4 = success_parse_fromto(<<URIBin/binary, " ; tag=100">>),
    ?assertEqual({display_name,  []}, ersip_hdr_fromto:display_name(To4)),
    ?assertEqual({tag, <<"100">>}, ersip_hdr_fromto:tag(To4)),
    ?assertEqual(URI, ersip_hdr_fromto:uri(To4)),
    ok.


%% This format violates RFC 3261 cluase:
%% | Even if the "display-name" is empty, the "name-addr" form MUST be
%% | used if the "addr-spec" contains a comma, semicolon, or question
%% | mark.
%% But some not very compiant clients do this.
comma_username_test() ->
    URIBin = <<"sip:+111,,,2@atlanta.com">>,
    {ok, URI} = ersip_uri:parse(URIBin),
    To = success_parse_fromto(URIBin),
    ?assertEqual(URI, ersip_hdr_fromto:uri(To)),

    To2 = success_parse_fromto(<<URIBin/binary, ";tag=12345">>),
    ?assertEqual(URI, ersip_hdr_fromto:uri(To2)),
    ?assertEqual({tag, <<"12345">>}, ersip_hdr_fromto:tag(To2)),
    ok.


parse_fail_test() ->
    ?assertMatch({error, _}, ersip_hdr_fromto:parse(create(<<"a@b">>))),
    ?assertMatch({error, _}, ersip_hdr_fromto:parse(create(<<"sip:a@b;tag=\"123\"">>))),
    ?assertMatch({error, _}, ersip_hdr_fromto:parse(create(<<"sip:a@b;my_param=&">>))),
    NoValue = ersip_hdr:new(<<"To">>),
    ?assertMatch({error, _}, ersip_hdr_fromto:parse(NoValue)),
    NValues = ersip_hdr:new(<<"To">>),
    NValues1 = ersip_hdr:add_values([<<"Alice <sip:a@b>">>,
                                     <<"Bob <sip:b@a>">>],
                                    NValues),
    ?assertMatch({error, _}, ersip_hdr_fromto:parse(NValues1)),
    ?assertMatch({error, _}, ersip_hdr_fromto:parse(create(<<"Bond, James <sip:bond@mi6.uk>">>))),
    ?assertMatch({error, _}, ersip_hdr_fromto:parse(create(<<"sip:example.net;?">>))),
    ok.

make_test() ->
    URIBin = <<"sip:alice@atlanta.com">>,
    {ok, URI} = ersip_uri:parse(URIBin),
    To = ersip_hdr_fromto:make(<<"Alice <", URIBin/binary, ">;tag=1928301774">>),
    ?assertEqual({display_name,  [<<"Alice">>]}, ersip_hdr_fromto:display_name(To)),
    ?assertEqual({tag, <<"1928301774">>}, ersip_hdr_fromto:tag(To)),
    ?assertEqual(URI, ersip_hdr_fromto:uri(To)),
    ?assertError({error, _}, ersip_hdr_fromto:make(<<>>)).

tag_key_test() ->
    ?assertEqual({tag_key, <<"1928301774">>}, tag_key(<<"sip:a@b;tag=1928301774">>)),
    ?assertEqual({tag_key, <<"abc">>}, tag_key(<<"sip:a@b;tag=ABC">>)),
    ?assertEqual(undefined, tag_key(<<"sip:a@b">>)).

assemble_test() ->
    reassemble_check(<<"<sip:a@b>">>),
    reassemble_check(<<"Alice <sip:a@b>">>),
    reassemble_check(<<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>),
    reassemble_check(<<"\"A. G. Bell\" <sip:agb@bell-telephone.com>;tag=a48s">>),
    reassemble_check(<<"\"A. G. Bell\" <sip:agb@bell-telephone.com>;tag=a48s;myparam=Value">>),
    ok.

reassemble_without_quotes_test() ->
    reassemble_check(<<"sip:example.net;a">>, <<"<sip:example.net>;a">>),
    reassemble_check(<<"sip:example.net;tag=88sja8x">>, <<"<sip:example.net>;tag=88sja8x">>),
    ok.

set_tag_test() ->
    AliceF = success_parse_fromto(<<"Alice <sip:alice@atlanta.com>">>),
    AliceWithTag = ersip_hdr_fromto:set_tag({tag, <<"88sja8x">>}, AliceF),
    ?assertEqual(<<"Alice <sip:alice@atlanta.com>;tag=88sja8x">>, assemble(AliceWithTag)),
    ok.

build_test() ->
    AliceF = success_parse_fromto(<<"Alice <sip:alice@atlanta.com>">>),
    AliceFH = ersip_hdr_fromto:build(<<"From">>, AliceF),
    {ok, AliceF2} = ersip_hdr_fromto:parse(AliceFH),
    ?assertEqual(AliceF, AliceF2).

rfc4475_crazy_example_test() ->
    CrazyExampleBin = <<"\"BEL:\<hex>07</hex> NUL:\<hex>00</hex> DEL:\<hex>7F</hex>\" <sip:1_unusual.URI~(to-be!sure)&isn't+it$/crazy?,/;;*@example.com>">>,
    To = success_parse_fromto(CrazyExampleBin),
    ?assertEqual({display_name,  <<"\"BEL:\<hex>07</hex> NUL:\<hex>00</hex> DEL:\<hex>7F</hex>\"">>}, ersip_hdr_fromto:display_name(To)),
    reassemble_check(CrazyExampleBin),
    ok.

raw_params_test() ->
    Uri = success_parse_fromto(<<"Alice <sip:a@b>;tag=1abc;my=1;other;other1">>),
    RawParams = lists:sort(ersip_hdr_fromto:raw_params(Uri)),
    ExpectedParams = lists:sort([{<<"tag">>, <<"1abc">>},
                                 <<"other">>, <<"other1">>,
                                 {<<"my">>, <<"1">>}]),
    ?assertEqual(ExpectedParams, RawParams),

    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================
create(Bin) ->
    H = ersip_hdr:new(<<"To">>),
    ersip_hdr:add_value(Bin, H).

success_parse_fromto(Bin) ->
    H = create(Bin),
    {ok, Hdr} = ersip_hdr_fromto:parse(H),
    Hdr.

tag_key(Bin) ->
    ersip_hdr_fromto:tag_key(ersip_hdr_fromto:make(Bin)).

reassemble_check(Bin) ->
    reassemble_check(Bin, Bin).

reassemble_check(Bin, ExpectedResultBin) ->
    {ok, FromTo} = ersip_hdr_fromto:parse(create(Bin)),
    BinAssembled = iolist_to_binary(ersip_hdr_fromto:assemble(FromTo)),
    {ok, _} = ersip_hdr_fromto:parse(create(BinAssembled)),
    ?assertEqual(ExpectedResultBin, BinAssembled).

assemble(FromTo) ->
    iolist_to_binary(ersip_hdr_fromto:assemble(FromTo)).
