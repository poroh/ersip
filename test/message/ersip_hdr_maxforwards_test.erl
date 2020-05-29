%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Maxforwards tests
%%%

-module(ersip_hdr_maxforwards_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

parse_test() ->
    parse_success(<<"70">>),
    parse_success(<<"0">>),
    parse_success(<<"200">>),
    parse_fail(<<",">>),
    parse_fail(<<>>),
    parse_fail(<<"a@b">>),
    parse_fail(<<"70,1">>),

    ?assertMatch({error, {invalid_maxforwards, _}}, ersip_hdr_maxforwards:parse(<<"70,1">>)),
    ?assertMatch({ok, _}, ersip_hdr_maxforwards:parse(<<"70">>)),
    ok.

make_test() ->
    ?assertError({error, _}, ersip_hdr_maxforwards:make(<<"-1">>)),
    H = create_header(<<"-1">>),
    ?assertError({error, _}, ersip_hdr_maxforwards:make(H)),
    H1 = create_header(<<"55">>),
    ?assertEqual(make(<<"55">>), ersip_hdr_maxforwards:make(H1)),
    EmptyH1 = ersip_hdr:new(<<"Max-Forwards">>),
    ?assertError({error, no_maxforwards}, ersip_hdr_maxforwards:make(EmptyH1)),
    ok.

dec_test() ->
    MV0 = ersip_hdr_maxforwards:make(0),
    MV1 = ersip_hdr_maxforwards:make(1),
    ?assertError({error, _}, ersip_hdr_maxforwards:dec(MV0)),
    ?assertEqual({maxforwards, 0}, ersip_hdr_maxforwards:dec(MV1)),
    ok.

assemble_test() ->
    ?assertEqual(<<"70">>, ersip_hdr_maxforwards:assemble_bin(ersip_hdr_maxforwards:make(70))),
    ?assertEqual(<<"70">>, ersip_hdr_maxforwards:assemble(ersip_hdr_maxforwards:make(70))),
    ok.

raw_test() ->
    ?assertEqual(70, ersip_hdr_maxforwards:raw(ersip_hdr_maxforwards:make(<<"70">>))),
    ?assertEqual(70, ersip_hdr_maxforwards:raw(ersip_hdr_maxforwards:make(70))),
    ok.

%%===================================================================
%% Helpers
%%===================================================================

make(Bin) ->
    ersip_hdr_maxforwards:make(Bin).

create_header(Bin) ->
    MaxforwardsH = ersip_hdr:new(<<"Max-Forwards">>),
    ersip_hdr:add_value(Bin, MaxforwardsH).

parse_call_id(Bin) ->
    MaxforwardsH = create_header(Bin),
    ersip_hdr_maxforwards:parse(MaxforwardsH).

parse_success(Bin) ->
    ?assertEqual({ok, make(Bin)}, parse_call_id(Bin)).

parse_fail(Bin) ->
    ?assertMatch({error, _}, parse_call_id(Bin)).

