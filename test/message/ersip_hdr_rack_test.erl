%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% RSeq tests
%%%

-module(ersip_hdr_rack_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

parse_test() ->
    parse_success(<<"776656 1 INVITE">>),
    parse_success(<<"776656 0 INVITE">>),
    parse_success(<<"4294967295 1 INVITE">>),
    parse_fail(<<",">>),
    parse_fail(<<>>),
    parse_fail(<<"a@b">>),
    parse_fail(<<"776656 0 INVITE, 776656 1 INVITE">>),
    parse_fail(<<"4294967296 1 INVITE">>),
    parse_fail(<<"1 4294967296 INVITE">>),
    parse_fail(<<"-1 1 INVITE">>),
    parse_fail(<<"1 -1 INVITE">>),
    ok.

parse_bin_test() ->
    ?assertMatch({ok, _}, ersip_hdr_rack:parse(<<"776656 1 INVITE">>)),
    ?assertMatch({error, _}, ersip_hdr_rack:parse(<<",">>)),
    ok.

make_test() ->
    ?assertError({error, _}, ersip_hdr_rack:make(<<"1">>)),
    ?assertError({error, _}, ersip_hdr_rack:make(<<"4294967296 1 INVITE">>)),
    RAck = ersip_hdr_rack:make(<<"1 2 INVITE">>),
    ?assertEqual(1, ersip_hdr_rseq:value(ersip_hdr_rack:rseq(RAck))),
    ?assertEqual(2, ersip_hdr_cseq:number(ersip_hdr_rack:cseq(RAck))),
    ?assertEqual(ersip_method:invite(), ersip_hdr_cseq:method(ersip_hdr_rack:cseq(RAck))),

    RAck2 = ersip_hdr_rack:make(ersip_hdr_rseq:make(3), ersip_hdr_cseq:make(ersip_method:invite(), 4)),
    ?assertEqual(3, ersip_hdr_rseq:value(ersip_hdr_rack:rseq(RAck2))),
    ?assertEqual(4, ersip_hdr_cseq:number(ersip_hdr_rack:cseq(RAck2))),
    ?assertEqual(ersip_method:invite(), ersip_hdr_cseq:method(ersip_hdr_rack:cseq(RAck2))),

    RAck3H = create_header(<<"5 6 INVITE">>),
    RAck3  = ersip_hdr_rack:make(RAck3H),
    ?assertEqual(5, ersip_hdr_rseq:value(ersip_hdr_rack:rseq(RAck3))),
    ?assertEqual(6, ersip_hdr_cseq:number(ersip_hdr_rack:cseq(RAck3))),
    ?assertEqual(ersip_method:invite(), ersip_hdr_cseq:method(ersip_hdr_rack:cseq(RAck3))),

    ?assertError({error,_ }, ersip_hdr_rack:make(create_header(<<"-1 6 INVITE">>))),
    ok.

rebuild_test() ->
    rebuild(<<"776656 1 INVITE">>),
    rebuild(<<"776656 0 INVITE">>),
    rebuild(<<"4294967295 1 INVITE">>),
    ok.

build_test() ->
    RAckH = create_header(<<"776656 1 INVITE">>),
    {ok, RAck} = ersip_hdr_rack:parse(RAckH),
    RAckHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(RAckH)],
    BuiltRAckH = ersip_hdr_rack:build(<<"RAck">>, RAck),
    BuiltRAckHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(BuiltRAckH)],
    ?assertEqual(RAckHValues, BuiltRAckHValues),
    ok.

empty_test() ->
    EmptyH = ersip_hdr:new(<<"RAck">>),
    ?assertEqual({error, no_rack}, ersip_hdr_rack:parse(EmptyH)),
    ok.

raw_test() ->
    ?assertMatch(#{rseq := 1}, ersip_hdr_rack:raw(ersip_hdr_rack:make(<<"1 776656 INVITE">>))),
    ?assertMatch(#{cseq := {776656, <<"INVITE">>}}, ersip_hdr_rack:raw(ersip_hdr_rack:make(<<"1 776656 INVITE">>))),
    ?assertEqual(<<"1 776656 INVITE">>, ersip_hdr_rack:assemble_bin(ersip_hdr_rack:make(#{rseq => 1, cseq => {776656, <<"INVITE">>}}))),
    ok.


%%===================================================================
%% Helpers
%%===================================================================

make(Bin) ->
    ersip_hdr_rack:make(Bin).

create_header(Bin) ->
    RseqH = ersip_hdr:new(<<"RSeq">>),
    ersip_hdr:add_value(Bin, RseqH).

parse_rack(Bin) ->
    RseqH = create_header(Bin),
    ersip_hdr_rack:parse(RseqH).

parse_success(Bin) ->
    ?assertEqual({ok, make(Bin)}, parse_rack(Bin)).

parse_fail(Bin) ->
    ?assertMatch({error, _}, parse_rack(Bin)).

rebuild(Bin) ->
    R = ersip_hdr_rack:assemble_bin(ersip_hdr_rack:make(Bin)),
    ?assertEqual(Bin, R).
