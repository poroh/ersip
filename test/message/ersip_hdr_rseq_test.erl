%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% RSeq tests
%%%

-module(ersip_hdr_rseq_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

parse_raw_sip_test() ->
    parse_success(<<"70">>),
    parse_success(<<"0">>),
    parse_success(<<"200">>),
    parse_fail(<<",">>),
    parse_fail(<<>>),
    parse_fail(<<"a@b">>),
    parse_fail(<<"70,1">>),
    ok.

parse_bin_test() ->
    ?assertMatch({ok, _}, ersip_hdr_rseq:parse(<<"70">>)),
    ?assertMatch({error, _}, ersip_hdr_rseq:parse(<<"70,1">>)),
    ok.

make_test() ->
    ?assertError({invalid_rseq, _}, ersip_hdr_rseq:make(<<"-1">>)),
    H = create_header(<<"-1">>),
    ?assertError({invalid_rseq, _}, ersip_hdr_rseq:make(H)),
    H1 = create_header(<<"55">>),
    ?assertEqual(make(<<"55">>), ersip_hdr_rseq:make(H1)),
    EmptyH1 = ersip_hdr:new(<<"RSeq">>),
    ?assertError(no_rseq, ersip_hdr_rseq:make(EmptyH1)).

inc_test() ->
    RV0 = ersip_hdr_rseq:make(0),
    RV1 = ersip_hdr_rseq:make(1),
    RV3 = ersip_hdr_rseq:make(16#FFFFFFFF),
    ?assertEqual({rseq, 1}, ersip_hdr_rseq:inc(RV0)),
    ?assertEqual({rseq, 2}, ersip_hdr_rseq:inc(RV1)),
    ?assertError(negative_rseq, ersip_hdr_rseq:inc(RV3)),
    ok.

value_test() ->
    RSeq0 = ersip_hdr_rseq:make(10),
    ?assertEqual(10, ersip_hdr_rseq:value(RSeq0)),
    RSeq1 = ersip_hdr_rseq:make(<<"11">>),
    ?assertEqual(11, ersip_hdr_rseq:value(RSeq1)).

build_test() ->
    RSeqH = create_header(<<"12">>),
    {ok, RSeq} = ersip_hdr_rseq:parse(RSeqH),
    RSeqHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(RSeqH)],
    BuiltRSeqH = ersip_hdr_rseq:build(<<"RSeq">>, RSeq),
    BuiltRSeqHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(BuiltRSeqH)],
    ?assertEqual(RSeqHValues, BuiltRSeqHValues).

empty_test() ->
    EmptyH = ersip_hdr:new(<<"RSeq">>),
    ?assertEqual({error, no_rseq}, ersip_hdr_rseq:parse(EmptyH)),
    ok.

assemble_bin_test() ->
    ?assertEqual(<<"44">>, ersip_hdr_rseq:assemble_bin(make(<<"44">>))),
    ?assertEqual(<<"1234">>, ersip_hdr_rseq:assemble_bin(make(<<"1234">>))),
    ok.

%%===================================================================
%% Helpers
%%===================================================================

make(Bin) ->
    ersip_hdr_rseq:make(Bin).

create_header(Bin) ->
    RseqH = ersip_hdr:new(<<"RSeq">>),
    ersip_hdr:add_value(Bin, RseqH).

parse_rseq(Bin) ->
    RseqH = create_header(Bin),
    ersip_hdr_rseq:parse(RseqH).

parse_success(Bin) ->
    ?assertEqual({ok, make(Bin)}, parse_rseq(Bin)).

parse_fail(Bin) ->
    ?assertMatch({error, _}, parse_rseq(Bin)).
