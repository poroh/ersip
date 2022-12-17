%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP CSeq header tests
%%%

-module(ersip_hdr_date_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

parse_test() ->
    {ok, DT} = ersip_hdr_date:parse(create(<<"Fri, 16 Aug 1963 05:00:00 GMT">>)),
    %% checking all weekdays
    ?assertMatch({ok, _}, ersip_hdr_date:parse(create(<<"Sun, 1 Jul 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _}, ersip_hdr_date:parse(create(<<"Mon, 2 Jul 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _}, ersip_hdr_date:parse(create(<<"Tue, 3 Jul 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _}, ersip_hdr_date:parse(create(<<"Wed, 4 Jul 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _}, ersip_hdr_date:parse(create(<<"Thu, 5 Jul 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _}, ersip_hdr_date:parse(create(<<"Fri, 6 Jul 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _}, ersip_hdr_date:parse(create(<<"Sat, 7 Jul 2018 05:00:00 GMT">>))),

    %% checking all monthes
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Mon, 1 Jan 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Thu, 1 Feb 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"THu, 1 Mar 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Sun, 1 Apr 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Tue, 1 May 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Fri, 1 Jun 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Sun, 1 Jul 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Wed, 1 Aug 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Sat, 1 Sep 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Mon, 1 Oct 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Thu, 1 Nov 2018 05:00:00 GMT">>))),
    ?assertMatch({ok, _} , ersip_hdr_date:parse(create(<<"Sat, 1 Dec 2018 05:00:00 GMT">>))),

    ?assertMatch({ok, _} , ersip_hdr_date:parse(<<"Sat, 1 Dec 2018 05:00:00 GMT">>)),

    ?assertEqual({1963,8,16}, ersip_hdr_date:date(DT)),
    ?assertEqual({5,0,0}, ersip_hdr_date:time(DT)),
    ok.

parse_fail_test() ->
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 31 Feb 2018 05:00:00 GMT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Sun, 16 Aug 1963 05:00:00 GMT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fra, 16 Aug 1963 05:00:00 GMT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri  16 Aug 1963 05:00:00 GMT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 16 Awg 1963 05:00:00 GMT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 16 Aug some 05:00:00 GMT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 16 Aug 1963 105:00:00 GMT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 16 Aug 1963 05:100:00 GMT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 16 Aug 1963 05:00:100 GMT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 16 Aug 1963 05:00:00 UTC">>))), %% according RFC only GMT is valid
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 16 Aug 1963 05:00:00">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 16 Aug 1963 05:00:00 +0004">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 16 Aug 1963 05:00:00 -0004">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 1 Feb 2018 05:00:00 UT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 1 Feb 2018 05:00:00 EST">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 1 Feb 2018 05:00:00 EDT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 1 Feb 2018 05:00:00 MST">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 1 Feb 2018 05:00:00 MDT">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 1 Feb 2018 05:00:00 PST">>))),
    ?assertMatch({error, _}, ersip_hdr_date:parse(create(<<"Fri, 1 Feb 2018 05:00:00 PDT">>))),
    NoValue = ersip_hdr:new(<<"Date">>),
    ?assertMatch({error, _}, ersip_hdr_date:parse(NoValue)),
    ok.

reassemble_test() ->
    reassemble(<<"Fri, 16 Aug 1963 05:00:00 GMT">>),
    reassemble(<<"Tue, 31 Jul 2018 01:49:00 GMT">>),
    reassemble(<<"Wed, 1 Jan 2020 00:00:00 GMT">>),
    reassemble(<<"Sat, 1 Feb 2020 00:00:00 GMT">>),
    reassemble(<<"Sun, 1 Mar 2020 00:00:00 GMT">>),
    reassemble(<<"Wed, 1 Apr 2020 00:00:00 GMT">>),
    reassemble(<<"Fri, 1 May 2020 00:00:00 GMT">>),
    reassemble(<<"Mon, 1 Jun 2020 00:00:00 GMT">>),
    reassemble(<<"Wed, 1 Jul 2020 00:00:00 GMT">>),
    reassemble(<<"Sat, 1 Aug 2020 00:00:00 GMT">>),
    reassemble(<<"Tue, 1 Sep 2020 00:00:00 GMT">>),
    reassemble(<<"Thu, 1 Oct 2020 00:00:00 GMT">>),
    reassemble(<<"Sun, 1 Nov 2020 00:00:00 GMT">>),
    reassemble(<<"Tue, 1 Dec 2020 00:00:00 GMT">>),
    ok.

now_test() ->
    {date, {_, _}} = ersip_hdr_date:now(),
    ok.

make_test() ->
    D = {2015,8,8},
    T = {11,11,11},
    ?assert(ersip_hdr_date:is_valid(ersip_hdr_date:make(D,T))),
    ?assertEqual(<<"Thu, 1 Feb 2018 05:06:07 GMT">>, ersip_hdr_date:assemble_bin(ersip_hdr_date:make(<<"Thu, 1 Feb 2018 05:06:07 GMT">>))),
    ?assertError({invalid_date, _}, ersip_hdr_date:make(<<"Irf, 1 Feb 2018 05:06:07 GMT">>)),
    ?assertError({invalid_date, _}, ersip_hdr_date:make(create(<<"Irf, 1 Feb 2018 05:06:07 GMT">>))),
    ok.

build_test() ->
    DTH = create(<<"Tue, 31 Jul 2018 01:49:00 GMT">>),
    DTHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(DTH)],
    {ok, DT} = ersip_hdr_date:parse(DTH),
    BuiltDTH = ersip_hdr_date:build(<<"Date">>,  DT),
    BuiltDTHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(BuiltDTH)],
    ?assertEqual(DTHValues, BuiltDTHValues),
    ok.

raw_test() ->
    ?assertEqual({{2018, 2, 1}, {5, 6, 7}}, ersip_hdr_date:raw(ersip_hdr_date:make(<<"Thu, 1 Feb 2018 05:06:07 GMT">>))),
    ?assertEqual(<<"Thu, 1 Feb 2018 05:06:07 GMT">>, ersip_hdr_date:assemble_bin(ersip_hdr_date:make({{2018, 2, 1}, {5, 6, 7}}))),
    ?assertError({invalid_date, _}, ersip_hdr_date:make({{2018, 2, 99}, {5, 6, 7}})),
    ?assertError({invalid_date, _}, ersip_hdr_date:make({{2018, 2, 1}, {25, 6, 7}})),
    ok.

is_valid_test() ->
    ?assertEqual(true, ersip_hdr_date:is_valid(ersip_hdr_date:make(<<"Thu, 1 Feb 2018 05:06:07 GMT">>))),
    ?assertEqual(false, ersip_hdr_date:is_valid(not_a_date)),
    ?assertEqual(false, ersip_hdr_date:is_valid({date, {1}})),
    ?assertEqual(false, ersip_hdr_date:is_valid({date, {{a, b, c}, {d, e, f}}})),
    ?assertEqual(false, ersip_hdr_date:is_valid({date, {{2018, 2, 1}, {d, e, f}}})),
    ?assertEqual(false, ersip_hdr_date:is_valid({date, {{a, b, c}, {5, 6, 7}}})),
    ok.

%%===================================================================
%% Helpers
%%===================================================================
create(Bin) ->
    H = ersip_hdr:new(<<"Date">>),
    ersip_hdr:add_values([Bin], H).

make(Bin) ->
    ersip_hdr_date:make(create(Bin)).

reassemble(Bin) ->
    DT = make(Bin),
    ?assertEqual(Bin, ersip_hdr_date:assemble_bin(DT)).

