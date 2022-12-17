%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Opttag_List header tests
%%%

-module(ersip_hdr_opttag_list_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

parse_raw_hdr_test() ->
    _ = parse_success(<<"100rel, timer">>),
    _ = parse_success(<<"100rel,timer">>),
    _ = parse_success(<<"100rel">>),
    parse_fail(<<>>),
    parse_fail(<<"&">>),
    ok.

parse_binary_test() ->
    ?assertMatch({ok, _}, ersip_hdr_opttag_list:parse(<<"100rel, timer">>)),
    ?assertMatch({error, _}, ersip_hdr_opttag_list:parse(<<"&">>)),
    ok.

assemble_bin_test() ->
    ?assertEqual(<<"100rel, timer">>, ersip_hdr_opttag_list:assemble_bin(ersip_hdr_opttag_list:make(<<"100rel, timer">>))),
    ?assertEqual(<<"100rel">>, ersip_hdr_opttag_list:assemble_bin(ersip_hdr_opttag_list:make(<<"100rel">>))),
    ok.

option_tag_test() ->
    ?assertError({invalid_option_tag, _}, ersip_option_tag:make(<<"^">>)),
    ok.

option_tag_make_test() ->
    OptTagList = ersip_hdr_opttag_list:make(<<"100rel, timer">>),
    ?assertEqual([ersip_option_tag:make(<<"100rel">>),
                  ersip_option_tag:make(<<"timer">>)],
                 ersip_hdr_opttag_list:to_list(OptTagList)),
    ?assertError({invalid_option_tag_list, _}, ersip_hdr_opttag_list:make(<<"@">>)),
    ok.

append_test() ->
    OptTagList0 = ersip_hdr_opttag_list:make(<<"timer">>),
    OptTagList1 = ersip_hdr_opttag_list:append(ersip_option_tag:make(<<"100rel">>), OptTagList0),
    OptTagRef = ersip_hdr_opttag_list:make(<<"100rel, timer">>),
    ?assertEqual(OptTagRef, OptTagList1),
    ok.


raw_test() ->
    L = ersip_hdr_opttag_list:make([<<"timer">>,<<"100rel">>]),
    ?assertEqual(true, lists:member(<<"timer">>, ersip_hdr_opttag_list:raw(L))),
    ?assertEqual(true, lists:member(<<"100rel">>, ersip_hdr_opttag_list:raw(L))),
    ?assertEqual(false, lists:member(<<"another">>, ersip_hdr_opttag_list:raw(L))),
    ok.

make_raw_error_test() ->
    ?assertError({invalid_option_tag_list, _}, ersip_hdr_opttag_list:make([<<"@">>])),
    ok.


%%===================================================================
%% Helpers
%%===================================================================

create(<<>>) ->
    ersip_hdr:new(<<"Supported">>);
create(Bin) ->
    OpttagListH = ersip_hdr:new(<<"Supported">>),
    ersip_hdr:add_value(Bin, OpttagListH).

parse_success(Bin) ->
    OpttagListH = create(Bin),
    {ok, OpttagList} = ersip_hdr_opttag_list:parse(OpttagListH),
    OpttagList.

parse_fail(Bin) ->
    OpttagListH = create(Bin),
    ?assertMatch({error, _}, ersip_hdr_opttag_list:parse(OpttagListH)).
