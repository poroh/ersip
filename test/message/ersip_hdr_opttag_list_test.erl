%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Opttag_List header tests
%%

-module(ersip_hdr_opttag_list_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_test() ->
    parse_success(<<"100rel, timer">>),
    parse_success(<<"100rel,timer">>),
    parse_success(<<"100rel">>),
    parse_fail(<<>>),
    parse_fail(<<"&">>),
    ok.

option_tag_test() ->
    ?assertError({error, _}, ersip_option_tag:make(<<"^">>)).

option_tag_make_test() ->
    OptTagList = ersip_hdr_opttag_list:make(<<"100rel, timer">>),
    ?assertEqual([ersip_option_tag:make(<<"100rel">>),
                  ersip_option_tag:make(<<"timer">>)],
                 ersip_hdr_opttag_list:to_list(OptTagList)),
    ?assertError({error, _}, ersip_hdr_opttag_list:make(<<"@">>)),
    ok.

append_test() ->
    OptTagList0 = ersip_hdr_opttag_list:make(<<"timer">>),
    OptTagList1 = ersip_hdr_opttag_list:append(ersip_option_tag:make(<<"100rel">>), OptTagList0),
    OptTagRef = ersip_hdr_opttag_list:make(<<"100rel, timer">>),
    ?assertEqual(OptTagRef, OptTagList1),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

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
