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
    ?assertError({ error, _ }, ersip_option_tag:make(<<"^">>)).

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
    { ok, OpttagList } = ersip_hdr_opttag_list:parse(OpttagListH),
    OpttagList.

parse_fail(Bin) ->
    OpttagListH = create(Bin),
    ?assertMatch({ error, _ }, ersip_hdr_opttag_list:parse(OpttagListH)).
