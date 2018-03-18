%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Allow header tests
%%

-module(ersip_hdr_allow_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_test() ->
    parse_success(<<"INVITE, ACK, OPTIONS">>),
    parse_success(<<"INVITE,ACK,OPTIONS">>),
    parse_success(<<"INVITE">>),
    parse_fail(<<>>),
    parse_fail(<<"&">>),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

create(<<>>) ->
    ersip_hdr:new(<<"Allow">>);
create(Bin) ->
    AllowH = ersip_hdr:new(<<"Allow">>),
    ersip_hdr:add_value(Bin, AllowH).

parse_success(Bin) ->
    AllowH = create(Bin),
    { ok, Allow } = ersip_hdr_allow:parse(AllowH),
    Allow.

parse_fail(Bin) ->
    AllowH = create(Bin),
    ?assertMatch({ error, _ }, ersip_hdr_allow:parse(AllowH)).
