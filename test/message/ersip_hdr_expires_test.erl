%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Expires header tests
%%%

-module(ersip_hdr_expires_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

rebuild_test() ->
    rebuild(<<"0">>),
    rebuild(<<"5">>),
    rebuild(<<"4294967295">>),
    ok.

parse_test() ->
    ?assertMatch({ok, _}, ersip_hdr_expires:parse(<<"1">>)),
    ?assertMatch({error, {invalid_expires, _}}, ersip_hdr_expires:parse(<<"-1">>)),
    ok.

parse_error_test() ->
    parse_error(<<"4294967296">>),
    parse_error(<<"a">>),
    parse_error(<<"0xff">>),
    parse_error(<<"-1">>),
    parse_error(<<"-100">>),
    parse_error(<<"1.1">>),
    parse_error(<<"5, 6">>),
    parse_error(<<"">>),
    ?assertMatch({error, {invalid_expires, _}}, ersip_hdr_expires:parse(ersip_hdr:new(<<"Expires">>))),
    ok.


make_test() ->
    ?assertEqual(<<"1">>, ersip_hdr_expires:assemble_bin(ersip_hdr_expires:make(1))),
    ?assertEqual(<<"20">>, ersip_hdr_expires:assemble_bin(ersip_hdr_expires:make(20))),

    ?assertError({invalid_expires, _}, ersip_hdr_expires:make(<<"1.1">>)),
    ?assertError({invalid_expires, _}, ersip_hdr_expires:make(<<"-1">>)),
    ?assertError({invalid_expires, _}, ersip_hdr_expires:make(<<"1, 2">>)),
    ok.

raw_test() ->
    ?assertEqual(1, ersip_hdr_expires:raw(ersip_hdr_expires:make(<<"1">>))),
    ?assertEqual(20, ersip_hdr_expires:raw(ersip_hdr_expires:make(<<"20">>))),
    ok.

%%===================================================================
%% Helpers
%%===================================================================

rebuild(Bin) ->
    Expires = ersip_hdr_expires:make(Bin),
    {ok, Expires1} = ersip_hdr_expires:parse(ersip_hdr_expires:build(<<"Expires">>, Expires)),
    ?assertEqual(Expires, Expires1).

create(Bin) ->
    H = ersip_hdr:new(<<"Expires">>),
    ersip_hdr:add_value(Bin, H).

parse_error(Bin) ->
    ?assertMatch({error, {invalid_expires, _}}, ersip_hdr_expires:parse(create(Bin))).
