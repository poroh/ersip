%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% TEL URI tests
%%%

-module(ersip_uri_tel_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

global_parse_test() ->
    {ok, Tel} = ersip_uri_tel:parse(<<"tel:+1(650)555-0123;ext=123">>),
    ?assertEqual(global, ersip_uri_tel:type(Tel)),
    ?assertEqual(<<"+1(650)555-0123">>, ersip_uri_tel:phone(Tel)),
    ?assertEqual(<<"123">>, ersip_uri_tel:extension(Tel)),
    ok.

local_parse_test() ->
    {ok, Tel2} = ersip_uri_tel:parse(<<"tel:555-0123;phone-context=+1(650)">>),
    ?assertEqual(local, ersip_uri_tel:type(Tel2)),
    ?assertEqual(<<"555-0123">>, ersip_uri_tel:phone(Tel2)),
    ?assertEqual(phone_prefix, ersip_uri_tel:phone_context_type(Tel2)),
    ?assertEqual(<<"+1(650)">>, ersip_uri_tel:phone_context(Tel2)),
    ok.

rfc_examples_test() ->
    Tel1 = ersip_uri_tel:make(<<"tel:+1-201-555-0123">>),
    ?assertEqual(global, ersip_uri_tel:type(Tel1)),
    ?assertEqual(<<"+1-201-555-0123">>, ersip_uri_tel:phone(Tel1)),
    ?assertEqual(undefined,             ersip_uri_tel:phone_context_type(Tel1)),
    ?assertEqual(undefined,             ersip_uri_tel:phone_context(Tel1)),
    ?assertEqual(undefined,             ersip_uri_tel:extension(Tel1)),

    Tel2 = ersip_uri_tel:make(<<"tel:7042;phone-context=example.com">>),
    ?assertEqual(local,                              ersip_uri_tel:type(Tel2)),
    ?assertEqual(<<"7042">>,                         ersip_uri_tel:phone(Tel2)),
    ?assertEqual(domain,                             ersip_uri_tel:phone_context_type(Tel2)),
    ?assertEqual(ersip_host:make(<<"example.com">>), ersip_uri_tel:phone_context(Tel2)),
    ?assertEqual(undefined,                          ersip_uri_tel:extension(Tel2)),

    Tel3 = ersip_uri_tel:make(<<"tel:863-1234;phone-context=+1-914-555">>),
    ?assertEqual(local, ersip_uri_tel:type(Tel3)),
    ?assertEqual(<<"863-1234">>,   ersip_uri_tel:phone(Tel3)),
    ?assertEqual(phone_prefix,     ersip_uri_tel:phone_context_type(Tel3)),
    ?assertEqual(<<"+1-914-555">>, ersip_uri_tel:phone_context(Tel3)),
    ?assertEqual(undefined,        ersip_uri_tel:extension(Tel3)),
    ok.

make_error_test() ->
    ?assertError({invalid_tel_uri, _}, ersip_uri_tel:make(<<"tel:;">>)),
    ok.

parse_error_test() ->
    ?assertMatch({error, {invalid_subscriber, _}} , ersip_uri_tel:parse(<<"tel:">>)),
    ?assertMatch({error, {invalid_subscriber, _}} , ersip_uri_tel:parse(<<"tel:+">>)),
    ?assertMatch({error, {invalid_subscriber, _}} , ersip_uri_tel:parse(<<"tel:+()">>)),
    ?assertMatch({error, {invalid_subscriber, _}} , ersip_uri_tel:parse(<<"tel:+-">>)),
    ?assertMatch({error, {invalid_subscriber, _}} , ersip_uri_tel:parse(<<"tel:()---">>)),
    ?assertMatch({error, {invalid_phone_context, _}}, ersip_uri_tel:parse(<<"tel:123;phone-context=&">>)),
    ?assertMatch({error, {invalid_phone_context, _}}, ersip_uri_tel:parse(<<"tel:123;phone-context=+()">>)),
    ?assertMatch({error, {invalid_phone_context, _}}, ersip_uri_tel:parse(<<"tel:123;phone-context=1234..">>)),
    ?assertMatch({error, {invalid_phone_context, _}}, ersip_uri_tel:parse(<<"tel:123;phone-context=+123abc">>)),
    ?assertMatch({error, {invalid_phone_context, _}}, ersip_uri_tel:parse(<<"tel:123;phone-context=[::1]a">>)),
    ?assertMatch({error, {invalid_phone_context, _}}, ersip_uri_tel:parse(<<"tel:123;phone-context=a.com+">>)),
    ?assertMatch({error, {garbage_at_the_end, _}},    ersip_uri_tel:parse(<<"tel:123?">>)),
    ?assertMatch({error, {invalid_scheme, _}},        ersip_uri_tel:parse(<<"sip:alice@atlanta.com">>)),
    ok.

rebuild_test() ->
    Cases = [<<"tel:+1-201-555-0123">>,
             <<"tel:7042;phone-context=example.com">>,
             <<"tel:863-1234;phone-context=+1-914-555">>
            ],
    [test_rebuild(X) || X <- Cases],
    ok.

comparison_test() ->
    Pairs = [{<<"tel:+1-201-555-0123">>, <<"tel:+12015550123">>},
             {<<"tel:+1-201-555-0123;ext=1">>, <<"tel:+12015550123;ext=(1)">>},
             {<<"tel:+1-201-555-0123;unknown=a">>, <<"tel:+12015550123;unknown=a">>},
             {<<"tel:+1-201-555-0123;unknown=a">>, <<"tel:+12015550123;unknown=%41">>},
             {<<"tel:7042;phone-context=example.com">>, <<"tel:7042;phone-context=EXAMPLE.COM">>},
             {<<"tel:7(042);phone-context=example.com">>, <<"tel:7042;phone-context=EXAMPLE.COM">>},
             {<<"tel:863-1234;phone-context=+1-914-555">>, <<"tel:8631234;phone-context=+1914555">>}
            ],
    [test_equal(X, Y) || {X, Y} <- Pairs],
    ok.

%%===================================================================
%% Helpers
%%===================================================================

test_rebuild(X) ->
    ?assertEqual(X, ersip_uri_tel:assemble_bin(ersip_uri_tel:make(X))).

test_equal(X, Y) ->
    XURI = ersip_uri_tel:make(X),
    YURI = ersip_uri_tel:make(Y),
    ?assertEqual(ersip_uri_tel:make_key(XURI), ersip_uri_tel:make_key(YURI)).
