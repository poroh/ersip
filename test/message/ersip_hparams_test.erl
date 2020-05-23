%%%
%%% Copyright (c) 2018 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Header parameters test
%%%

-module(ersip_hparams_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

parse_raw_test() ->
    {ok, HParams0, <<>>} = ersip_hparams:parse_raw(<<"a=b">>),
    ?assertEqual({ok, <<"b">>}, ersip_hparams:find_raw(<<"A">>, HParams0)),

    {ok, HParams1, <<>>} = ersip_hparams:parse_raw(<<"a=b ; c=d">>),
    ?assertEqual({ok, <<"d">>}, ersip_hparams:find_raw(<<"c">>, HParams1)),

    {ok, HParams2, <<>>} = ersip_hparams:parse_raw(<<"a=b ; c">>),
    ?assertEqual({ok, <<>>}, ersip_hparams:find_raw(<<"c">>, HParams2)),

    ?assertMatch({error, {invalid_param, _}}, ersip_hparams:parse_raw(<<"@=@">>)),
    ?assertMatch({error, {invalid_param, _}}, ersip_hparams:parse_raw(<<"@">>)),
    ?assertMatch({error, {invalid_param, _}}, ersip_hparams:parse_raw(<<"a=b;@">>)),
    ?assertMatch({error, {invalid_param, _}}, ersip_hparams:parse_raw(<<"@;a=b">>)),
    ok.

find_raw_test() ->
    {ok, HParams, <<>>} = ersip_hparams:parse_raw(<<"a=b">>),
    ?assertEqual({ok, <<"b">>}, ersip_hparams:find_raw(<<"A">>, HParams)),
    ?assertEqual(not_found, ersip_hparams:find_raw(<<"ab">>, HParams)),

    HParams0 = ersip_hparams:new(),
    HParams1 = ersip_hparams:set(expires, 3, <<"Expires">>, <<"3">>, HParams0),
    HParams2 = ersip_hparams:set(q, ersip_qvalue:make(<<"0.1">>), <<"q">>, <<"0.1">>, HParams1),
    HParams3 = ersip_hparams:set_raw(<<"MyCustomParam">>, <<>>, HParams2),
    ?assertEqual({ok, <<"3">>},  ersip_hparams:find_raw(expires, HParams3)),
    ?assertEqual(not_found,      ersip_hparams:find_raw(q1,  HParams3)),
    ok.

set_raw_test() ->
    HParams0 = ersip_hparams:new(),
    HParams1 = ersip_hparams:set_raw(<<"a">>, <<"b">>, HParams0),
    HParams2 = ersip_hparams:set_raw(<<"a">>, <<"c">>, HParams1),
    ?assertEqual(<<"a=c">>, ersip_hparams:assemble_bin(HParams2)),
    ?assertEqual({ok, <<"c">>}, ersip_hparams:find_raw(<<"A">>, HParams2)),
    ok.

assemble_test() ->
    Rebuild = fun(Bin) ->
                      {ok, HParams, <<>>} = ersip_hparams:parse_raw(Bin),
                      ?assertEqual(Bin, ersip_hparams:assemble_bin(HParams))
              end,
    Rebuild(<<"a=b">>),
    Rebuild(<<"a">>),
    Rebuild(<<"a;b;c">>),
    Rebuild(<<"A=b;b;c">>),
    Rebuild(<<"A=192.168.1.1;b;c">>),
    ok.

set_test() ->
    HParams0 = ersip_hparams:new(),
    HParams1 = ersip_hparams:set(expires, 3, <<"Expires">>, <<"3">>, HParams0),
    ?assertEqual(<<"Expires=3">>, ersip_hparams:assemble_bin(HParams1)),

    HParams2 = ersip_hparams:set(q, ersip_qvalue:make(<<"0.1">>), <<"q">>, <<"0.1">>, HParams1),
    ?assertEqual(<<"Expires=3;q=0.1">>, ersip_hparams:assemble_bin(HParams2)),

    HParams3 = ersip_hparams:set_raw(<<"MyCustomParam">>, <<>>, HParams2),
    ?assertEqual(<<"Expires=3;q=0.1;MyCustomParam">>, ersip_hparams:assemble_bin(HParams3)),

    ?assertEqual({ok, 3},                            ersip_hparams:find(expires,       HParams3)),
    ?assertEqual({ok, 3},                            ersip_hparams:find(<<"expires">>, HParams3)),
    ?assertEqual({ok, ersip_qvalue:make(<<"0.1">>)}, ersip_hparams:find(q,             HParams3)),
    ?assertEqual(not_found,                          ersip_hparams:find(q1,            HParams3)),
    ?assertEqual(not_found,                          ersip_hparams:find(<<"q1">>,      HParams3)),
    ?assertEqual({ok, <<"3">>},                      ersip_hparams:find_raw(expires,   HParams3)),
    ok.


get_test() ->
    ?assertEqual(<<"b">>, ersip_hparams:get(<<"a">>, ersip_hparams:make(<<"a=b">>))),
    ?assertEqual(<<"d">>, ersip_hparams:get(<<"c">>, ersip_hparams:make(<<"a=b;c=d">>))),
    ?assertEqual(<<>>, ersip_hparams:get(<<"lr">>, ersip_hparams:make(<<"lr">>))),
    ?assertError({no_param, _}, ersip_hparams:get(<<"e">>, ersip_hparams:make(<<>>))),
    ?assertError({no_param, _}, ersip_hparams:get(<<"e">>, ersip_hparams:make(<<"a=b">>))),
    ok.

make_test() ->
    ?assertEqual({ok, <<"b">>}, ersip_hparams:find_raw(<<"a">>, ersip_hparams:make(<<"a=b;c=d">>))),
    ?assertEqual({ok, <<"d">>}, ersip_hparams:find_raw(<<"c">>, ersip_hparams:make(<<"a=b;c=d">>))),
    ?assertError({bad_params, _}, ersip_hparams:make(<<"&=?">>)),
    ?assertError({garbage_at_the_end, _}, ersip_hparams:make(<<"a=b, &">>)),

    ?assertEqual({ok, <<"b">>}, ersip_hparams:find_raw(<<"a">>, ersip_hparams:make(#{<<"a">> => <<"b">>}))),
    ?assertEqual({ok, <<"d">>}, ersip_hparams:find_raw(<<"c">>, ersip_hparams:make(#{<<"a">> => <<"b">>, <<"c">> => <<"d">>}))),
    ?assertError({invalid_raw_value, _}, ersip_hparams:make(#{a => <<"a">>})),
    ok.

empty_test() ->
    ?assertEqual(true, ersip_hparams:is_empty(ersip_hparams:make(<<>>))),
    ?assertEqual(false, ersip_hparams:is_empty(ersip_hparams:make(<<"a=b">>))),
    ?assertEqual(false, ersip_hparams:is_empty(ersip_hparams:make(<<"lr">>))),
    ok.

parse_known_test() ->
    ?assertEqual({error, my_error}, ersip_hparams:parse_known(fun(_, _) -> {error, my_error} end,
                                                              ersip_hparams:make(<<"a=b;c=d">>))),

    {ok, HParams} = ersip_hparams:parse_known(fun(<<"a">>, V) -> {ok, {a, V}};
                                                 (_, _) -> {ok, unknown}
                                              end,
                                              ersip_hparams:make(<<"a=b;c=d">>)),
    ?assertEqual(<<"b">>, ersip_hparams:get(a, HParams)),
    ok.

