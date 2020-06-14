%%%
%%% Copyright (c) 2017, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Method tests
%%%

-module(ersip_method_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

construction_test() ->
    {ok, AAAA, <<>>} =  ersip_method:parse(<<"AAAA">>),
    {ok, BBBB, <<>>} =  ersip_method:parse(<<"BBBB">>),
    ?assertEqual(AAAA, ersip_method:make(<<"AAAA">>)),
    ?assertNotEqual(BBBB, ersip_method:make(<<"AAAA">>)),
    ?assertError({invalid_method, _}, ersip_method:make(<<"  ">>)),
    ?assertError({invalid_method, _}, ersip_method:make(<<"INVITE,">>)),
    ok.


method_set_intersection_test() ->
    MS1 = ersip_method_set:intersection(
            ersip_method_set:new([ersip_method:invite(), ersip_method:options()]),
            ersip_method_set:new([ersip_method:options()])),
    ?assertEqual(true,  ersip_method_set:has(ersip_method:options(), MS1)),
    ?assertEqual(false, ersip_method_set:has(ersip_method:invite(),  MS1)),

    MS2 = ersip_method_set:intersection(
            ersip_method_set:new([ersip_method:invite()]),
            ersip_method_set:new([ersip_method:options()])),
    ?assertEqual(false, ersip_method_set:has(ersip_method:options(), MS2)),
    ?assertEqual(false, ersip_method_set:has(ersip_method:invite(),  MS2)),
    ok.
