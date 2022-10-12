%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Qvalue tests
%%

-module(ersip_qvalue_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

comparision_test() ->
    ?assertEqual(make(<<"1">>), make(<<"1.">>)),
    ?assertEqual(make(<<"1">>), make(<<"1.0">>)),
    ?assertEqual(make(<<"1">>), make(<<"1.00">>)),
    ?assertEqual(make(<<"1">>), make(<<"1.000">>)),
    ?assertEqual(make(<<"0">>), make(<<"0.">>)),
    ?assertEqual(make(<<"0">>), make(<<"0.0">>)),
    ?assertEqual(make(<<"0">>), make(<<"0.00">>)),
    ?assertEqual(make(<<"0">>), make(<<"0.000">>)),
    ?assertEqual(make(<<"0.5">>), make(<<"0.500">>)),
    ?assertEqual(make(<<"0.501">>), make(<<"0.501">>)),
    ?assertEqual(make(<<"0.510">>), make(<<"0.51">>)),
    ?assert(make(<<"0">>) =< make(<<"1.">>)),
    ?assert(make(<<"0">>) =< make(<<"0.001">>)),
    ?assert(make(<<"0.500">>) =< make(<<"0.501">>)),
    ?assert(make(<<"0.500">>) =< make(<<"0.501">>)),
    ?assert(make(<<"0.501">>) =< make(<<"0.502">>)),
    ok.


rebuld_test() ->
    rebuild(<<"0">>),
    rebuild(<<"0.01">>),
    rebuild(<<"1">>),
    rebuild(<<"0.001">>),
    rebuild(<<"0.999">>),
    rebuild(<<"0.5">>),
    rebuild(<<"0.501">>),
    rebuild(<<"0.51">>),
    rebuild(<<"0.499">>),
    ok.

parse_error_test() ->
    parse_error(<<"1.1">>),
    parse_error(<<"-0">>),
    parse_error(<<"0.abc">>),
    parse_error(<<"1.001">>),
    parse_error(<<"0.-1">>),
    ok.

-dialyzer({nowarn_function, make_error_test/0}).
make_error_test() ->
    ?assertError({invalid_qvalue, _}, ersip_qvalue:make(<<"1.1">>)),
    ?assertError({invalid_qvalue, _}, ersip_qvalue:make(<<"-0">>)),
    ?assertError({invalid_qvalue, _}, ersip_qvalue:make(<<"0.500a">>)),
    ?assertError({invalid_qvalue, _}, ersip_qvalue:make(<<"0.abc">>)),
    ?assertError({invalid_qvalue, _}, ersip_qvalue:make(<<"1.001">>)),
    ?assertError({invalid_qvalue, _}, ersip_qvalue:make(1001)),
    ?assertError({invalid_qvalue, _}, ersip_qvalue:make(-1)),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

make(Bin) ->
    ersip_qvalue:make(Bin).

rebuild(Bin) ->
    Contact = ersip_qvalue:make(Bin),
    ContactBin = iolist_to_binary(ersip_qvalue:assemble(Contact)),
    {ok, Contact1} = ersip_qvalue:parse(ContactBin),
    ?assertEqual(Contact, Contact1).

parse_error(Bin) ->
    ?assertMatch({error, {invalid_qvalue, _}}, ersip_qvalue:parse(Bin)),
    ok.
