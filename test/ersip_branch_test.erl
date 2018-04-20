%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP branch tests
%%

-module(ersip_branch_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

is_rfc3261_test() ->
    assert_rfc3261(<<"z9hG4bK776asdhds">>),
    assert_rfc3261(<<"z9hG4bK">>),
    assert_not_rfc3261(<<"z9hg4bka">>),
    assert_not_rfc3261(<<"_z9hG4bK">>),
    assert_not_rfc3261(<<"_">>),
    assert_not_rfc3261(<<"z">>).

is_rfc3261_key_test() ->
    assert_key_rfc3261(<<"z9hg4bk776asdhds">>),
    assert_key_rfc3261(<<"z9hg4bk">>),
    assert_key_not_rfc3261(<<"_z9hG4bK">>),
    assert_key_not_rfc3261(<<"_">>),
    assert_key_not_rfc3261(<<"z">>).

branch_compare_test() ->
    assert_equal(<<"z9hG4bK776asdhds">>, <<"z9hg4bk776asdhds">>),
    assert_not_equal(<<"z9hG4bK77_6asdhds">>, <<"z9hG4bK776asdhds">>).

branch_key_idempotent_test() ->
    Key = make_key(<<"z9hG4bK776asdhds">>),
    ?assertEqual(Key, ersip_branch:make_key(Key)).

assemle_test() ->
    check_reassemble(<<"z9hg4bk776asdhds">>),
    check_reassemble(<<"z9hg4bk">>),
    check_reassemble(<<"_z9hG4bK">>),
    check_reassemble(<<"_">>),
    check_reassemble(<<"z">>).

%%%===================================================================
%%% Helpers
%%%===================================================================
assert_rfc3261(Bin) ->
    Branch = ersip_branch:make(Bin),
    ?assertEqual(true, ersip_branch:is_rfc3261(Branch)).

assert_not_rfc3261(Bin) ->
    Branch = ersip_branch:make(Bin),
    ?assertEqual(false, ersip_branch:is_rfc3261(Branch)).

assert_key_rfc3261(Bin) ->
    BranchKey = make_key(Bin),
    ?assertEqual(true, ersip_branch:is_rfc3261(BranchKey)).

assert_key_not_rfc3261(Bin) ->
    BranchKey = make_key(Bin),
    ?assertEqual(false, ersip_branch:is_rfc3261(BranchKey)).

assert_equal(Bin1, Bin2) ->
    BranchKey1 = make_key(Bin1),
    BranchKey2 = make_key(Bin2),
    ?assertEqual(true, BranchKey1 =:= BranchKey2).

assert_not_equal(Bin1, Bin2) ->
    BranchKey1 = make_key(Bin1),
    BranchKey2 = make_key(Bin2),
    ?assertEqual(false, BranchKey1 =:= BranchKey2).

make_key(Bin) ->
    Branch = ersip_branch:make(Bin),
    ersip_branch:make_key(Branch).

check_reassemble(Binary) ->
    Branch = ersip_branch:make(Binary),
    ?assertEqual(Binary, iolist_to_binary(ersip_branch:assemble(Branch))).

