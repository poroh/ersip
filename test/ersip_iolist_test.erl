%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% iolist tests
%%

-module(ersip_iolist_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

is_empty_test() ->
    ?assertEqual(true, ersip_iolist:is_empty(<<>>)),
    ?assertEqual(true, ersip_iolist:is_empty([])),
    ?assertEqual(true, ersip_iolist:is_empty([<<>>])),
    ?assertEqual(true, ersip_iolist:is_empty([[], <<>>])),
    ?assertEqual(true, ersip_iolist:is_empty([[], [<<>>]])),
    ?assertEqual(true, ersip_iolist:is_empty([[], []])).

trim_head_lws_test() ->
    ?assertEqual([], ersip_iolist:trim_head_lws([<<>>])),
    ?assertEqual([], ersip_iolist:trim_head_lws([])),
    ?assertEqual(<<>>, ersip_iolist:trim_head_lws(<<>>)),
    ?assertEqual("abc", ersip_iolist:trim_head_lws(" \tabc")),
    ?assertEqual("abc", ersip_iolist:trim_head_lws("\t abc")),
    ?assertEqual(<<"abc">>, ersip_iolist:trim_head_lws(<<" \tabc">>)),
    ?assertEqual(<<"abc">>, ersip_iolist:trim_head_lws(<<"\t abc">>)),
    ?assertEqual(["abc"], ersip_iolist:trim_head_lws([<<>>, "abc"])),
    ?assertEqual(["abc"], ersip_iolist:trim_head_lws([<<>>, " abc"])),
    ?assertEqual([<<"abc">>], ersip_iolist:trim_head_lws([<<" abc">>])),
    ?assertEqual([<<"abc">>], ersip_iolist:trim_head_lws(["", <<" abc">>])),
    ?assertEqual(["abc"], ersip_iolist:trim_head_lws([<<>>, " abc"])).

trim_tail_lws_test() ->
    ?assertEqual([],          ersip_iolist:trim_tail_lws([<<>>])),
    ?assertEqual([],          ersip_iolist:trim_tail_lws([])),
    ?assertEqual(<<>>,        ersip_iolist:trim_tail_lws(<<>>)),
    ?assertEqual("abc",       ersip_iolist:trim_tail_lws("abc \t")),
    ?assertEqual("abc",       ersip_iolist:trim_tail_lws("abc\t ")),
    ?assertEqual(<<"abc">>,   ersip_iolist:trim_tail_lws(<<"abc \t">>)),
    ?assertEqual(<<"abc">>,   ersip_iolist:trim_tail_lws(<<"abc\t ">>)),
    ?assertEqual(["abc"],     ersip_iolist:trim_tail_lws(["abc", <<>>])),
    ?assertEqual(["abc"],     ersip_iolist:trim_tail_lws(["abc ", <<>>])),
    ?assertEqual([<<"abc">>], ersip_iolist:trim_tail_lws([<<"abc ">>])),
    ?assertEqual([<<"abc">>], ersip_iolist:trim_tail_lws([<<"abc ">>, ""])),
    ?assertEqual(["abc"],     ersip_iolist:trim_tail_lws(["abc ", <<>>])),
    ?assertEqual(["abc"],     ersip_iolist:trim_tail_lws(["abc ", <<" ">>, <<" ">>])).

join_test() ->
    iolist_equal(<<"a">>,       ersip_iolist:join(",",     ["a"])),
    iolist_equal(<<"a,b">>,     ersip_iolist:join(",",     ["a","b"])),
    iolist_equal(<<"a,b">>,     ersip_iolist:join(<<",">>, ["a","b"])),
    iolist_equal(<<"a,b,c">>,   ersip_iolist:join(",",     ["a","b", <<"c">>])),
    iolist_equal(<<"a, b, c">>, ersip_iolist:join(", ",    ["a","b", <<"c">>])).



%%%===================================================================
%%% Helpers
%%%===================================================================

iolist_equal(IO1, IO2) ->
    ?assertEqual(iolist_to_binary(IO1), iolist_to_binary(IO2)). 
