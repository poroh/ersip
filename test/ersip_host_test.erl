%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message parser tests
%%

-module(ersip_host_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

hostname_parse_test() ->
    ?assertEqual({ok, {ipv4, {127, 0, 0, 1}}},             ersip_host:parse(<<"127.0.0.1">>)),
    ?assertEqual({ok, {ipv6, {0, 0, 0, 0,   0, 0, 0, 1}}}, ersip_host:parse(<<"[::1]">>)),
    ?assertEqual({ok, {hostname, <<"example.com">>}},        ersip_host:parse(<<"example.com">>)),
    ?assertEqual({ok, {hostname, <<"example.com.">>}},       ersip_host:parse(<<"example.com.">>)),
    ?assertEqual({ok, {hostname, <<"exa-mple.com.">>}},      ersip_host:parse(<<"exa-mple.com.">>)),
    ?assertEqual({ok, {hostname, <<"x.com">>}},              ersip_host:parse(<<"x.com">>)),
    ?assertEqual({ok, {hostname, <<"10.com">>}},             ersip_host:parse(<<"10.com">>)),
    ?assertEqual({ok, {hostname, <<"10.c-m">>}},             ersip_host:parse(<<"10.c-m">>)),
    ?assertEqual({ok, {hostname, <<"10.c--m">>}},            ersip_host:parse(<<"10.c--m">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"127..">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"[">>)),
    ?assertMatch({error, {invalid_ipv6, _}},  ersip_host:parse(<<"[]">>)),
    ?assertMatch({error, {invalid_ipv6, _}},  ersip_host:parse(<<"[:1:]">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"[::1">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"example..">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<>>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<".">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<".com.">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"example.1.">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"example.com-">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"example.-com">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"example.-com">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"example.com--">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"-example.com">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"example-.com">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"--example.com">>)),
    ?assertMatch({error, {invalid_host, _}},  ersip_host:parse(<<"example--.com">>))
        .

host_assemble_test() ->
    check_reassemble(<<"127.0.0.1">>),
    check_reassemble(<<"[::1]">>),
    check_reassemble(<<"example.com">>),
    check_reassemble(<<"example.com.">>),
    check_reassemble(<<"x.com">>),
    ok.

hostname_is_host_test() ->
    ?assertEqual(true, ersip_host:is_host({ipv4, {1, 2, 3, 4}})),
    ?assertEqual(true, ersip_host:is_host({ipv4, {0, 2, 3, 4}})),
    ?assertEqual(false, ersip_host:is_host({ipv4, {1, 2, 3}})),
    ?assertEqual(false, ersip_host:is_host({ipv4, {1, 2, 3, 4, 5}})),
    ?assertEqual(false, ersip_host:is_host({ipv4, {256, 2, 3, 4}})),
    ?assertEqual(false, ersip_host:is_host({ipv4, {-1, 2, 3, 4}})),
    ?assertEqual(true, ersip_host:is_host({ipv6, {1, 2, 3, 4, 1, 2, 3, 4}})),
    ?assertEqual(true, ersip_host:is_host({ipv6, {1, 0, 3, 4, 1, 2, 3, 4}})),
    ?assertEqual(false, ersip_host:is_host({ipv6, {1, 0, 3, 4, 1, 2, 3}})),
    ?assertEqual(false, ersip_host:is_host({ipv6, {1, 0, 3, 4, 1, 2, 3, 4, 5}})),
    ?assertEqual(false, ersip_host:is_host({ipv6, {-1, 0, 3, 4, 1, 2, 3}})),
    ?assertEqual(false, ersip_host:is_host({ipv6, {65536, 0, 3, 4, 1, 2, 3, 4, 5}})).

host_make_test() ->
    ?assertEqual(ersip_host:make(<<"127.0.0.1">>), ersip_host:make({127, 0, 0, 1})),
    ?assertEqual(ersip_host:make(<<"127.0.0.1">>), ersip_host:make({ipv4, {127, 0, 0, 1}})),
    ?assertEqual(ersip_host:make(<<"[::1]">>), ersip_host:make({ipv6, {0, 0, 0, 0,  0, 0, 0, 1}})),
    ?assertEqual(ersip_host:make(<<"[::1]">>), ersip_host:make({0, 0, 0, 0,  0, 0, 0, 1})),
    ?assertEqual(ersip_host:make(<<"a.com">>), ersip_host:make({hostname, <<"a.com">>})),
    ?assertError({error, _}, ersip_host:make({hostname, <<".">>})),
    ?assertError({error, _}, ersip_host:make(<<".">>)).

make_key_test() ->
    ?assertEqual(make_key(<<"example.com.">>),  make_key(<<"example.com">>)),
    ?assertEqual(make_key(<<"examplE.com">>),   make_key(<<"example.com">>)),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

check_reassemble(Binary) ->
    {ok, Host} = ersip_host:parse(Binary),
    ?assertEqual(Binary, iolist_to_binary(ersip_host:assemble(Host))).

make_key(Bin) ->
    ersip_host:make_key(ersip_host:make(Bin)).
