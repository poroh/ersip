%%
%% Copyright (c) 2020 Dmitry Poroh
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
    _ = parse_success(<<"INVITE, ACK, OPTIONS">>),
    _ = parse_success(<<"INVITE,ACK,OPTIONS">>),
    _ = parse_success(<<"INVITE">>),
    parse_fail(<<>>),
    parse_fail(<<"&">>),
    ok.

parse_bin_test() ->
    ?assertMatch({ok, _}, ersip_hdr_allow:parse(<<"INVITE,ACK,OPTIONS">>)),
    ?assertMatch({error, {invalid_allow, _}}, ersip_hdr_allow:parse(<<"@,!,&">>)),
    ok.

make_test() ->
    Allow = ersip_hdr_allow:make(<<"INVITE, ACK">>),
    ?assertEqual(true, ersip_hdr_allow:has(ersip_method:make(<<"INVITE">>), Allow)),
    ?assertEqual(false, ersip_hdr_allow:has(ersip_method:make(<<"OPTIONS">>), Allow)),
    ok.

make_error_test() ->
    ?assertError({invalid_allow, _}, ersip_hdr_allow:make(<<"&">>)),
    ?assertError({invalid_allow, _}, ersip_hdr_allow:make(<<"INVITE&">>)),
    ok.

assemble_bin_test() ->
    [?assertEqual(adjust(X), adjust(ersip_hdr_allow:assemble_bin(ersip_hdr_allow:make(X))))
     || X <- [<<"INVITE, ACK, OPTIONS">>,
              <<"INVITE">>
             ]],
    ok.

raw_test() ->
    Allow = [<<"ACK">>, <<"INVITE">>, <<"OPTIONS">>],
    ?assertEqual(Allow, ersip_hdr_allow:raw(ersip_hdr_allow:make(Allow))),
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
    {ok, Allow} = ersip_hdr_allow:parse(AllowH),
    Allow.

parse_fail(Bin) ->
    AllowH = create(Bin),
    ?assertMatch({error, _}, ersip_hdr_allow:parse(AllowH)).

adjust(Bin) ->
    lists:sort(binary:split(Bin, <<", ">>, [global])).
