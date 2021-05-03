%%
%% Copyright (c) 2021 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media BFCP 'floorctrl' attribute
%%

-module(ersip_sdp_bfcp_floorctrl_test).

-include_lib("eunit/include/eunit.hrl").

-import(ersip_sdp_bfcp_floorctrl,
        [new/2,
         client/1,
         server/1,
         set_client/2,
         set_server/2,
         parse/1,
         assemble_bin/1
        ]).

%%%===================================================================
%%% Cases
%%%===================================================================
parse_test() ->
    {ok, BF1, <<>>} = parse(<<"c-only">>),
    ?assertEqual(true, client(BF1)),
    ?assertEqual(false, server(BF1)),
    {ok, BF2, <<>>} = parse(<<"s-only">>),
    ?assertEqual(false, client(BF2)),
    ?assertEqual(true, server(BF2)),
    {ok, BF3, <<>>} = parse(<<"s-only c-only">>),
    {ok, BF3, <<>>} = parse(<<"c-only s-only">>),
    {ok, BF3, <<>>} = parse(<<"c-s">>),
    ?assertEqual(true, client(BF3)),
    ?assertEqual(true, server(BF3)).

parse_error_test() ->
    lists:foreach(
      fun(Bin) ->
              ?assertMatch({error, {invalid_bfcp_floorctrl, _}},
                           parse(Bin))
      end,
      [<<>>,
       <<" ">>,
       <<" c-only">>,
       <<"c-only ">>,
       <<"foo">>]).

assemble_test() ->
    BF = new(true, true),
    ?assertEqual(<<"s-only c-only">>, assemble_bin(BF)),
    BF1 = set_client(false, BF),
    ?assertEqual(<<"s-only">>, assemble_bin(BF1)),
    BF2 = set_server(false, BF),
    ?assertEqual(<<"c-only">>, assemble_bin(BF2)).
