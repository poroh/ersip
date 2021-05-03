%%
%% Copyright (c) 2021 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP media BFCP 'floorid' attribute
%%

-module(ersip_sdp_bfcp_floorid_test).

-include_lib("eunit/include/eunit.hrl").

-import(ersip_sdp_bfcp_floorid,
        [new/2,
         id/1,
         streams/1,
         set_id/2,
         set_streams/2,
         parse/1,
         assemble_bin/1]).

%%%===================================================================
%%% Cases
%%%===================================================================
parse_test() ->
    {ok, BF1, <<>>} = parse(<<"123 mstrm:stream-1 stream-2">>),
    ?assertEqual(123, id(BF1)),
    ?assertEqual([<<"stream-1">>, <<"stream-2">>], streams(BF1)),
    {ok, BF2, <<>>} = parse(<<"65535 m-stream:stream-1 stream-2">>),
    ?assertEqual(65535, id(BF2)),
    ?assertEqual([<<"stream-1">>, <<"stream-2">>], streams(BF2)),
    {ok, BF3, <<>>} = parse(<<"0 m-stream:stream-1">>),
    ?assertEqual(0, id(BF3)),
    ?assertEqual([<<"stream-1">>], streams(BF3)).

parse_error_test() ->
    lists:foreach(
      fun(Bin) ->
              ?assertMatch({error, {invalid_bfcp_floorid, _}},
                           parse(Bin))
      end,
      [<<"s mstrm:stream-1">>,
       <<"-2 mstrm:stream-1">>,
       <<"65536 mstrm:stream-1">>,
       <<"1 mstrm:">>,
       <<"1 foo:stream-1">>]).

assemble_test() ->
    BF1 = new(5, [<<"stream-1">>, <<"stream-2">>]),
    BF2 = set_id(10, BF1),
    BF3 = set_streams([<<"stream-2">>, <<"stream-1">>], BF2),
    ?assertEqual(<<"10 mstrm:stream-2 stream-1">>,
                 assemble_bin(BF3)).
