%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP origin (o=) test
%%

-module(ersip_sdp_origin_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

parse_test() ->
    {ok, Orig1} = parse(<<"o=mhandley 2890844526 2890845468 IN IP4 126.16.64.4">>), %% RFC 2326
    ?assertEqual(<<"mhandley">>, ersip_sdp_origin:username(Orig1)),
    ?assertEqual(2890844526,     ersip_sdp_origin:session_id(Orig1)),
    ?assertEqual(2890845468,     ersip_sdp_origin:session_version(Orig1)),
    ?assertEqual(ersip_sdp_addr:make(<<"IN IP4 126.16.64.4">>), ersip_sdp_origin:address(Orig1)),

    {ok, Orig2} = parse(<<"o=jdoe 2890844526 2890842807 IN IP4 10.47.16.5">>), %% RFC 4566
    ?assertEqual(<<"jdoe">>, ersip_sdp_origin:username(Orig2)),
    ?assertEqual(2890844526, ersip_sdp_origin:session_id(Orig2)),
    ?assertEqual(2890842807, ersip_sdp_origin:session_version(Orig2)),
    ?assertEqual(ersip_sdp_addr:make(<<"IN IP4 10.47.16.5">>), ersip_sdp_origin:address(Orig2)),

    {ok, Orig3} = parse(<<"o=- 2873397496 0 ATM NSAP 47.0091.8100.0000.0060.3E64.FD01.0060.3E64.FD01.00">>), %% RFC 3108
    ?assertEqual(<<"-">>, ersip_sdp_origin:username(Orig3)),
    ?assertEqual(2873397496, ersip_sdp_origin:session_id(Orig3)),
    ?assertEqual(0, ersip_sdp_origin:session_version(Orig3)),
    ?assertEqual(ersip_sdp_addr:make(<<"ATM NSAP 47.0091.8100.0000.0060.3E64.FD01.0060.3E64.FD01.00">>),
                 ersip_sdp_origin:address(Orig3)),
    ok.

parse_error_test() ->
    ?assertMatch({error, {invalid_origin, _}}, ersip_sdp_origin:parse(<<"o=- 0 0 IN IP4 10.47.16.5">>)),
    ?assertMatch({error, {invalid_origin, _}}, parse(<<"o=2890844526 2890842807 IN IP4 10.47.16.5">>)),
    ?assertMatch({error, {invalid_origin, _}}, parse(<<"o=- 2890844526 -1 IN IP4 10.47.16.5">>)),
    ?assertMatch({error, {invalid_origin, _}}, parse(<<"o=- 2890844526 1a IN IP4 10.47.16.5">>)),
    ?assertMatch({error, {invalid_origin, _}}, parse(<<"o=- 2890844526 1 a@b IP4 10.47.16.5">>)),
    ?assertMatch({error, {invalid_origin, _}}, parse(<<"o=- 2890844526 1 IN I@4 10.47.16.5">>)),
    ?assertMatch({error, {invalid_origin, _}}, parse(<<"o=- 2890844526 1 IN IP4 10">>)),
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================

parse(Bin) ->
    case ersip_sdp_origin:parse(<<Bin/binary, "\r\n">>) of
        {ok, V, <<>>} ->
            {ok, V};
        {error, _} = Error ->
            Error
    end.
