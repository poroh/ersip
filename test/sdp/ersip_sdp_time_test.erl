%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP time fields (t=, r=, z=) test
%%

-module(ersip_sdp_time_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================
-define(crlf, "\r\n").

parse_test() ->
    T0 = <<"t=0 0" ?crlf>>,
    {ok, Time0, <<>>} = ersip_sdp_time:parse(T0),
    ?assertEqual(0, ersip_sdp_time:start(Time0)),
    ?assertEqual(0, ersip_sdp_time:stop(Time0)),

    T1 = <<"t=3034423619 3042462419" ?crlf
           "r=604800 3600 0 90000" ?crlf>>,
    {ok, Time1, <<>>} = ersip_sdp_time:parse(T1),
    ?assertEqual(3034423619, ersip_sdp_time:start(Time1)),
    ?assertEqual(3042462419, ersip_sdp_time:stop(Time1)),

    T3 = <<"t=3034423619 3042462419" ?crlf
           "r=604800 3600 0 90000" ?crlf
           "r=604800 3600 0 90000" ?crlf>>,
    ?assertMatch({ok, _, <<>>}, ersip_sdp_time:parse(T3)),

    T4 = <<"t=0 0" ?crlf
           "t=3034423619 3042462419" ?crlf
           "r=604800 3600 0 90000" ?crlf
           "r=7d 1h 0 25h" ?crlf
           "z=2882844526 -1h 2898848070 0" ?crlf>>,
    ?assertMatch({ok, _, <<>>}, ersip_sdp_time:parse(T4)),
    ok.

parse_error_test() ->
    ?assertMatch({error, no_time_specified}, ersip_sdp_time:parse(<<"a=sendrcev" ?crlf>>)),
    ?assertMatch({error, {invalid_time, _}}, ersip_sdp_time:parse(<<"t=0 b" ?crlf>>)),
    ?assertMatch({error, {invalid_time, _}}, ersip_sdp_time:parse(<<"t=b 0" ?crlf>>)),
    ?assertMatch({error, {invalid_time, _}}, ersip_sdp_time:parse(<<"t=0 0 0" ?crlf>>)),
    ?assertMatch({error, {invalid_time, _}}, ersip_sdp_time:parse(<<"t=-1 0" ?crlf>>)),
    ?assertMatch({error, {invalid_time, _}}, ersip_sdp_time:parse(<<"t=0 0">>)),
    ?assertMatch({error, {invalid_time, _}}, ersip_sdp_time:parse(<<"t=0 0" ?crlf "r=">>)),
    ?assertMatch({error, {invalid_zone, _}}, ersip_sdp_time:parse(<<"t=0 0" ?crlf "z=">>)),
    ok.
