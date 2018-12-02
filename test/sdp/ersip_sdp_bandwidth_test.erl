%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP origin (o=) test
%%

-module(ersip_sdp_bandwidth_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

parse_test() ->
    %% RFC 3890:
    %% b=AS:60
    %% b=TIAS:50780
    BandBin1 = <<"b=AS:60" ?crlf
                 "b=CT:128" ?crlf
                 "b=TIAS:50780" ?crlf
               >>,
    {ok, Band1} = parse(BandBin1),
    ?assertEqual(50780, ersip_sdp_bandwidth:tias(Band1)),
    ?assertEqual(60,    ersip_sdp_bandwidth:as(Band1)),
    ?assertEqual(128,   ersip_sdp_bandwidth:ct(Band1)),

    %% RFC 4566:
    %% b=X-YZ:128
    BandBin2 = <<"b=X-YZ:128" ?crlf>>,
    {ok, Band2} = parse(BandBin2),
    ?assertEqual(128, ersip_sdp_bandwidth:experimental(<<"X-YZ">>, Band2)),
    ?assertEqual(128, ersip_sdp_bandwidth:experimental(<<"x-yz">>, Band2)),
    ?assertEqual(undefined, ersip_sdp_bandwidth:experimental(<<"X-YZK">>, Band2)),
    ?assertEqual(undefined, ersip_sdp_bandwidth:ct(Band2)),
    ?assertEqual(undefined, ersip_sdp_bandwidth:as(Band2)),
    ?assertEqual(undefined, ersip_sdp_bandwidth:tias(Band2)),
    ok.

parse_error_test() ->
    ?assertMatch({error, {invalid_bandwidth, _}}, parse(<<"b=AS:60">>)), %% no CRLF
    ?assertMatch({error, {invalid_bandwidth, _}}, parse(<<"b=AS" ?crlf>>)), %% no colon
    ?assertMatch({error, {invalid_bandwidth_type,  _}}, parse(<<"b=X-@:60" ?crlf>>)), %% not token
    ?assertMatch({error, {invalid_bandwidth_value, _}}, parse(<<"b=AS:a" ?crlf>>)), %% not number
    ?assertMatch({error, {invalid_bandwidth_value, _}}, parse(<<"b=AS:11 a" ?crlf>>)), %% garbage at end
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

parse(Bin) ->
    case ersip_sdp_bandwidth:parse(Bin) of
        {ok, Band, <<>>} ->
            {ok, Band};
        {ok, _, R} ->
            {error, {garbage_at_end, R}};
        {error, _} = Error ->
            Error
    end.
