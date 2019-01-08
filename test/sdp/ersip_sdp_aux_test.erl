%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP auxiliary function test
%%

-module(ersip_sdp_aux_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

check_token_test() ->
    %% %x21 / %x23-27 / %x2A-2B / %x2D-2E / %x30-39 / %x41-5A / %x5E-7E
    [?assertEqual(false, ersip_sdp_aux:check_token(<<C:8>>)) || C <- lists:seq(0, 16#20)],
    ?assertEqual(true,   ersip_sdp_aux:check_token(<<16#21>>)),
    ?assertEqual(false,  ersip_sdp_aux:check_token(<<16#22>>)), %% '"'
    [?assertEqual(true,  ersip_sdp_aux:check_token(<<C:8>>)) || C <- lists:seq(16#23, 16#27)],
    ?assertEqual(false,  ersip_sdp_aux:check_token(<<16#28>>)), %% '('
    ?assertEqual(false,  ersip_sdp_aux:check_token(<<16#29>>)), %% ')'
    [?assertEqual(true,  ersip_sdp_aux:check_token(<<C:8>>)) || C <- lists:seq(16#2A, 16#2B)],
    ?assertEqual(false,  ersip_sdp_aux:check_token(<<16#2C>>)), %% ','
    [?assertEqual(true,  ersip_sdp_aux:check_token(<<C:8>>)) || C <- lists:seq(16#2D, 16#2E)],
    ?assertEqual(false,  ersip_sdp_aux:check_token(<<16#2F>>)), %% '/'
    [?assertEqual(true,  ersip_sdp_aux:check_token(<<C:8>>)) || C <- lists:seq(16#30, 16#39)],
    ?assertEqual(false,  ersip_sdp_aux:check_token(<<16#40>>)), %% '@'
    [?assertEqual(true,  ersip_sdp_aux:check_token(<<C:8>>)) || C <- lists:seq(16#41, 16#5A)],
    ?assertEqual(false,  ersip_sdp_aux:check_token(<<16#5B>>)), %% '['
    ?assertEqual(false,  ersip_sdp_aux:check_token(<<16#5C>>)), %% '\'
    ?assertEqual(false,  ersip_sdp_aux:check_token(<<16#5D>>)), %% ']'
    [?assertEqual(true,  ersip_sdp_aux:check_token(<<C:8>>)) || C <- lists:seq(16#5E, 16#7E)],
    [?assertEqual(false, ersip_sdp_aux:check_token(<<C:8>>)) || C <- lists:seq(16#7f, 16#ff)],
    ?assertEqual(false, ersip_sdp_aux:check_token(<<>>)),
    ok.

parse_token_test() ->
    ?assertEqual({ok, <<"RTP">>, <<"/AVP">>}, ersip_sdp_aux:parse_token(<<"RTP/AVP">>)),
    ?assertEqual({ok, <<"RTP">>, <<"">>}, ersip_sdp_aux:parse_token(<<"RTP">>)),
    [?assertEqual({ok, <<Char:8>>, <<"/Rest">>}, ersip_sdp_aux:parse_token(<<Char:8, "/Rest">>)) || Char <- token_chars()],
    AllChars = list_to_binary(token_chars()),
    ?assertEqual({ok, AllChars, <<"/AVP">>}, ersip_sdp_aux:parse_token(<<AllChars/binary, "/AVP">>)),
    ?assertMatch({error, token_expected}, ersip_sdp_aux:parse_token(<<"/AVP">>)),
    ?assertMatch({error, token_expected}, ersip_sdp_aux:parse_token(<<>>)),
    ok.

parse_key_test() ->
    ?assertEqual({ok, <<"key value">>, <<>>}, ersip_sdp_aux:parse_key(<<"k=key value\r\n">>)),
    ?assertEqual({ok, undefined, <<"m=audio 2123 RTP/AVP 0\r\n">>}, ersip_sdp_aux:parse_key(<<"m=audio 2123 RTP/AVP 0\r\n">>)),
    ?assertMatch({error, {invalid_key, _}}, ersip_sdp_aux:parse_key(<<"k=key value">>)),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================


%% token-char =  %x21 / %x23-27 / %x2A-2B / %x2D-2E / %x30-39
%%               / %x41-5A / %x5E-7E
token_chars() ->
    lists:flatten([16#21,
                   lists:seq(16#23, 16#27),
                   lists:seq(16#2A, 16#2B),
                   lists:seq(16#2D, 16#2E),
                   lists:seq(16#30, 16#39),
                   lists:seq(16#41, 16#5A),
                   lists:seq(16#5E, 16#7E)]).
