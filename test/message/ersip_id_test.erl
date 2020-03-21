%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Identifiers test
%%

-module(ersip_id_test).

-include("ersip_sip_abnf.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

token_test() ->
    ?assert(ersip_parser_aux:check_token(ersip_id:token(<<1,2,3,4,5>>))),
    [?assert(ersip_parser_aux:check_token(ersip_id:token(crypto:strong_rand_bytes(7)))) || _ <- lists:seq(1, 100)],
    ok.

word_test() ->
    ?assertMatch({ok, _}, ersip_hdr_callid:parse(ersip_id:word(<<1,2,3,4,5>>))),
    [?assertMatch({ok, _}, ersip_hdr_callid:parse(ersip_id:word(crypto:strong_rand_bytes(7)))) || _ <- lists:seq(1, 100)],
    ok.

alphanum_test() ->
    ?assertMatch(ok, check_alphanum(ersip_id:alphanum(<<1,2,3,4,5>>))),
    [?assertMatch(ok, check_alphanum(ersip_id:alphanum(crypto:strong_rand_bytes(7)))) || _ <- lists:seq(1, 100)],
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================


check_alphanum(<<>>) ->
    ok;
check_alphanum(<<Char/utf8, Rest/binary>>) when ?is_alphanum(Char) ->
    check_alphanum(Rest);
check_alphanum(<<Char/utf8, _/binary>>) ->
    {failed, Char}.


