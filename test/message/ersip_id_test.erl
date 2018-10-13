%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Identifiers test
%%

-module(ersip_id_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

token_test() ->
    ?assert(ersip_parser_aux:check_token(ersip_id:token(<<1,2,3,4,5>>))),
    [?assert(ersip_parser_aux:check_token(ersip_id:token(crypto:strong_rand_bytes(7)))) || _ <- lists:seq(1, 100)],
    ok.
