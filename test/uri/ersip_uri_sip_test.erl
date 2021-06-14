%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP URI tests
%%%

-module(ersip_uri_sip_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

secrue_test() ->
    ?assertEqual(true, ersip_uri_sip:secure(ersip_uri_sip:make(<<"sips:a@b">>))),
    ?assertEqual(false, ersip_uri_sip:secure(ersip_uri_sip:make(<<"sip:a@b">>))),
    ok.

make_test() ->
    ?assertError({invalid_sip_uri, _}, ersip_uri_sip:make(<<"tel:123">>)),
    ok.
