%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Display Name functions tests
%%%

-module(ersip_display_name_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

raw_test() ->
    ?assertEqual(<<"Alice">>,          ersip_display_name:raw(make(<<"\"Alice\"">>))),
    ?assertEqual(<<"Lewis Carroll">>,  ersip_display_name:raw(make(<<"Lewis Carroll">>))),
    ?assertEqual(<<"Lewis Carroll">>, ersip_display_name:raw(make(<<"\"Lewis Carroll\"">>))),
    ?assertEqual(<<"Theodore \"Teddy\" Roosevelt">>, ersip_display_name:raw(make(<<"Theodore \"Teddy\" Roosevelt">>))),
    ?assertEqual(<<"Theodore \"Teddy\" Roosevelt">>, ersip_display_name:raw(make(<<"\"Theodore \\\"Teddy\\\" Roosevelt\"">>))),
    ?assertEqual(<<"<sip:a@b> <sip:a@b>">>, ersip_display_name:raw(make(<<"<sip:a@b> <sip:a@b>">>))),
    ok.

parse_dn_test() ->
    ?assertEqual({ok, ersip_display_name:empty(), <<"">>}, ersip_display_name:parse_dn(<<"">>)),
    ?assertEqual({ok, make(<<"sip">>), <<":a@b">>}, ersip_display_name:parse_dn(<<"sip:a@b">>)),
    ?assertEqual({ok, make(<<"Alice">>), <<"<sip:a@b>">>}, ersip_display_name:parse_dn(<<"Alice <sip:a@b>">>)),
    ?assertEqual({ok, make(<<"\"Alice\"">>), <<" <sip:a@b>">>}, ersip_display_name:parse_dn(<<"\"Alice\" <sip:a@b>">>)),
    ok.

%%===================================================================
%% Helpers
%%===================================================================

make(Bin) ->
    ersip_display_name:make(Bin).
