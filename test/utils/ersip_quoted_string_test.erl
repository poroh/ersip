%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Quoted string test
%%%

-module(ersip_quoted_string_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

skip_test() ->
    ?assertEqual({ok, <<"">>},    ersip_quoted_string:skip(<<"\"abc\"">>)),
    ?assertEqual({ok, <<"abc">>}, ersip_quoted_string:skip(<<"\"abc\"abc">>)),
    ?assertEqual({ok, <<"">>},    ersip_quoted_string:skip(<<"\"\"">>)),
    ?assertEqual({ok, <<"a">>},   ersip_quoted_string:skip(<<"\"\"a">>)),
    ?assertEqual({ok, <<"a">>},   ersip_quoted_string:skip(<<"\"\\\"\"a">>)),
    ?assertEqual(error,           ersip_quoted_string:skip(<<"abc">>)),
    ?assertEqual(error,           ersip_quoted_string:skip(<<"">>)),
    ?assertEqual(error,           ersip_quoted_string:skip(<<"\"\\", 16#FF, "\"">>)),
    ok.

quote_test() ->
    ?assertEqual(<<"\"abc\"">>,   ersip_quoted_string:quote(<<"abc">>)),
    ?assertEqual(<<"\"\\\"\"">>,  ersip_quoted_string:quote(<<"\"">>)),
    ?assertEqual(<<"\"абв\"">>,   ersip_quoted_string:quote(<<"абв">>)),
    ?assertEqual(<<"\"你好，世界\"">>, ersip_quoted_string:quote(<<"你好，世界">>)),
    ?assertEqual(<<"\"I love using \\\"quotes\\\" in the sentences\"">>,
                 ersip_quoted_string:quote(<<"I love using \"quotes\" in the sentences">>)),
    ?assertEqual(<<"\"\\\\\"">>, ersip_quoted_string:quote(<<"\\">>)),
    ok.
