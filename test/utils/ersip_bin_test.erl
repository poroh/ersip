%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Binaries test
%%

-module(ersip_bin_test).
-include_lib("eunit/include/eunit.hrl").

to_lower_test() ->
    ?assertEqual(<<"aa1bbzzdd_@%20">>, ersip_bin:to_lower(<<"AA1BBZZDD_@%20">>)),
    ok.

-dialyzer({nowarn_function, to_lower_error_test/0}).
to_lower_error_test() ->
    ?assertMatch({'EXIT', {badarg, _}}, catch ersip_bin:to_lower(1)),
    ?assertMatch({'EXIT', {badarg, _}}, catch ersip_bin:to_lower(atom)),
    ok.

to_upper_test() ->
    ?assertEqual(<<"AA1BBZZDD_@%20">>, ersip_bin:to_upper(<<"aa1bbzzdd_@%20">>)),
    ok.

-dialyzer({nowarn_function, to_upper_error_test/0}).
to_upper_error_test() ->
    ?assertMatch({'EXIT', {badarg, _}}, catch ersip_bin:to_upper(1)),
    ?assertMatch({'EXIT', {badarg, _}}, catch ersip_bin:to_upper(atom)),
    ok.


trim_lws_test() ->
    ?assertEqual(<<"aa1bbzzdd_@%20">>, ersip_bin:trim_head_lws(<<"   \t  aa1bbzzdd_@%20">>)),
    ?assertEqual(<<"aa1bbzzdd_@%20">>, ersip_bin:trim_head_lws(<<"\t   aa1bbzzdd_@%20">>)),
    ?assertEqual(<<>>, ersip_bin:trim_head_lws(<<"\t   ">>)),
    ?assertEqual(<<>>, ersip_bin:trim_head_lws(<<"  \t">>)),
    ?assertEqual(<<>>, ersip_bin:trim_head_lws(<<>>)).


unquote_test() ->
    ?assertEqual(<<" ">>, ersip_bin:unquote_rfc_2396(<<"%20">>)),
    ?assertEqual(<<"/">>, ersip_bin:unquote_rfc_2396(<<"%2f">>)),
    ?assertEqual(<<"*/">>, ersip_bin:unquote_rfc_2396(<<"%2A%2F">>)).
