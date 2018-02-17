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
    ?assertMatch({'EXIT', {badarg, _}}, catch ersip_bin:to_lower(1)),
    ?assertMatch({'EXIT', {badarg, _}}, catch ersip_bin:to_lower(atom)).


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
