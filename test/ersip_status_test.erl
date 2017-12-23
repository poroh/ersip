%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Status codes tests
%%

-module(ersip_status_test).

-include_lib("eunit/include/eunit.hrl").

response_type_test() ->
    ?assertEqual(provisional, ersip_status:response_type(100)),
    ?assertEqual(provisional, ersip_status:response_type(199)),
    ?assertEqual(final, ersip_status:response_type(200)),
    ?assertEqual(final, ersip_status:response_type(299)),
    ?assertEqual(final, ersip_status:response_type(300)),
    ?assertEqual(final, ersip_status:response_type(399)),
    ?assertEqual(final, ersip_status:response_type(400)),
    ?assertEqual(final, ersip_status:response_type(499)),
    ?assertEqual(final, ersip_status:response_type(500)),
    ?assertEqual(final, ersip_status:response_type(599)),
    ?assertEqual(final, ersip_status:response_type(600)),
    ?assertEqual(final, ersip_status:response_type(699)),
    ?assertException(error, function_clause, ersip_status:response_type(99)),
    ?assertException(error, function_clause, ersip_status:response_type(700)),
    ?assertException(error, function_clause, ersip_status:response_type(a)).
