%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% iolist tests
%%

-module(ersip_hdr_test).

-include_lib("eunit/include/eunit.hrl").

as_integer_test() ->
    H0 = ersip_hdr:new(<<"Content-Length">>),
    H1 = ersip_hdr:add_value([ <<"1">>, <<"0">> ], H0),
    ?assertEqual({ok, 10}, ersip_hdr:as_integer(H1)).

