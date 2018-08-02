%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Messages tests
%%

-module(ersip_method_test).
-include_lib("eunit/include/eunit.hrl").

construction_test() ->
    {ok, AAAA} =  ersip_method:parse(<<"AAAA">>),
    {ok, BBBB} =  ersip_method:parse(<<"BBBB">>),
    ?assertEqual(AAAA, ersip_method:make(<<"AAAA">>)),
    ?assertNotEqual(BBBB, ersip_method:make(<<"AAAA">>)),
    ?assertError({error, {invalid_method, _}}, ersip_method:make(<<"  ">>)).


