%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Auxiliatry parsers tests
%%

-module(ersip_parse_aux_test).

-include_lib("eunit/include/eunit.hrl").

quoted_string_test() ->
    lists:foreach(fun(X) ->
                          check_ok(X, <<>>, X)
                  end,
                  [ <<"\"\"">>,
                    <<"\"abcd\"">>,
                    <<"\"a\\\"cd\"">>,
                    <<"\"a\\\"\"">>,
                    <<"\"", 16#c2, 16#a2, "\"">>,
                    <<"\"", 16#e2, 16#82, 16#ac, "\"">>,
                    <<"\"", 16#f0, 16#90, 16#8d, 16#88, "\"">>,
                    <<"\"", 16#f8, 16#01, 16#02, 16#03, 16#04, "\"">>,
                    <<"\"", 16#fc, 16#01, 16#02, 16#03, 16#04, 16#05, "\"">>
                  ]),
    lists:foreach(fun(X) ->
                          check_error(X)
                  end,
                  [ <<>>,
                    <<"a\"\"">>,
                    <<16#c2, 16#a2, "\"\"">>,
                    <<16#e2, 16#82, 16#ac, "\"\"">>,
                    <<16#f0, 16#90, 16#8d, 16#88, "\"\"">>,
                    <<"\"">>,
                    <<"\"", 16#c2, "\"">>,
                    <<"\"", 16#e2, 16#82, "\"">>,
                    <<"\"", 16#e2, "\"">>,
                    <<"\"", 16#f0, 16#90, 16#8d, "\"">>,
                    <<"\"", 16#f0, 16#90, "\"">>,
                    <<"\"", 16#f0, "\"">>,
                    <<"\"", 16#c2>>,
                    <<"\"", 16#e2, 16#82>>,
                    <<"\"", 16#e2>>,
                    <<"\"", 16#f0, 16#90, 16#8d>>,
                    <<"\"", 16#f0, 16#90>>,
                    <<"\"", 16#f0>>
                  ]),
    check_ok(<<"\"aaa\"">>, <<"bcd">>, <<"\"aaa\"bcd">>).
    
check_ok(Quoted, Rest, Sting) -> 
    ?assertEqual({ok, Quoted, Rest}, ersip_parser_aux:quoted_string(Sting)).
    
check_error(Sting) -> 
    ?assertEqual(error, ersip_parser_aux:quoted_string(Sting)).
    
