%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Auxiliatry parsers tests
%%

-module(ersip_parse_aux_test).

-include_lib("eunit/include/eunit.hrl").


%%%===================================================================
%%% Cases
%%%===================================================================

quoted_string_test() ->
    lists:foreach(fun(X) ->
                          qs_check_ok(X, <<>>, X)
                  end,
                  [<<"\"\"">>,
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
                          qs_check_error(X)
                  end,
                  [<<>>,
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
    qs_check_ok(<<"\"aaa\"">>, <<"bcd">>, <<"\"aaa\"bcd">>).

token_list_test() ->
    ?assertEqual({ok, [<<"a">>, <<"b">>], <<>>}, ersip_parser_aux:token_list(<<"a b">>, lws)),
    ?assertEqual({ok, [<<"a">>], <<>>}, ersip_parser_aux:token_list(<<"a">>, lws)),
    ?assertEqual(error, ersip_parser_aux:token_list(<<>>, lws)),
    ?assertEqual({ok, [<<"a">>, <<"b">>], <<>>}, ersip_parser_aux:token_list(<<"a", 9, "b">>, lws)),
    ?assertEqual({ok, [<<"a">>, <<"b">>], <<>>}, ersip_parser_aux:token_list(<<"  a ", 9, 9, "b    ">>, lws)),
    ?assertEqual({ok, [<<"a">>], <<"<sip:b>">>}, ersip_parser_aux:token_list(<<"a <sip:b>">>, lws)),
    ?assertEqual({ok, [<<"a">>], <<"<sip:b> <sip:d>">>}, ersip_parser_aux:token_list(<<"a <sip:b> <sip:d>">>, lws)),
    ?assertEqual(error, ersip_parser_aux:token_list(<<"<sip:b> <sip:d>">>, lws)),
    ?assertEqual(error, ersip_parser_aux:token_list(<<"<sip:b>">>, lws)).

parse_gen_param_value_test() ->
    ?assertEqual({ok, <<"a">>, <<>>}, ersip_parser_aux:parse_gen_param_value(<<"a">>)),
    ?assertEqual({ok, <<"\"a\"">>, <<>>}, ersip_parser_aux:parse_gen_param_value(<<"\"a\"">>)),
    ?assertEqual({ok, {ipv6, {0, 0, 0, 0, 0, 0, 0, 1}}, <<>>}, ersip_parser_aux:parse_gen_param_value(<<"[::1]">>)).


parse_lws_test() ->
    ?assertEqual({ok, {lws, 1}, <<>>}, ersip_parser_aux:parse_lws(<<" ">>)),
    ?assertEqual({ok, {lws, 2}, <<"xyz">>}, ersip_parser_aux:parse_lws(<<"  xyz">>)),
    ?assertEqual({ok, {lws, 3}, <<"xyz">>}, ersip_parser_aux:parse_lws(<<"  ", 9, "xyz">>)),
    ?assertMatch({error, _}, ersip_parser_aux:parse_lws(<<"xyz">>)).


parse_kvp_test() ->
    Validator = fun(Key, Value) -> {ok, {Key, Value}} end,
    {ok,
     [{<<"a">>, <<"b">>},
      {<<"c">>, <<"d">>}
     ],
     <<>>
    } = ersip_parser_aux:parse_kvps(Validator, <<";">>, <<"a=b;c=d">>),
    {ok,
     [{<<"a">>, <<"b">>},
      {<<"c">>, <<"d">>}
     ],
     <<>>
    } = ersip_parser_aux:parse_kvps(Validator, <<";">>, <<" a = b ; c = d">>),
    TransformValidator = fun(Key, Value) -> {ok, {Key, binary_to_integer(Value)}} end,
    {ok,
     [{<<"a">>, 1},
      {<<"c">>, 2}
     ],
     <<>>
    } = ersip_parser_aux:parse_kvps(TransformValidator, <<";">>, <<"a=1;c=2">>),
    SkipAValidator = fun(<<"a">>, _Value) ->
                             skip;
                        (Key, Value) ->
                             {ok, {Key, Value}}
                     end,
    {ok,
     [{<<"x">>, <<"1">>},
      {<<"c">>, <<"2">>}
     ],
     <<>>
    } = ersip_parser_aux:parse_kvps(SkipAValidator, <<";">>, <<"x=1;a=1;a;c=2">>),
    ErrorAValidator = fun(<<"a">>, _Value) ->
                              {error, unexpeted_a};
                         (Key, Value) ->
                              {ok, {Key, Value}}
                      end,

    {error, unexpeted_a}
        = ersip_parser_aux:parse_kvps(ErrorAValidator, <<";">>, <<"x=1;a=1;a;c=2">>),
    {error, unexpeted_a}
        = ersip_parser_aux:parse_kvps(ErrorAValidator, <<";">>, <<"x=1;a;a=1;c=2">>).


parse_params_test() ->
    Validator = fun(Key, Value) -> {ok, {Key, Value}} end,
    {ok,
     [{<<"a">>, <<"b">>},
      {<<"c">>, <<"d">>}
     ],
     <<>>
    } = ersip_parser_aux:parse_params(Validator, $;, <<"a=b;c=d">>),
    ?assertMatch({error, _}, ersip_parser_aux:parse_params(Validator, $;, <<"a=\"">>)),
    ok.

%%%===================================================================
%%% Implementation
%%%===================================================================

qs_check_ok(Quoted, Rest, Sting) ->
    ?assertEqual({ok, Quoted, Rest}, ersip_parser_aux:quoted_string(Sting)).

qs_check_error(Sting) ->
    ?assertEqual(error, ersip_parser_aux:quoted_string(Sting)).
