%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Messages tests
%%

-module(ersip_msg_test).
-include_lib("eunit/include/eunit.hrl").

message_set_test() ->
    Vectors =
        [ { all,       type,   [ request, response ] },
          { response,  status, [ 100, 199, 200, 299, 300, 399, 400, 499, 500, 599 ] },
          { response,  reason, [ <<"OK">>, <<"Ringing">>, <<"Temorary Failure">> ] },
          { request,   method, [ <<"REGISTER">>, <<"INVITE">>, <<"ACK">> ] },
          { request,   ruri,   [ <<"sip:a@b">>, <<"sip:a@b:5060">>, <<"sip:a@b;tranport=tls">> ] }
        ],
    lists:foreach(fun({all, Item, Values}) ->
                          message_set(request, Item, Values),
                          message_set(response, Item, Values);
                     ({Type, Item, Values}) ->
                       message_set(Type, Item, Values)
                  end,
                  Vectors).

message_reset_type_test() ->
    M0 = ersip_msg:new(),
    M1 = ersip_msg:set([ {type, request} ], M0),
    ?assertEqual(request, ersip_msg:get(type, M1)),
    M1 = ersip_msg:set([ {type, request} ], M0).


message_multiget_test() ->
    M0 = ersip_msg:new(),
    M1 = ersip_msg:set([ {type,   request},
                         {method, <<"INVITE">> }
                         ], M0),
    ?assertEqual(
       [ {type,   request},
         {method, <<"INVITE">> }
       ],
       ersip_msg:get([type, method], M1)).

message_get_header_test() ->
    M0 = ersip_msg:new(),
    M1 = ersip_msg:add(<<"Some-Header">>,  ["1"], M0),
    M2 = ersip_msg:add(<<"Some-Header">>,  [<<"a">>, "b"], M1),
    ?assertEqual([ ["1"], [ <<"a">>, "b" ] ], ersip_msg:get(<<"SOME-HEADER">>, M2)).

message_set(Type, Item, Values) ->
    lists:foreach(
      fun(Val) ->
              Msg0 = ersip_msg:new(),
              Msg1 = ersip_msg:set([ {type, Type},
                                     {Item, Val } ], Msg0),
              ?assertEqual(Val, ersip_msg:get(Item, Msg1))
      end,
      Values).
