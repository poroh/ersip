%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Messages tests
%%

-module(ersip_msg_test).
-include_lib("eunit/include/eunit.hrl").

-define(crlf, "\r\n").

message_set_test() ->
    Vectors =
        [{all,       type,   [request, response]},
         {response,  status, [100, 199, 200, 299, 300, 399, 400, 499, 500, 599]},
         {response,  reason, [<<"OK">>, <<"Ringing">>, <<"Temorary Failure">>]},
         {request,   method, lists:map(fun ersip_method:make/1, [<<"REGISTER">>, <<"INVITE">>, <<"ACK">>])},
         {request,   ruri,   [<<"sip:a@b">>, <<"sip:a@b:5060">>, <<"sip:a@b;tranport=tls">>]}
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
    M1 = ersip_msg:set([{type, request}], M0),
    ?assertEqual(request, ersip_msg:get(type, M1)),
    M1 = ersip_msg:set([{type, request}], M0).


message_multiget_test() ->
    M0 = ersip_msg:new(),
    M1 = ersip_msg:set([{type,   request},
                        {method, <<"INVITE">>}
                       ], M0),
    ?assertEqual(
       [{type,   request},
        {method, ersip_method:make(<<"INVITE">>)}
       ],
       ersip_msg:get([type, method], M1)).

message_get_header_test() ->
    M0 = ersip_msg:new(),
    M1 = ersip_msg:add(<<"Some-Header">>,  ["1"], M0),
    M2 = ersip_msg:add(<<"Some-Header">>,  [<<"a">>, "b"], M1),
    Hdr = ersip_msg:get(<<"SOME-HEADER">>, M2),
    ?assertEqual(lists:map(fun iolist_to_binary/1,
                           [["1"], [<<"a">>, "b"]]),
                 lists:map(fun iolist_to_binary/1,
                           ersip_hdr:raw_values(Hdr) )).

message_empty_header_test() ->
    M0 = ersip_msg:new(),
    ?assertEqual(M0,  ersip_msg:add(<<"Some-Header">>,  ["   \t"], M0)).

message_serialize_req_test() ->
    M0 = ersip_msg:new(),
    M1 = ersip_msg:set([{type,   request},
                        {method, <<"INVITE">>},
                        {ruri,       <<"sip:alice@example.com">>},
                        {<<"Via">>,  <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>},
                        {<<"From">>, <<"sip:bob@biloxi.com">>},
                        {<<"To">>,   <<"sip:alice@example.com">>},
                        {<<"Max-Forwards">>, <<"70">>},
                        {<<"CSeq">>, <<"1 INVITE">>},
                        {<<"Call-Id">>, <<"some-call-id">>}
                       ], M0),
    ExpectedMessage = <<"INVITE sip:alice@example.com SIP/2.0"
                        ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
                        ?crlf "To: sip:alice@example.com"
                        ?crlf "From: sip:bob@biloxi.com"
                        ?crlf "Call-Id: some-call-id"
                        ?crlf "CSeq: 1 INVITE"
                        ?crlf "Max-Forwards: 70"
                        ?crlf ?crlf>>,
    ?assertEqual(ExpectedMessage, ersip_msg:serialize_bin(M1)).

message_serialize_resp_test() ->
    M0 = ersip_msg:new(),
    M1 = ersip_msg:set([{type,   response},
                        {status, 200},
                        {reason, <<"OK">>},
                        {<<"Via">>,
                         {mval,
                          [<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1">>,
                           <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>]}},
                        {<<"From">>, <<"sip:bob@biloxi.com">>},
                        {<<"To">>,   <<"sip:alice@example.com">>},
                        {<<"CSeq">>, <<"1 INVITE">>},
                        {<<"Call-Id">>, <<"some-call-id">>}
                       ], M0),
    ExpectedMessage = <<"SIP/2.0 200 OK"
                        ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1"
                        ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
                        ?crlf "To: sip:alice@example.com"
                        ?crlf "From: sip:bob@biloxi.com"
                        ?crlf "Call-Id: some-call-id"
                        ?crlf "CSeq: 1 INVITE"
                        ?crlf ?crlf>>,
    ?assertEqual(ExpectedMessage, ersip_msg:serialize_bin(M1)).

message_set_invalid_method_test() ->
    M0 = ersip_msg:new(),
    ?assertError({invalid_method_type, _},
                 ersip_msg:set([{type,   request},
                                {method, ok}],
                               M0)).

message_get_headers_test() ->
    M0 = ersip_msg:new(),
    M1 = ersip_msg:set([{type,   request},
        {method, <<"INVITE">>},
        {ruri,       <<"sip:alice@example.com">>},
        {<<"Via">>,  <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>},
        {<<"From">>, <<"sip:bob@biloxi.com">>},
        {<<"To">>,   <<"sip:alice@example.com">>},
        {<<"Max-Forwards">>, <<"70">>},
        {<<"CSeq">>, <<"1 INVITE">>},
        {<<"Call-Id">>, <<"some-call-id">>}
    ], M0),
    Headers = ersip_msg:get_headers(M1),
    ?assertEqual([<<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>],
        get_header_values(<<"Via">>, Headers)),
    ?assertEqual([<<"sip:bob@biloxi.com">>],
        get_header_values(<<"From">>, Headers)),
    ?assertEqual([<<"sip:alice@example.com">>],
        get_header_values(<<"To">>, Headers)),
    ?assertEqual([<<"70">>],
        get_header_values(<<"Max-Forwards">>, Headers)),
    ?assertEqual([<<"1 INVITE">>],
        get_header_values(<<"CSeq">>, Headers)),
    ?assertEqual([<<"some-call-id">>],
        get_header_values(<<"Call-Id">>, Headers)).


clear_headers_test() ->
    M0 = ersip_msg:new(),
    M1 = ersip_msg:set([{type,   request},
        {method, <<"INVITE">>},
        {ruri,       <<"sip:alice@example.com">>},
        {<<"Via">>,  <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>},
        {<<"From">>, <<"sip:bob@biloxi.com">>},
        {<<"To">>,   <<"sip:alice@example.com">>},
        {<<"Max-Forwards">>, <<"70">>},
        {<<"CSeq">>, <<"1 INVITE">>},
        {<<"Call-Id">>, <<"some-call-id">>}
    ], M0),
    M2 = ersip_msg:clear_headers(M1),
    ?assertEqual([], ersip_msg:get_headers(M2)),
    ok.

message_set(Type, Item, Values) ->
    lists:foreach(
      fun(Val) ->
              Msg0 = ersip_msg:new(),
              Msg1 = ersip_msg:set([{type, Type},
                                    {Item, Val}], Msg0),
              ?assertEqual(Val, ersip_msg:get(Item, Msg1))
      end,
      Values).

get_header_values(Name, Headers) ->
    [Header] = lists:filter(fun(H) ->
                                case ersip_hdr:name(H) of
                                    Name ->
                                        true;
                                    _ ->
                                        false
                                end
                            end,
        Headers),
    ersip_hdr:raw_values(Header).


