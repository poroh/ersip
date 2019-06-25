%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message parser tests
%%

-module(ersip_parser_test).

-include_lib("eunit/include/eunit.hrl").

-define(crlf, "\r\n").

basic_request_parse_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    ?assertEqual([<<"Test">>], ersip_msg:get(body, PMsg)).

basic_request_without_body_parse_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 0"
            ?crlf ?crlf
          >>,
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    ?assertEqual({method, <<"INVITE">>}, ersip_msg:get(method, PMsg)).

basic_response_parse_test() ->
    Msg = <<"SIP/2.0 200 OK"
            ?crlf "Via: SIP/2.0/UDP server10.biloxi.com"
            ?crlf "   ;branch=z9hG4bKnashds8;received=192.0.2.3"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "   ;branch=z9hG4bK77ef4c2312983.1;received=192.0.2.2"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com"
            ?crlf "   ;branch=z9hG4bK776asdhds ;received=192.0.2.1"
            ?crlf "To: Bob <sip:bob@biloxi.com>;tag=a6c85cf"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:bob@192.0.2.4>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    ?assertEqual([<<"Test">>], ersip_msg:get(body, PMsg)).

truncated_message_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf ""
          >>,
    P  = ersip_parser:new_dgram(Msg),
    {{error, {bad_message, _}}, _P2} = ersip_parser:parse(P).

message_too_long_on_success_parse_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "test"
          >>,
    P  = ersip_parser:new(#{max_message_len => byte_size(Msg)-1}),
    P1 = ersip_parser:add_binary(Msg, P),
    ?assertMatch({{error, message_too_long}, _P2}, ersip_parser:parse(P1)),
    ok.

message_not_too_long_on_success_parse_test() ->
    %% Same as message_too_long_on_success_parse_test but when
    %% max_message_len is exactly match message length
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "test"
          >>,
    P  = ersip_parser:new(#{max_message_len => byte_size(Msg)}),
    P1 = ersip_parser:add_binary(Msg, P),
    ?assertMatch({{ok, _Message}, _P2}, ersip_parser:parse(P1)),
    ok.

message_too_long_on_header_parse_test() ->
    Msg0 = <<"INVITE sip:bob@biloxi.com SIP/2.0"
             ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
             ?crlf "Max-Forwards: 70"
             ?crlf "To: Bob <sip:bob@biloxi.com>"
             ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
             ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
             ?crlf "CSeq: 314159 INVITE"
             ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
             ?crlf "Content-Type: application/sdp"
             ?crlf "Content-Length: 4">>,
    P  = ersip_parser:new(#{max_message_len => byte_size(Msg0)-1}),
    P1 = ersip_parser:add_binary(Msg0, P),
    ?assertMatch({{error, message_too_long}, _P2}, ersip_parser:parse(P1)),
    ok.

message_too_long_on_first_line_parse_test() ->
    Msg0 = <<"INVITE sip:bob@biloxi.com SIP/2.0">>,
    P  = ersip_parser:new(#{max_message_len => byte_size(Msg0)-1}),
    P1 = ersip_parser:add_binary(Msg0, P),
    ?assertMatch({{error, message_too_long}, _P2}, ersip_parser:parse(P1)),
    ok.


invalid_method_test() ->
    Msg = <<"INV@TE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    P  = ersip_parser:new_dgram(Msg),
    {{error, {bad_message, _}}, _P2} = ersip_parser:parse(P).

no_content_len_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf ?crlf "Test"
          >>,
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    ?assertEqual([<<"Test">>], ersip_msg:get(body, PMsg)).

invalid_content_len_dgram_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: invalid"
            ?crlf ?crlf "Test"
          >>,
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    ?assertEqual([<<"Test">>], ersip_msg:get(body, PMsg)).

invalid_content_len_stream_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: invalid"
            ?crlf ?crlf "Test"
          >>,
    P  = ersip_parser:new(),
    P1 = ersip_parser:add_binary(Msg, P),
    {{error, {bad_message, _}}, _P2} = ersip_parser:parse(P1).

double_content_len_stream_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf "Content-Length: 5"
            ?crlf ?crlf "Test"
          >>,
    P  = ersip_parser:new(),
    P1 = ersip_parser:add_binary(Msg, P),
    {{error, {bad_message, _}}, _P2} = ersip_parser:parse(P1).


incremental_data_test() ->
    Msgs = [<<"INVITE sip:bob@biloxi.com">>,
            <<" SIP/2.0" ?crlf>>,
            <<"Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
              ?crlf "Max-Forwards: 70"
              ?crlf "To: Bob <sip:bob@biloxi.com>"
              ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
              ?crlf "Call-ID: a84b4c76e66710@pc33">>,
            <<".atlanta.com"
              ?crlf "CSeq: 314159 INVITE"
              ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
              ?crlf "Content-Type: application/sdp"
              ?crlf "Content-Length: 1">>,
            <<"0" ?crlf ?crlf "01234">>],
    P0 = ersip_parser:new(),
    P1 =
        lists:foldl(fun(Msg, P) ->
                            P1 = ersip_parser:add_binary(Msg, P),
                            {more_data, P2} = ersip_parser:parse(P1),
                            P2
                    end,
                    P0,
                    Msgs),
    P2 = ersip_parser:add_binary(<<"56789">>, P1),
    {{ok, PMsg}, _P3} = ersip_parser:parse(P2),
    ?assertEqual([<<"01234">>, <<"56789">>], ersip_msg:get(body, PMsg)).


a_lot_of_crlfs_test() ->
    P0 = ersip_parser:new(#{max_message_len => 5}),
    P1 = ersip_parser:add_binary(<<"\r\n">>, P0),
    {more_data, P2} = ersip_parser:parse(P1),
    P3 = ersip_parser:add_binary(<<"\r\n\r\n\r\n\r\n">>, P2),
    {more_data, _} = ersip_parser:parse(P3),
    ok.

negative_cases_test_() ->
    [?_assertMatch({error, {bad_status_line,_}}, parse_fail_reason(<<"SIP/2.0 700 OK", ?crlf>>)),
     ?_assertMatch({error, {bad_status_line,_}}, parse_fail_reason(<<"SIP/2.0 2000 OK", ?crlf>>)),
     ?_assertMatch({error, {bad_status_line,_}}, parse_fail_reason(<<"SIP/3.0 200 OK", ?crlf>>)),
     ?_assertMatch({error, {bad_message, {bad_request_line,_}}}, parse_fail_reason(<<"REGISTER", ?crlf>>)),
     ?_assertMatch({error, {bad_header, _}}, parse_fail_reason(
                                               <<"REGISTER alice@example.com SIP/2.0" ?crlf,
                                                 "No real header here" ?crlf
                                                 "Via: X" ?crlf>>)),
     ?_assertMatch({error, {bad_header, _}}, parse_fail_reason(
                                               <<"REGISTER alice@example.com SIP/2.0" ?crlf,
                                                 "No real header here" ?crlf ?crlf>>)),
     ?_assertMatch({error, {bad_message, no_headers}}, parse_fail_reason(
                                                         <<"REGISTER alice@example.com SIP/2.0" ?crlf,
                                                           ?crlf>>))
    ].

parse_fail_reason(Message) ->
    P0 = ersip_parser:new(),
    P1 = ersip_parser:add_binary(Message, P0),
    {Reason, _} = ersip_parser:parse(P1),
    Reason.
