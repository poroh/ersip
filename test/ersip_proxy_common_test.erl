%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common proxy routinges tests
%%

-module(ersip_proxy_common_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

stateless_proxy_forward_to_proxy_test() ->
    %% Checking strict routing information update:
    %%
    %% If RURI is this proxy then it must be replaced by value from
    %% last Route, last Route must be removed from the message.
    BobURI = <<"sip:bob@biloxi.com">>,
    ThisProxyURI = <<"sip:this.proxy.org">>,
    NextProxyURI = <<"sip:next.proxy.org">>,
    %% Check strict-routing message recovery
    Msg = <<"INVITE ", BobURI/binary, " SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Route: <", ThisProxyURI/binary, ">"
            ?crlf "Route: <", NextProxyURI/binary, ">"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOptions =
        #{check_rroute_fun =>
              fun(URI) ->
                      ersip_uri:make(ThisProxyURI) == URI
              end},
    Options = #{proxy => ProxyOptions},
    SipMsg0 = process_route_info(raw_message(Msg), Options),
    Target = ersip_uri:make(NextProxyURI),
    {SipMsg2, #{nexthop := NexthopURI}} = ersip_proxy_common:forward_request(Target, SipMsg0, ProxyOptions),
    ?assertEqual(Target, NexthopURI),
    %% Check route has only one element NextProxyURI.
    ExpectedRouteSet = ersip_hdr_route:make(<<"<", NextProxyURI/binary, ">">>),
    ?assertEqual(ExpectedRouteSet, ersip_sipmsg:get(route, SipMsg2)),
    ok.

stateless_proxy_forward_to_ua_test() ->
    %% Checking strict routing information update:
    %%
    %% If RURI is this proxy then it must be replaced by value from
    %% last Route, last Route must be removed from the message.
    BobURI = <<"sip:bob@biloxi.com">>,
    ThisProxyURI = <<"sip:this.proxy.org">>,
    %% Check strict-routing message recovery
    Msg = <<"INVITE ", BobURI/binary, " SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Route: <", ThisProxyURI/binary, ">"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOptions =
        #{check_rroute_fun =>
              fun(URI) ->
                      ersip_uri:make(ThisProxyURI) == URI
              end},
    Options = #{proxy => ProxyOptions},
    SipMsg0 = process_route_info(raw_message(Msg), Options),
    Target = ersip_uri:make(BobURI),
    {SipMsg2, #{nexthop := NexthopURI}} = ersip_proxy_common:forward_request(Target, SipMsg0, ProxyOptions),
    ?assertEqual(Target, NexthopURI),
    %% Check route does not have any elements
    ?assertEqual(not_found, ersip_sipmsg:find(route, SipMsg2)),
    ok.



request_validation_success_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ok, _}, request_validation(raw_message(Msg))),
    ok.

request_validation_success_no_maxforwards_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({ok, _}, request_validation(raw_message(Msg))),
    ok.

request_validation_bad_maxforwards_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: xyz"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {reply, BadMsg} = request_validation(raw_message(Msg)),
    ?assertEqual(400, ersip_sipmsg:status(BadMsg)),
    Reason = ersip_sipmsg:reason(BadMsg),
    ?assert(binary:match(ersip_bin:to_lower(Reason), <<"max-forwards">>) =/= nomatch),
    ok.

request_validation_unsupported_scheme_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {reply, BadMsg} = request_validation(
                        raw_message(Msg),
                        #{validate => #{scheme_val_fun => fun(_) -> false end}}),
    ?assertEqual(416, ersip_sipmsg:status(BadMsg)),
    Reason = ersip_sipmsg:reason(BadMsg),
    ?assertEqual(<<"Unsupported URI Scheme">>, Reason),
    ok.

request_validation_unsupported_scheme_cannot_reply_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({error, _},
                 request_validation(
                   raw_message(Msg),
                   #{validate => #{scheme_val_fun => fun(_) -> false end}})),
    ok.

request_validation_no_resp_to_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: xyz"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({error, _}, request_validation(raw_message(Msg))),
    ok.

request_validation_no_resp_from_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: xyz"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ?assertMatch({error, _}, request_validation(raw_message(Msg))),
    ok.

request_validation_maxforwards_is_zero_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 0"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    {reply, BadMsg} = request_validation(raw_message(Msg)),
    ?assertEqual(483, ersip_sipmsg:status(BadMsg)),
    Reason = ersip_sipmsg:reason(BadMsg),
    ?assertEqual(<<"Too many hops">>, Reason),
    ok.

request_validation_maxforwards_is_zero_options_test() ->
    Msg = <<"OPTIONS sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 0"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 OPTIONS"
            ?crlf ?crlf
          >>,
    {reply, BadMsg} = request_validation(raw_message(Msg)),
    ?assertEqual(483, ersip_sipmsg:status(BadMsg)),
    Reason = ersip_sipmsg:reason(BadMsg),
    ?assertEqual(<<"Too many hops">>, Reason),
    ok.

request_validation_maxforwards_is_zero_options_reply_test() ->
    Msg = <<"OPTIONS sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 0"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 OPTIONS"
            ?crlf ?crlf
          >>,
    AllowMethodsList = [<<"INVITE">>,
                        <<"ACK">>,
                        <<"OPTIONS">>,
                        <<"CANCEL">>,
                        <<"BYE">>
                       ],
    SupportedList = [<<"100rel">>, <<"timers">>],
    AllowMethods =
        ersip_hdr_allow:from_list(
          [ersip_method:make(M) || M <- AllowMethodsList]),
    Supported =
        ersip_hdr_opttag_list:from_list(
          [ersip_option_tag:make(S) || S <- SupportedList]),
    Options = #{validate => #{reply_on_options => true},
                proxy => #{
                  allow => AllowMethods,
                  supported => Supported
                 }
               },
    {reply, RespMsg} = request_validation(raw_message(Msg), Options),
    ?assertEqual(200, ersip_sipmsg:status(RespMsg)),
    Reason = ersip_sipmsg:reason(RespMsg),
    ?assertEqual(<<"OK">>, Reason),
    RespRawMsg = raw_message(ersip_sipmsg:serialize_bin(RespMsg)),
    {ok, RespSipMsg} = ersip_sipmsg:parse(RespRawMsg, [allow, supported]),
    ?assertEqual(AllowMethods, ersip_sipmsg:get(allow, RespSipMsg)),
    ?assertEqual(Supported, ersip_sipmsg:get(supported, RespSipMsg)),
    ok.

request_validation_maxforwards_is_zero_options_reply_no_allow_test() ->
    Msg = <<"OPTIONS sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 0"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 OPTIONS"
            ?crlf ?crlf
          >>,
    Options = #{validate => #{reply_on_options => true}},
    {reply, RespMsg} = request_validation(raw_message(Msg), Options),
    ?assertEqual(200, ersip_sipmsg:status(RespMsg)),
    Reason = ersip_sipmsg:reason(RespMsg),
    ?assertEqual(<<"OK">>, Reason),
    RespRawMsg = raw_message(ersip_sipmsg:serialize_bin(RespMsg)),
    ?assertEqual(ersip_hdr:new(<<"Allow">>), ersip_msg:get(<<"Allow">>, RespRawMsg)),
    ok.


request_validation_proxy_require_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Proxy-Require: gin"
            ?crlf ?crlf
          >>,
    {reply, RespMsg} = request_validation(raw_message(Msg)),
    ?assertEqual(420, ersip_sipmsg:status(RespMsg)),
    Reason = ersip_sipmsg:reason(RespMsg),
    ?assertEqual(<<"Bad Extension">>, Reason),
    RespRawMsg = raw_message(ersip_sipmsg:serialize_bin(RespMsg)),
    {ok, RespSipMsg} = ersip_sipmsg:parse(RespRawMsg, [unsupported]),
    Unsupported = ersip_hdr_opttag_list:from_list([ersip_option_tag:make(<<"gin">>)]),
    ?assertEqual(Unsupported, ersip_sipmsg:get(unsupported, RespSipMsg)),
    ok.


request_validation_proxy_require_no_required_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Proxy-Require: gin"
            ?crlf ?crlf
          >>,
    SupportedList = [<<"100rel">>, <<"timers">>],
    Supported =
        ersip_hdr_opttag_list:from_list(
          [ersip_option_tag:make(S) || S <- SupportedList]),
    Options = #{proxy_params => #{supported => Supported}},
    {reply, RespMsg} = request_validation(raw_message(Msg), Options),
    ?assertEqual(420, ersip_sipmsg:status(RespMsg)),
    Reason = ersip_sipmsg:reason(RespMsg),
    ?assertEqual(<<"Bad Extension">>, Reason),
    RespRawMsg = raw_message(ersip_sipmsg:serialize_bin(RespMsg)),
    {ok, RespSipMsg} = ersip_sipmsg:parse(RespRawMsg, [unsupported]),
    Unsupported = ersip_hdr_opttag_list:from_list([ersip_option_tag:make(<<"gin">>)]),
    ?assertEqual(Unsupported, ersip_sipmsg:get(unsupported, RespSipMsg)),
    ok.

request_validation_proxy_require_all_supported_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Proxy-Require: gin"
            ?crlf ?crlf
          >>,
    SupportedList = [<<"gin">>],
    Supported =
        ersip_hdr_opttag_list:from_list(
          [ersip_option_tag:make(S) || S <- SupportedList]),
    Options = #{proxy => #{supported => Supported}},
    {ok, _} = request_validation(raw_message(Msg), Options),
    ok.


request_validation_proxy_require_cannot_reply_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Proxy-Require: gin"
            ?crlf ?crlf
          >>,
    ?assertMatch({error, _}, request_validation(raw_message(Msg))),
    ok.

proxy_params_check_no_check_fun_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ThisProxyURI = ersip_uri:make(<<"sip:this.proxy.org">>),
    ?assertError({error, _},
                 process_route_info(
                   raw_message(Msg),
                   #{proxy =>
                         #{record_route_uri => ThisProxyURI
                          }})),
    ok.

proxy_params_check_no_validate_test() ->
    Msg = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ThisProxyURI = ersip_uri:make(<<"sip:this.proxy.org">>),
    %%% Check that invalid options are ignored if no_validate => true
    process_route_info(
      raw_message(Msg),
      #{proxy =>
            #{no_validate => true,
              record_route_uri => ThisProxyURI
             }}),
    ok.

process_route_info_strict_routing_test() ->
    %% Checking strict routing information update:
    %%
    %% If RURI is this proxy then it must be replaced by value from
    %% last Route, last Route must be removed from the message.
    BobURI = <<"sip:bob@biloxi.com">>,
    ThisProxyURI = <<"sip:this.proxy.org">>,
    NextProxyURI = <<"sip:next.proxy.org">>,
    %% Check strict-routing message recovery
    Msg = <<"INVITE ", ThisProxyURI/binary, " SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Route: <", NextProxyURI/binary, ">"
            ?crlf "Route: <", BobURI/binary, ">"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOptions =
        #{check_rroute_fun =>
              fun(URI) ->
                      ersip_uri:make(ThisProxyURI) == URI
              end},
    Options = #{proxy => ProxyOptions},
    SipMsg0 = process_route_info(raw_message(Msg), Options),
    SipMsg = rebuild_sipmsg(SipMsg0),
    ?assertEqual(ersip_uri:make(BobURI), ersip_sipmsg:ruri(SipMsg)),
    ExpectedRouteSet = ersip_hdr_route:make(<<"<", NextProxyURI/binary, ">">>),
    ?assertEqual(ExpectedRouteSet, ersip_sipmsg:get(route, SipMsg)),
    ok.

process_route_info_loose_routing_test() ->
    %% Checking loose routing information update:
    %%
    %% Need to remove this proxy from the route information
    BobURI = <<"sip:bob@biloxi.com">>,
    ThisProxyURI = <<"sip:this.proxy.org">>,
    NextProxyURI = <<"sip:next.proxy.org">>,
    %% Check strict-routing message recovery
    Msg = <<"INVITE ", BobURI/binary, " SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Route: <", ThisProxyURI/binary, ">"
            ?crlf "Route: <", NextProxyURI/binary, ">"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOptions =
        #{check_rroute_fun =>
              fun(URI) ->
                      ersip_uri:make(ThisProxyURI) == URI
              end},
    Options = #{proxy => ProxyOptions},
    SipMsg0 = process_route_info(raw_message(Msg), Options),
    SipMsg = rebuild_sipmsg(SipMsg0),
    ?assertEqual(ersip_uri:make(BobURI), ersip_sipmsg:ruri(SipMsg)),
    ExpectedRouteSet = ersip_hdr_route:make(<<"<", NextProxyURI/binary, ">">>),
    ?assertEqual(ExpectedRouteSet, ersip_sipmsg:get(route, SipMsg)),
    ok.

process_route_info_no_rr_checker_test() ->
    %% If no record-route detector provided then message is passed
    %% without modifications
    BobURI = <<"sip:bob@biloxi.com">>,
    ThisProxyURI = <<"sip:this.proxy.org">>,
    NextProxyURI = <<"sip:next.proxy.org">>,
    %% Check strict-routing message recovery
    Msg = <<"INVITE ", BobURI/binary, " SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Route: <", ThisProxyURI/binary, ">"
            ?crlf "Route: <", NextProxyURI/binary, ">"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    SipMsg0 = process_route_info(raw_message(Msg), #{}),
    SipMsg = rebuild_sipmsg(SipMsg0),
    ?assertEqual(ersip_uri:make(BobURI), ersip_sipmsg:ruri(SipMsg)),
    ExpectedRouteSet = ersip_hdr_route:make(<<"<", ThisProxyURI/binary, ">, <", NextProxyURI/binary, ">">>),
    ?assertEqual(ExpectedRouteSet, ersip_sipmsg:get(route, SipMsg)),
    ok.

process_route_info_no_routes_loose_route_test() ->
    %% If no route headers - do not do anything
    BobURI = <<"sip:bob@biloxi.com">>,
    %% Check strict-routing message recovery
    Msg = <<"INVITE ", BobURI/binary, " SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOptions =
        #{check_rroute_fun =>
              fun(_) ->
                      false
              end},
    Options = #{proxy => ProxyOptions},
    SipMsg0 = process_route_info(raw_message(Msg), Options),
    SipMsg = rebuild_sipmsg(SipMsg0),
    ?assertEqual(ersip_uri:make(BobURI), ersip_sipmsg:ruri(SipMsg)),
    ?assertEqual(not_found, ersip_sipmsg:find(route, SipMsg)),
    ok.

process_route_info_no_routes_strict_route_test() ->
    %% If no route headers - do not do anything
    BobURI = <<"sip:bob@biloxi.com">>,
    %% Check strict-routing message recovery
    Msg = <<"INVITE ", BobURI/binary, " SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,

    ProxyOptions =
        #{check_rroute_fun =>
              fun(_) ->
                      true
              end},
    Options = #{proxy => ProxyOptions},
    SipMsg0 = process_route_info(raw_message(Msg), Options),
    SipMsg = rebuild_sipmsg(SipMsg0),
    ?assertEqual(ersip_uri:make(BobURI), ersip_sipmsg:ruri(SipMsg)),
    ?assertEqual(not_found, ersip_sipmsg:find(route, SipMsg)),
    ok.

forward_request_set_ruri_test() ->
    %% Check that forward_request sets request URI to target and
    %% decrements max-forwards
    BobURI = <<"sip:bob@biloxi.com">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOpts = #{},
    {SipMsg0, Opts} = forward_request(BobURI, raw_message(Msg), ProxyOpts),
    ExpectedNexthop = ersip_uri:make(BobURI),
    ?assertMatch(#{nexthop := ExpectedNexthop}, Opts),
    SipMsg = rebuild_sipmsg(SipMsg0),
    ?assertEqual(69, ersip_hdr_maxforwards:value(ersip_sipmsg:get(maxforwards, SipMsg))),
    ?assertEqual(ersip_uri:make(BobURI), ersip_sipmsg:ruri(SipMsg)),
    ok.

forward_request_add_maxforwars_test() ->
    %% Check that proxy adds Max-From to the request
    BobURI = <<"sip:bob@biloxi.com">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOpts = #{},
    {SipMsg0, _} = forward_request(BobURI, raw_message(Msg), ProxyOpts),
    SipMsg = rebuild_sipmsg(SipMsg0),
    ?assertEqual(70, ersip_hdr_maxforwards:value(ersip_sipmsg:get(maxforwards, SipMsg))),
    ok.

forward_request_invalid_maxforwars_test() ->
    %% Check that proxy adds Max-From to the request
    BobURI = <<"sip:bob@biloxi.com">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Max-Forwards: x"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOpts = #{},
    RawMsg = raw_message(Msg),
    {ok, SipMsg} = ersip_sipmsg:parse(RawMsg, []),
    Target = ersip_uri:make(BobURI),
    ?assertError({error, {invalid_maxforwards, _}},
                 ersip_proxy_common:forward_request(Target, SipMsg, ProxyOpts)).

forward_request_record_route_add_test() ->
    %% Check that proxy adds Max-From to the request
    BobURI = <<"sip:bob@biloxi.com">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ThisProxyURI = ersip_uri:make(<<"sip:this.proxy.org">>),
    ProxyOpts = #{record_route_uri => ThisProxyURI,
                  check_rroute_fun => fun(X) -> ThisProxyURI == X end
                 },
    {SipMsg0, _} = forward_request(BobURI, raw_message(Msg), ProxyOpts),
    SipMsg = rebuild_sipmsg(SipMsg0),
    RecordRouteSet = ersip_sipmsg:get(record_route, SipMsg),
    RecordRouteURI = ersip_hdr_route:uri(ersip_route_set:first(RecordRouteSet)),
    ?assertEqual(ersip_uri:set_param(lr, true, ThisProxyURI), RecordRouteURI),
    ok.

forward_request_record_route_append_test() ->
    %% Check that proxy adds Max-From to the request
    BobURI = <<"sip:bob@biloxi.com">>,
    AnotherProxyURI = <<"sip:another.proxy.org;lr">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Record-Route: <", AnotherProxyURI/binary, ">"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ThisProxyURI = ersip_uri:make(<<"sip:this.proxy.org">>),
    ProxyOpts = #{record_route_uri => ThisProxyURI,
                  check_rroute_fun => fun(X) -> ThisProxyURI == X end
                 },
    {SipMsg0, _} = forward_request(BobURI, raw_message(Msg), ProxyOpts),
    SipMsg = rebuild_sipmsg(SipMsg0),
    RecordRouteSet = ersip_sipmsg:get(record_route, SipMsg),
    RecordRouteURI = ersip_hdr_route:uri(ersip_route_set:first(RecordRouteSet)),
    RecordRouteURI2 = ersip_hdr_route:uri(ersip_route_set:last(RecordRouteSet)),
    ?assertEqual(ersip_uri:set_param(lr, true, ThisProxyURI), RecordRouteURI),
    ?assertEqual(ersip_uri:make(AnotherProxyURI), RecordRouteURI2),
    ok.

forward_request_record_route_append_sips_test() ->
    %% If the Request-URI contains a SIPS URI, or the topmost Route
    %% header field value (after the post processing of bullet 6)
    %% contains a SIPS URI, the URI placed into the Record-Route
    %% header field MUST be a SIPS URI.
    BobURI = <<"sips:bob@biloxi.com">>,
    AnotherProxyURI = <<"sip:another.proxy.org;lr">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Record-Route: <", AnotherProxyURI/binary, ">"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ThisProxyURI = ersip_uri:make(<<"sips:this.proxy.org">>),
    ProxyOpts = #{record_route_uri => ThisProxyURI,
                  check_rroute_fun => fun(X) -> X == ThisProxyURI end
                 },
    _ = forward_request(BobURI, raw_message(Msg), ProxyOpts),
    ok.

forward_request_record_route_append_not_sips_test() ->
    %% If the Request-URI contains a SIPS URI, or the topmost Route
    %% header field value (after the post processing of bullet 6)
    %% contains a SIPS URI, the URI placed into the Record-Route
    %% header field MUST be a SIPS URI.
    BobURI = <<"sips:bob@biloxi.com">>,
    AnotherProxyURI = <<"sip:another.proxy.org;lr">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Record-Route: <", AnotherProxyURI/binary, ">"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ThisProxyURI = ersip_uri:make(<<"sip:this.proxy.org">>),
    ProxyOpts = #{record_route_uri => ThisProxyURI,
                  check_rroute_fun => fun(X) -> X == ThisProxyURI end
                 },
    ?assertError({error, _}, forward_request(BobURI, raw_message(Msg), ProxyOpts)),
    ok.

forward_request_no_rr_sip_to_sips_test() ->
    %% If the Request-URI contains a SIPS URI, or the topmost Route
    %% header field value (after the post processing of bullet 6)
    %% contains a SIPS URI, the URI placed into the Record-Route
    %% header field MUST be a SIPS URI.
    BobURI = <<"sips:bob@biloxi.com">>,
    AnotherProxyURI = <<"sip:another.proxy.org;lr">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Record-Route: <", AnotherProxyURI/binary, ">"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOpts = #{},
    RawMsg0 = raw_message(Msg),
    RawMsg1 = ersip_msg:set_source(udp_source(), RawMsg0),
    ?assertError({error, _}, forward_request(BobURI, RawMsg1, ProxyOpts)),
    ok.

forward_request_no_rr_sips_to_sip_test() ->
    %% If the Request-URI contains a SIPS URI, or the topmost Route
    %% header field value (after the post processing of bullet 6)
    %% contains a SIPS URI, the URI placed into the Record-Route
    %% header field MUST be a SIPS URI.
    BobURI = <<"sip:bob@biloxi.com">>,
    AnotherProxyURI = <<"sip:another.proxy.org;lr">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Record-Route: <", AnotherProxyURI/binary, ">"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOpts = #{},
    RawMsg0 = raw_message(Msg),
    RawMsg1 = ersip_msg:set_source(tls_source(), RawMsg0),
    ?assertError({error, _}, forward_request(BobURI, RawMsg1, ProxyOpts)),
    ok.

forward_request_no_rr_sips_to_sip_by_route_test() ->
    %% Check invariant:
    %% Either Record-route is added or nexthop cannot be sip.
    BobURI = <<"sips:bob@biloxi.com">>,
    AnotherProxyURI = <<"sip:another.proxy.org;lr">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Route: <", AnotherProxyURI/binary, ">"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOpts = #{},
    RawMsg0 = raw_message(Msg),
    RawMsg1 = ersip_msg:set_source(tls_source(), RawMsg0),
    ?assertError({error, _}, forward_request(BobURI, RawMsg1, ProxyOpts)),
    ok.

forward_request_no_rr_sips_to_sips_test() ->
    BobURI = <<"sips:bob@biloxi.com">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOpts = #{},
    RawMsg0 = raw_message(Msg),
    RawMsg1 = ersip_msg:set_source(tls_source(), RawMsg0),
    %% Check no error here
    {SipMsg0, _} = forward_request(BobURI, RawMsg1, ProxyOpts),
    SipMsg = rebuild_sipmsg(SipMsg0),
    ?assertEqual(not_found, ersip_sipmsg:find(record_route, SipMsg)),
    ok.

forward_request_to_strict_router_test() ->
    BobURI = <<"sip:bob@biloxi.com">>,
    StrictRouterURI = <<"sip:stict.proxy.org">>,
    Msg = <<"INVITE sip:bobby-online@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Route: <", StrictRouterURI/binary, ">"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOpts = #{},
    %% Check no error here
    {SipMsg0, Opts} = forward_request(BobURI, raw_message(Msg), ProxyOpts),
    ExpectedNexthop = ersip_uri:make(StrictRouterURI),
    ?assertMatch(#{nexthop := ExpectedNexthop}, Opts),
    SipMsg = rebuild_sipmsg(SipMsg0),
    %% Check strict routing requirements:
    %% RURI is set tp StrictRouterURI
    ?assertEqual(ersip_uri:make(StrictRouterURI), ersip_sipmsg:ruri(SipMsg)),
    %% Last route is set to BobURI
    RouteSet = ersip_sipmsg:get(route, SipMsg),
    LastRoute = ersip_route_set:last(RouteSet),
    ?assertEqual(ersip_uri:make(BobURI), ersip_hdr_route:uri(LastRoute)),
    ok.

forward_request_to_loose_router_test() ->
    BobURI = <<"sip:bob@biloxi.com">>,
    LooseRouterURI = <<"sip:stict.proxy.org;lr">>,
    Msg = <<"INVITE ", BobURI/binary, " SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Route: <", LooseRouterURI/binary, ">"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,
    ProxyOpts = #{},
    %% Check no error here
    {SipMsg0, Opts} = forward_request(BobURI, raw_message(Msg), ProxyOpts),
    ExpectedNexthop = ersip_uri:make(LooseRouterURI),
    ?assertMatch(#{nexthop := ExpectedNexthop}, Opts),
    SipMsg = rebuild_sipmsg(SipMsg0),
    %% Check strict routing requirements:
    %% RURI is set tp StrictRouterURI
    ?assertEqual(ersip_uri:make(BobURI), ersip_sipmsg:ruri(SipMsg)),
    ok.


%%%===================================================================

raw_message(Bin) ->
    P  = ersip_parser:new_dgram(Bin),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg.

request_validation(RawMsg, Options) ->
    ersip_proxy_common:request_validation(RawMsg, Options).

request_validation(RawMsg) ->
    request_validation(RawMsg, #{}).

process_route_info(RawMsg, Options) ->
    {ok, SipMsg} = request_validation(RawMsg, Options),
    ersip_proxy_common:process_route_info(SipMsg, maps:get(proxy, Options, #{})).

forward_request(Target, RawMsg, Opts) when is_binary(Target) ->
    forward_request(ersip_uri:make(Target), RawMsg, Opts);
forward_request(Target, RawMsg, Opts) ->
    {ok, SipMsg} = request_validation(RawMsg, Opts),
    ersip_proxy_common:forward_request(Target, SipMsg, Opts).

rebuild_sipmsg(SipMsg) ->
    SipMsgBin = ersip_sipmsg:serialize_bin(SipMsg),
    P  = ersip_parser:new_dgram(SipMsgBin),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    {ok, SipMsg1} = ersip_sipmsg:parse(PMsg, all),
    SipMsg1.


peer() ->
    {{127, 0, 0, 1}, 5060}.

udp_source() ->
    Transport = ersip_transport:make(udp),
    ersip_source:new(peer(), Transport, undefined).

tls_source() ->
    Transport = ersip_transport:make(tls),
    ersip_source:new(peer(), Transport, undefined).
