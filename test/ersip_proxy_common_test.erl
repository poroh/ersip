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
    ?assertMatch({ ok, _ }, request_validation(raw_message(Msg))),
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
    ?assertMatch({ ok, _ }, request_validation(raw_message(Msg))),
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
    { reply, BadMsg } = request_validation(raw_message(Msg)),
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
    { reply, BadMsg } = request_validation(raw_message(Msg),
                                           #{ scheme_val_fun => fun(_) -> false end
                                            }),
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
    ?assertMatch({ error, _ },
                 request_validation(
                   raw_message(Msg),
                   #{ scheme_val_fun => fun(_) -> false end
                    })),
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
    ?assertMatch({ error, _ }, request_validation(raw_message(Msg))),
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
    ?assertMatch({ error, _ }, request_validation(raw_message(Msg))),
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
    { reply, BadMsg } = request_validation(raw_message(Msg)),
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
            ?crlf "CSeq: 314159 INVITE"
            ?crlf ?crlf
          >>,
    { reply, BadMsg } = request_validation(raw_message(Msg)),
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
            ?crlf "CSeq: 314159 INVITE"
            ?crlf ?crlf
          >>,
    AllowMethodsList = [ <<"INVITE">>,
                         <<"ACK">>,
                         <<"OPTIONS">>,
                         <<"CANCEL">>,
                         <<"BYE">>
                       ],
    SupportedList = [ <<"100rel">>, <<"timers">> ],
    AllowMethods =
        ersip_hdr_allow:from_list(
          [ ersip_method:make(M) || M <- AllowMethodsList ]),
    Supported =
        ersip_hdr_opttag_list:from_list(
          [ ersip_option_tag:make(S) || S <- SupportedList ]),
    Options = #{ reply_on_options => true,
                 proxy_params => #{
                   allow => AllowMethods,
                   supported => Supported
                  }
               },
    { reply, RespMsg } = request_validation(raw_message(Msg), Options),
    ?assertEqual(200, ersip_sipmsg:status(RespMsg)),
    Reason = ersip_sipmsg:reason(RespMsg),
    ?assertEqual(<<"OK">>, Reason),
    RespRawMsg = raw_message(ersip_sipmsg:serialize_bin(RespMsg)),
    { ok, RespSipMsg } = ersip_sipmsg:parse(RespRawMsg, [ allow, supported ]),
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
            ?crlf "CSeq: 314159 INVITE"
            ?crlf ?crlf
          >>,
    Options = #{ reply_on_options => true, proxy_params => #{} },
    { reply, RespMsg } = request_validation(raw_message(Msg), Options),
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
    { reply, RespMsg } = request_validation(raw_message(Msg)),
    ?assertEqual(420, ersip_sipmsg:status(RespMsg)),
    Reason = ersip_sipmsg:reason(RespMsg),
    ?assertEqual(<<"Bad Extension">>, Reason),
    RespRawMsg = raw_message(ersip_sipmsg:serialize_bin(RespMsg)),
    { ok, RespSipMsg } = ersip_sipmsg:parse(RespRawMsg, [ unsupported ]),
    Unsupported = ersip_hdr_opttag_list:from_list([ ersip_option_tag:make(<<"gin">>) ]),
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
    SupportedList = [ <<"100rel">>, <<"timers">> ],
    Supported =
        ersip_hdr_opttag_list:from_list(
          [ ersip_option_tag:make(S) || S <- SupportedList ]),
    Options = #{ proxy_params => #{ supported => Supported } },
    { reply, RespMsg } = request_validation(raw_message(Msg), Options),
    ?assertEqual(420, ersip_sipmsg:status(RespMsg)),
    Reason = ersip_sipmsg:reason(RespMsg),
    ?assertEqual(<<"Bad Extension">>, Reason),
    RespRawMsg = raw_message(ersip_sipmsg:serialize_bin(RespMsg)),
    { ok, RespSipMsg } = ersip_sipmsg:parse(RespRawMsg, [ unsupported ]),
    Unsupported = ersip_hdr_opttag_list:from_list([ ersip_option_tag:make(<<"gin">>) ]),
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
    SupportedList = [ <<"gin">> ],
    Supported =
        ersip_hdr_opttag_list:from_list(
          [ ersip_option_tag:make(S) || S <- SupportedList ]),
    Options = #{ proxy_params => #{ supported => Supported } },
    { ok, _ } = request_validation(raw_message(Msg), Options),
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
    ?assertMatch({ error, _ }, request_validation(raw_message(Msg))),
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
    ProxyOpts =
        #{ check_rroute_fun =>
               fun(URI) ->
                       ersip_uri:make(ThisProxyURI) == URI
               end
         },
    SipMsg0 = process_route_info(raw_message(Msg), ProxyOpts),
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
    ProxyOpts =
        #{ check_rroute_fun =>
               fun(URI) ->
                       ersip_uri:make(ThisProxyURI) == URI
               end
         },
    SipMsg0 = process_route_info(raw_message(Msg), ProxyOpts),
    SipMsg = rebuild_sipmsg(SipMsg0),
    ?assertEqual(ersip_uri:make(BobURI), ersip_sipmsg:ruri(SipMsg)),
    ExpectedRouteSet = ersip_hdr_route:make(<<"<", NextProxyURI/binary, ">">>),
    ?assertEqual(ExpectedRouteSet, ersip_sipmsg:get(route, SipMsg)),
    ok.

process_route_info_no_rrcecker_test() ->
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
    ProxyOpts = #{},
    SipMsg0 = process_route_info(raw_message(Msg), ProxyOpts),
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
    ProxyOpts =
        #{ check_rroute_fun =>
               fun(_URI) ->
                       false
               end
         },
    SipMsg0 = process_route_info(raw_message(Msg), ProxyOpts),
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
    ProxyOpts =
        #{ check_rroute_fun =>
               fun(_URI) ->
                       true
               end
         },
    SipMsg0 = process_route_info(raw_message(Msg), ProxyOpts),
    SipMsg = rebuild_sipmsg(SipMsg0),
    ?assertEqual(ersip_uri:make(BobURI), ersip_sipmsg:ruri(SipMsg)),
    ?assertEqual(not_found, ersip_sipmsg:find(route, SipMsg)),
    ok.

forward_request_set_ruri_test() ->
    %% Check that forward_request sets request URI to target and
    %% decrements max-forwards
    BobURI = <<"sip:bob@biloxi.com">>,
    %% Check strict-routing message recovery
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
    SipMsg0 = forward_request(BobURI, raw_message(Msg), ProxyOpts),
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
    SipMsg0 = forward_request(BobURI, raw_message(Msg), ProxyOpts),
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
    { ok, SipMsg } = ersip_sipmsg:parse(RawMsg, []),
    Target = ersip_uri:make(BobURI),
    ?assertError({ error, {invalid_maxforwards, _} },
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
    ProxyOpts = #{ record_route_uri => ThisProxyURI },
    SipMsg0 = forward_request(BobURI, raw_message(Msg), ProxyOpts),
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
    ProxyOpts = #{ record_route_uri => ThisProxyURI },
    SipMsg0 = forward_request(BobURI, raw_message(Msg), ProxyOpts),
    SipMsg = rebuild_sipmsg(SipMsg0),
    RecordRouteSet = ersip_sipmsg:get(record_route, SipMsg),
    RecordRouteURI = ersip_hdr_route:uri(ersip_route_set:first(RecordRouteSet)),
    RecordRouteURI2 = ersip_hdr_route:uri(ersip_route_set:last(RecordRouteSet)),
    ?assertEqual(ersip_uri:set_param(lr, true, ThisProxyURI), RecordRouteURI),
    ?assertEqual(ersip_uri:make(AnotherProxyURI), RecordRouteURI2),
    ok.

%%%===================================================================

raw_message(Bin) ->
    P  = ersip_parser:new_dgram(Bin),
    { {ok, PMsg}, _P2 } = ersip_parser:parse(P),
    PMsg.

request_validation(RawMsg, Opts) ->
    ersip_proxy_common:request_validation(RawMsg, Opts#{ to_tag => { tag, <<"12345">> } }).

request_validation(RawMsg) ->
    request_validation(RawMsg, #{}).

process_route_info(RawMsg, Opts) ->
    { ok, SipMsg } = request_validation(RawMsg, Opts),
    ersip_proxy_common:process_route_info(SipMsg, Opts).

forward_request(Target, RawMsg, Opts) when is_binary(Target) ->
    forward_request(ersip_uri:make(Target), RawMsg, Opts);
forward_request(Target, RawMsg, Opts) ->
    { ok, SipMsg } = request_validation(RawMsg, Opts),
    ersip_proxy_common:forward_request(Target, SipMsg, Opts).

rebuild_sipmsg(SipMsg) ->
    SipMsgBin = ersip_sipmsg:serialize_bin(SipMsg),
    P  = ersip_parser:new_dgram(SipMsgBin),
    { {ok, PMsg}, _P2 } = ersip_parser:parse(P),
    { ok, SipMsg1 } = ersip_sipmsg:parse(PMsg, all),
    SipMsg1.
