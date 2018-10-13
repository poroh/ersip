%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% iolist tests
%%

-module(ersip_hdr_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

as_integer_test() ->
    H0 = ersip_hdr:new(<<"Content-Length">>),
    H1 = ersip_hdr:add_value([<<"1">>, <<"0">>], H0),
    ?assertEqual({ok, 10}, ersip_hdr:as_integer(H1)),
    %% For headers that are proteced from separation by ",":
    CommaProtectedHeader = ersip_hdr:new(<<"www-authenticate">>),
    CommaProtectedHeader1 = ersip_hdr:add_value([<<"1">>, <<"0">>], CommaProtectedHeader),
    ?assertEqual({ok, 10}, ersip_hdr:as_integer(CommaProtectedHeader1)).

make_key_test() ->
    HeaderName = <<"Content-Length">>,
    ?assertEqual(ersip_hdr:make_key(HeaderName),
                 ersip_hdr:make_key(ersip_hdr:new(HeaderName))).

serialize_headers_test() ->
    ClH0 = ersip_hdr:new(<<"Content-Length">>),
    ClH1 = ersip_hdr:add_value([<<"1">>, <<"0">>], ClH0),
    ?assertEqual(<<"Content-Length: 10">>, serialize_to_bin(ClH1)),

    Via0 = ersip_hdr:new(<<"Via">>),
    Via1 = ersip_hdr:add_values(
             [<<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1">>,
              <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>
             ],
             Via0),
    ?assertEqual(<<"Via: SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1\r\n"
                   "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>,
                 serialize_to_bin(Via1)).

comma_split_test() ->
    H0 = ersip_hdr:new(<<"route">>),
    H1 = ersip_hdr:add_values(
           [<<"<sip:alice@atlanta.com>">>,
            <<"<sip:bob@biloxi.com>">>,
            <<"<sip:carol@chicago.com>">>
           ],
           H0),
    H2 = ersip_hdr:add_values(
           [<<"<sip:alice@atlanta.com>, <sip:bob@biloxi.com>">>,
            <<"<sip:carol@chicago.com>">>
           ],
           H0),
    H3 = ersip_hdr:add_values(
           [<<"<sip:alice@atlanta.com>, <sip:bob@biloxi.com>, <sip:carol@chicago.com>">>
           ],
           H0),
    ?assertEqual(H1, H2),
    ?assertEqual(H1, H3).

allow_compact_test() ->
    H0 = ersip_hdr:new(<<"Allow">>),
    H1 = ersip_hdr:add_values(
           [<<"ACK">>,
            <<"INVITE">>,
            <<"OPTIONS">>
           ],
           H0),
    ?assertEqual(<<"Allow: ACK, INVITE, OPTIONS">>,
                 serialize_to_bin(H1)).

contact_test() ->
    H0 = ersip_hdr:new(<<"Contact">>),
    H1 = ersip_hdr:add_values(
           [<<"sip:a@b">>,
            <<"sip:a@c">>
           ],
           H0),
    ?assertEqual(<<"Contact: sip:a@b\r\n"
                   "Contact: sip:a@c">>,
                 serialize_to_bin(H1)).

allow_empty_test() ->
    H0 = ersip_hdr:new(<<"Allow">>),
    H1 = ersip_hdr:add_values(
           [],
           H0),
    ?assertEqual(<<"A: B">>, serialize_to_bin_append(H1, [<<"A: B">>])).

no_split_exceptions_test() ->
    %% The exceptions to this rule are the WWW-Authenticate,
    %% Authorization, Proxy-Authenticate, and Proxy-Authorization
    %% header fields.
    WWWAuthenticateVal = <<"Digest realm=\"atlanta.com\","
                           " domain=\"sip:boxesbybob.com\", qop=\"auth\","
                           " nonce=\"f84f1cec41e6cbe5aea9c8e88d359\","
                           " opaque=\"\", stale=FALSE, algorithm=MD5">>,
    check_no_comma_split(<<"WWW-Authenticate">>,  WWWAuthenticateVal),

    AuthorizationVal = <<"Digest username=\"Alice\", realm=\"atlanta.com\","
                         " nonce=\"84a4cc6f3082121f32b42a2187831a9e\","
                         " response=\"7587245234b3434cc3412213e5f113a5432\"">>,
    check_no_comma_split(<<"Authorization">>, AuthorizationVal),

    ProxyAuthenticateVal = <<"Digest realm=\"atlanta.com\","
                             " domain=\"sip:ss1.carrier.com\", qop=\"auth\","
                             " nonce=\"f84f1cec41e6cbe5aea9c8e88d359\","
                             " opaque=\"\", stale=FALSE, algorithm=MD5">>,
    check_no_comma_split(<<"Proxy-Authenticate">>, ProxyAuthenticateVal),

    ProxyAuthorizationVal = <<"Digest username=\"Alice\", realm=\"atlanta.com\","
                              " nonce=\"c60f3082ee1212b402a21831ae\","
                              " response=\"245f23415f11432b3434341c022\"">>,
    check_no_comma_split(<<"Proxy-Authorization">>, ProxyAuthorizationVal).

add_topmost_to_singleton_test() ->
    H0 = ersip_hdr:new(<<"To">>),
    ?assertError({api_error, _}, ersip_hdr:add_topmost([],H0)),
    ok.

get_name_test() ->
    H0 = ersip_hdr:new(<<"Allow">>),
    ?assertEqual(<<"Allow">>, ersip_hdr:name(H0)).

%%%===================================================================
%%% Helpers
%%%===================================================================

serialize_to_bin(Header) ->
    iolist_to_binary(lists:reverse(ersip_hdr:serialize_rev_iolist(Header, []))).

serialize_to_bin_append(Header, Src) ->
    iolist_to_binary(lists:reverse(ersip_hdr:serialize_rev_iolist(Header, Src))).

check_no_comma_split(HeaderName, Value) ->
    H0 = ersip_hdr:new(HeaderName),
    H1 = ersip_hdr:add_value(Value, H0),
    ?assertEqual([Value], ersip_hdr:raw_values(H1)).
