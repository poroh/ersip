%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Expires header tests
%%

-module(ersip_hdr_auth_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

rebuild_test() ->
    rebuild(<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\",",
              " nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", opaque=\"5ccc069c403ebaf9f0171e9517f40e41\"">>),
    rebuild(<<"Kerberos realm=\"Contoso RTC Service Provider\", targetname=\"sip/hs1.contoso.com\", qop=\"auth\"">>),
    rebuild(<<"NTLM realm=\"Contoso RTC Service Provider\", targetname=\"hs1.contoso.com\", qop=\"auth\"">>),
    rebuild(<<"Kerberos realm=\"Contoso RTC Service Provider\", targetname=\"sip/hs1.contoso.com\",",
              " qop=\"auth\", opaque=\"ACDC123\", srand=\"3453453\", snum=1, rspauth=\"23423acfdee2\"">>),
    rebuild(<<"Myauth a=b, c=d">>),
    ok.

-define(crlf, "\r\n").

parse_msg_test() ->
    Msg = << "SIP/2.0 407 Proxy Authentication Required" ?crlf
             "Via: SIP/2.0/TLS Alice1.contoso.com;branch=z9hG4bKa" ?crlf
             "From: \"Alice\" <sip:Alice@contoso.com>;tag=354354535;epid=6534555" ?crlf
             "To: \"Alice\" <sip:Alice@contoso.com>;tag=8823488" ?crlf
             "Call-ID: 123213@Alice1.contoso.com" ?crlf
             "CSeq: 12347 REGISTER" ?crlf
             "Date: Sat, 13 Nov 2010 23:29:00 GMT" ?crlf
             "Proxy-Authenticate: Kerberos realm=\"Contoso RTC Service Provider\"," ?crlf
             "  targetname=\"sip/hs2.contoso.com\", qop=\"auth\"" ?crlf
             "Proxy-Authenticate: NTLM realm=\"Contoso RTC Service Provider\", " ?crlf
             "  targetname=\"hs2.contoso.com\", qop=\"auth\"" ?crlf
             "Content-Length: 0" ?crlf
             ?crlf>>,
    ParseResult = ersip_sipmsg:parse(Msg, [proxy_authenticate]),
    ?assertMatch({ok, _}, ParseResult),
    {ok, SipMsg} = ParseResult,
    Auths = ersip_sipmsg:get(proxy_authenticate, SipMsg),
    ?assertMatch([_, _], Auths),
    [KerberosAuth, NTLMAuth] = Auths,
    ?assertEqual(<<"kerberos">>, ersip_authinfo:type(KerberosAuth)),
    ?assertEqual(<<"ntlm">>, ersip_authinfo:type(NTLMAuth)),
    ok.

parse_error_test() ->
    Msg = << "SIP/2.0 407 Proxy Authentication Required" ?crlf
             "Via: SIP/2.0/TLS Alice1.contoso.com;branch=z9hG4bKa" ?crlf
             "From: \"Alice\" <sip:Alice@contoso.com>;tag=354354535;epid=6534555" ?crlf
             "To: \"Alice\" <sip:Alice@contoso.com>;tag=8823488" ?crlf
             "Call-ID: 123213@Alice1.contoso.com" ?crlf
             "CSeq: 12347 REGISTER" ?crlf
             "Date: Sat, 13 Nov 2010 23:29:00 GMT" ?crlf
             "Proxy-Authenticate: Digest username=\"123\" realm=\"biloxy.com\"," ?crlf
             "  nonce=\"WbujGVm7oe3OGpu65V3JPPNinEV2qZtE\", uri=\"sip:bilox" ?crlf
             "Proxy-Authenticate: NTLM realm=\"Contoso RTC Service Provider\", " ?crlf
             "  targetname=\"hs2.contoso.com\", qop=\"auth\"" ?crlf
             "Content-Length: 0" ?crlf
             ?crlf>>,
    ParseResult = ersip_sipmsg:parse(Msg, [proxy_authenticate]),
    ?assertMatch({error, _}, ParseResult),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

rebuild(Bin) ->
    Auth = ersip_authinfo:make(Bin),
    {ok, [Auth1]} = ersip_hdr_auth:parse(ersip_hdr_auth:build(<<"WWW-Authenticate">>, [Auth])),
    ?assertEqual(Auth, Auth1).
