%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP authorization info tests
%%

-module(ersip_authinfo_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

rebuild_test() ->
    rebuild(<<"Digest realm=\"testrealm@host.com\", qop=\"auth,auth-int\",",
              " nonce=\"dcd98b7102dd2f0e8b11d0f600bfb0c093\", opaque=\"5ccc069c403ebaf9f0171e9517f40e41\"">>),
    rebuild(<<"Kerberos realm=\"Contoso RTC Service Provider\", targetname=\"sip/hs1.contoso.com\", qop=\"auth\"">>),
    rebuild(<<"NTLM realm=\"Contoso RTC Service Provider\", targetname=\"hs1.contoso.com\", qop=\"auth\"">>),
    rebuild(<<"Kerberos realm=\"Contoso RTC Service Provider\", targetname=\"sip/hs1.contoso.com\",",
              " qop=\"auth\", opaque=\"ACDC123\", srand=\"3453453\", snum=1, rspauth=\"23423acfdee2\"">>),
    rebuild(<<"Myauth a=b, c=d">>),
    ok.

parse_error_test() ->
    parse_error(<<"">>),
    parse_error(<<"Digest">>),
    parse_error(<<"Digest ">>),
    parse_error(<<"Digest $=1">>),
    parse_error(<<"Digest x=y a=b">>),
    ok.

make_error_test() ->
    ?assertError({parse_error, _}, ersip_authinfo:make(<<"Digest">>)),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

rebuild(Bin) ->
    R = ersip_authinfo:assemble_bin(ersip_authinfo:make(Bin)),
    ?assertEqual(Bin, R).

parse_error(Bin) ->
    ?assertMatch({error, _}, ersip_authinfo:parse(Bin)).
