%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message parser tests
%%

-module(ersip_hostname_test).

-include_lib("eunit/include/eunit.hrl").

hostname_parse_test() ->
    ?assertEqual({ ok, { ipv4, { 127, 0, 0, 1 } } }, ersip_hostname:parse(<<"127.0.0.1">>)),
    ?assertEqual({ ok, { ipv6, { 0, 0, 0, 0,   0, 0, 0, 1 } } }, ersip_hostname:parse(<<"[::1]">>)),
    ?assertEqual({ ok, { hostname, <<"example.com">> } }, ersip_hostname:parse(<<"example.com">>)),
    ?assertEqual({ ok, { hostname, <<"example.com.">> } }, ersip_hostname:parse(<<"example.com.">>)),
    ?assertEqual({ ok, { hostname, <<"exa-mple.com.">> } }, ersip_hostname:parse(<<"exa-mple.com.">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<"127..">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<"[]">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<"[:1:]">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<"[::1">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<"example..">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<>>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<".">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<".com.">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<"example.1.">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<"-example.com">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<"example.co-m">>)),
    ?assertEqual({ error, einval },  ersip_hostname:parse(<<"example-.com">>))
        .
    
    
