%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% iolist tests
%%

-module(ersip_hdr_test).

-include_lib("eunit/include/eunit.hrl").

as_integer_test() ->
    H0 = ersip_hdr:new(<<"Content-Length">>),
    H1 = ersip_hdr:add_value([ <<"1">>, <<"0">> ], H0),
    ?assertEqual({ok, 10}, ersip_hdr:as_integer(H1)).

serialize_headers_test() ->
    ClH0 = ersip_hdr:new(<<"Content-Length">>),
    ClH1 = ersip_hdr:add_value([ <<"1">>, <<"0">> ], ClH0),
    ?assertEqual(<<"Content-Length: 10">>, serialize_to_bin(ClH1)),

    Via0 = ersip_hdr:new(<<"Via">>),
    Via1 = ersip_hdr:add_values(
             [ <<"SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1">>,
               <<"SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>
             ],
             Via0),
    ?assertEqual(<<"Via: SIP/2.0/UDP bigbox3.site3.atlanta.com;branch=z9hG4bK77ef4c2312983.1\r\n"
                   "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds">>,
                 serialize_to_bin(Via1)).

serialize_to_bin(Header) ->
    iolist_to_binary(lists:reverse(ersip_hdr:serialize_rev_iolist(Header, []))).
