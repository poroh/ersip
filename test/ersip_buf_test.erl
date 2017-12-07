%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP message buffer test
%%

-module(ersip_buf_test).

-include_lib("eunit/include/eunit.hrl").

read_till_crlf_test() ->
    Buf  = ersip_buf:new(#{}),
    Buf1 = ersip_buf:add(<<"a", $\r, $\n, "b">>, Buf),
    { ok, <<"a">>, Buf2 } = ersip_buf:read_till_crlf(Buf1),
    { more_data, Buf3 }   = ersip_buf:read_till_crlf(Buf2),
    Buf4 = ersip_buf:add(<<$\r>>, Buf3),
    { more_data, Buf5 }   = ersip_buf:read_till_crlf(Buf4),
    Buf6 = ersip_buf:add(<<$\n>>, Buf5),
    { ok, <<"b">>, _Buf7 } = ersip_buf:read_till_crlf(Buf6).

read_till_crlf_2_test() ->
    Buf  = ersip_buf:new(#{}),
    Buf1 = ersip_buf:add(<<"aaa", $\r,$\n,
                           "bbb", $\r,$\n
                         >>, Buf),
    lists:foldl(fun(S, Buf_) ->
                        { ok, S, B } = ersip_buf:read_till_crlf(Buf_),
                        B
                end,
                Buf1,
                [ <<"aaa">>, <<"bbb">> ]).
