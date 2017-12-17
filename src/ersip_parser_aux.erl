%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Auxiliatry parsing functions
%%

-module(ersip_parser_aux).

-export([ quoted_string/1 ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Parse quoted string (with unquoute).
%% DQUOTE *(qdtext / quoted-pair ) DQUOTE
%% qdtext         =  LWS / %x21 / %x23-5B / %x5D-7E
%%                        / UTF8-NONASCII
-spec quoted_string(Quoted) -> { ok, Quoted, Rest } | error when
      Quoted :: binary(),
      Quoted :: binary(),
      Rest   :: binary().
quoted_string(Quoted) ->
    Trimmed = ersip_bin:trim_head_lws(Quoted),
    TrimmedLen = byte_size(Trimmed),
    case quoted_string_impl(Trimmed, start) of
        { ok, Rest } ->
            Len = TrimmedLen - byte_size(Rest),
            { ok, binary:part(Trimmed, 0, Len), Rest };
        error ->
            error
    end.
        
%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec quoted_string_impl(binary(), State) -> { ok, Rest :: binary() } | error when
      State :: start
             | raw
             | escaped.

quoted_string_impl(<<>>, _) ->
    error;
quoted_string_impl(<<"\"", R/binary>>, start) ->
    quoted_string_impl(R, raw);
quoted_string_impl(_, start) ->
    error;
quoted_string_impl(<<"\"", R/binary>>, raw) ->
    { ok, R };
quoted_string_impl(<<"\\", R/binary>>, raw) ->
    quoted_string_impl(R, escaped);
quoted_string_impl(B, raw) ->
    case utf8_len(B) of
        { ok, Len } ->
            RestLen = byte_size(B) - Len,
            quoted_string_impl(binary:part(B, Len, RestLen), raw);
        error ->
            error
    end;
quoted_string_impl(<<Byte:8, R/binary>>, escaped) when Byte =< 16#7F ->
    quoted_string_impl(R, raw).

utf8_len(<<ASCII:8, _/binary>>) when ASCII =< 16#7F ->
    { ok, 1 };
utf8_len(<<UTF8_1:8, _:8, _/binary>>) 
  when UTF8_1 >= 16#C0 andalso UTF8_1 =< 16#DF  ->
    { ok, 2 };
utf8_len(<<UTF8_2:8, _:16, _/binary>>) 
  when UTF8_2 >= 16#E0 andalso UTF8_2 =< 16#EF ->
    { ok, 3 };
utf8_len(<<UTF8_3:8, _:24, _/binary>>) 
  when UTF8_3 >= 16#F0 andalso UTF8_3 =< 16#F7 ->
    { ok, 4 };
utf8_len(<<UTF8_4:8, _:32, _/binary>>) 
  when UTF8_4 >= 16#F8 andalso UTF8_4 =< 16#FB ->
    { ok, 5 };
utf8_len(<<UTF8_5:8, _:40, _/binary>>) 
  when UTF8_5 >= 16#FC andalso UTF8_5 =< 16#FD ->
    { ok, 6 };
utf8_len(_) ->
    error.

