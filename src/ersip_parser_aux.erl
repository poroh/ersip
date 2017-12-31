%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Auxiliatry parsing functions
%%

-module(ersip_parser_aux).
-include("ersip_sip_abnf.hrl").

-export([ quoted_string/1,
          token_list/2,
          check_token/1
        ]).

-type separator() :: lws.

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

%% @doc Parse token list separated with SEP
-spec token_list(binary(), SEP) -> { ok, [ Token, ... ], Rest } | error when
      SEP    :: separator(),
      Token  :: binary(),
      Rest   :: binary().
token_list(Binary, SEP) ->
    CompiledPattern = compile_pattern(SEP), 
    token_list_impl(Binary, [], CompiledPattern).

%% @doc Check binary is token
%% token       =  1*(alphanum / "-" / "." / "!" / "%" / "*"
%%                   / "_" / "+" / "`" / "'" / "~" )
-spec check_token(binary()) -> boolean().
check_token(Bin) ->
    check_token(Bin, start).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% @private
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

%% @private
%% @doc get length of the first UTF8 character in binary
-spec utf8_len(binary()) -> { ok, Len :: 1..6 } | error.
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

%% @private
-spec token_list_impl(binary(), [binary()], binary:cp()) -> Result when
      Result :: { ok, [ Token, ... ], Rest }
              | error,
      Token  :: binary(),
      Rest   :: binary().
token_list_impl(Binary, Acc, CompPattern) ->
    case binary:split(Binary, CompPattern) of
        [ <<>>, Rest ] ->
            token_list_impl(Rest, Acc, CompPattern); 
        [ T, Rest ] ->
            case check_token(T) of
                true ->
                    token_list_impl(Rest, [ T | Acc ], CompPattern);
                false ->
                    case Acc of
                        [] ->
                            error;
                        _ ->
                            { ok, lists:reverse(Acc), Binary }
                    end
            end;
        [ T ] ->
            case check_token(T) of
                true ->
                    { ok, lists:reverse([ T | Acc ]), <<>> };
                false ->
                    case Acc of
                        [] ->
                            error;
                        _ ->
                            { ok, lists:reverse(Acc), Binary }
                    end
            end
    end.

%% @private
-spec check_token(binary(), start | rest) -> boolean().
check_token(<<>>, start) ->
    false;
check_token(<<>>, rest) ->
    true;
check_token(<<Char/utf8, R/binary>>, _) when ?is_token_char(Char) ->
    check_token(R, rest);
check_token(_, _) ->
    false.

%% @private
-spec compile_pattern(separator()) -> binary:cp(). 
compile_pattern(lws) ->
    binary:compile_pattern([ <<" ">>, <<"\t">> ]). 
