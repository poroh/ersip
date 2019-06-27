%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP auxiliary functions
%%

-module(ersip_sdp_aux).

-export([check_token/1,
         parse_token/1,
         parse_info/1,
         assemble_info/1,
         parse_key/1,
         assemble_key/1,
         parse_crlf/1,
         binary_to_eol/2,
         unexpected_attribute_error/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type parse_result(X) :: ersip_parser_aux:parse_result(X).
-type maybe_binary() :: binary() | undefined.

%%%===================================================================
%%% API
%%%===================================================================

-spec check_token(binary()) -> boolean().
check_token(<<>>) ->
    false;
check_token(Bin) ->
    lists:all(fun is_token_char/1,
              binary_to_list(Bin)).

-spec parse_token(binary()) -> ersip_parser_aux:parse_result(binary()).
parse_token(<<>>) ->
    {error, token_expected};
parse_token(<<C, _/binary>> = Bin) ->
    case is_token_char(C) of
        true ->
            TokenEnd = find_token_end(Bin, 0),
            <<Token:TokenEnd/binary, Rest/binary>> = Bin,
            {ok, Token, Rest};
        false ->
            {error, token_expected}
    end.

%% information-field =   [%x69 "=" text CRLF]
-spec parse_info(binary()) -> parse_result(maybe_binary()).
parse_info(<<"i=", Rest/binary>>) ->
    binary_to_eol(info, Rest);
parse_info(Bin) ->
    {ok, undefined, Bin}.

-spec assemble_info(maybe_binary()) -> iolist().
assemble_info(undefined) ->
    [];
assemble_info(Info) ->
    [<<"i=">>, Info, <<"\r\n">>].

%% key-field =           [%x6b "=" key-type CRLF]
-spec parse_key(binary()) -> parse_result(maybe_binary()).
parse_key(<<"k=", Rest/binary>>) ->
    case ersip_sdp_aux:binary_to_eol(key, Rest) of
        {ok, _, _} = Ok -> Ok;
        {error, Reason} ->
            {error, {invalid_key, Reason}}
    end;
parse_key(Bin) ->
    {ok, undefined, Bin}.

-spec assemble_key(maybe_binary()) -> iolist().
assemble_key(undefined) ->
    [];
assemble_key(Key) ->
    [<<"k=">>, Key, <<"\r\n">>].

-spec parse_crlf(binary()) -> parse_result(true).
parse_crlf(<<"\r\n", Rest/binary>>) ->
    {ok, true, Rest};
parse_crlf(_) ->
    {error, crlf_expected}.

-spec binary_to_eol(atom(), binary()) -> parse_result(binary()).
binary_to_eol(Type, Bin) ->
    case binary:split(Bin, <<"\r\n">>) of
        [V, Rest1] ->
            {ok, V, Rest1};
        [V] ->
            {error, {unexpected_end, {Type, V}}}
    end.

-spec unexpected_attribute_error(atom(), binary()) -> {error, term()}.
unexpected_attribute_error(Expected, Bin) ->
    [V | _] = binary:split(Bin, <<"\r\n">>),
    {error, {unexpected_attribute_error, {Expected, V}}}.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec is_token_char(char()) -> boolean().
is_token_char(C) when C >= $a, C =< $z ->
    true;
is_token_char(C) when C >= $A, C =< $Z ->
    true;
is_token_char(C) ->
    is_token_char_full(C).

%%  token-char =          %x21 / %x23-27 / %x2A-2B / %x2D-2E / %x30-39
%%                        / %x41-5A / %x5E-7E
-spec is_token_char_full(char()) -> boolean().
is_token_char_full(16#22) -> false; %% '"'
is_token_char_full(16#28) -> false; %% '('
is_token_char_full(16#29) -> false; %% ')'
is_token_char_full(16#2C) -> false; %% ','
is_token_char_full(16#2F) -> false; %% '/'
is_token_char_full(16#40) -> false; %% '@'
is_token_char_full(16#5B) -> false; %% '['
is_token_char_full(16#5C) -> false; %% '\'
is_token_char_full(16#5D) -> false; %% ']'
is_token_char_full(C) when C < 16#21 -> false;
is_token_char_full(C) when C > 16#7E -> false;
is_token_char_full(_) -> true.

-spec find_token_end(binary(), non_neg_integer()) -> non_neg_integer().
find_token_end(<<>>, Acc) ->
    Acc;
find_token_end(<<C:8, Rest/binary>>, Acc) ->
    case is_token_char(C) of
        false ->
            Acc;
        true ->
            find_token_end(Rest, Acc+1)
    end.
