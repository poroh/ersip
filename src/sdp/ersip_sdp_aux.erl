%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SDP auxiliary functions
%%

-module(ersip_sdp_aux).

-export([check_token/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec check_token(binary()) -> boolean().
check_token(<<>>) ->
    false;
check_token(Bin) ->
    lists:all(fun is_token_char/1,
              binary_to_list(Bin)).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec is_token_char(char()) -> boolean().
is_token_char(C) when C >= $a, C =< $z ->
    true; %% shortcut for most common token chars...
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
