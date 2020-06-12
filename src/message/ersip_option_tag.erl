%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Option-Tag
%%% For example: 100rel, timers etc.
%%%

-module(ersip_option_tag).

-export([parse/1,
         make/1,
         to_binary/1
        ]).
-export_type([option_tag/0]).

%%===================================================================
%% Types
%%===================================================================

-type option_tag() :: {option_tag, binary()}.
-type parse_result() :: {ok, option_tag()} | {error, parse_error()}.
-type parse_error() :: {invalid_option_tag, binary()}.

%%===================================================================
%% API
%%===================================================================

-spec parse(binary()) -> parse_result().
parse(Bin) ->
    case ersip_parser_aux:check_token(Bin) of
        true ->  {ok, {option_tag, Bin}};
        false -> {error, {invalid_option_tag, Bin}}
    end.

-spec to_binary(option_tag()) -> binary().
to_binary({option_tag, Bin}) ->
    Bin.

-spec make(binary()) -> option_tag().
make(Bin) ->
    case parse(Bin) of
        {ok, M} -> M;
        {error, Reason} -> error(Reason)
    end.
