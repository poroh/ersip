%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Absolute URI
%%%

-module(ersip_uri_absolute).


-export([scheme/1,
         parse/1,
         assemble/1]).

-export_type([absolute_uri/0,
              parse_error/0]).

%%===================================================================
%% Types
%%===================================================================

-record(absolute_uri, {scheme :: binary(),
                       opaque :: binary()}).
-type absolute_uri() :: #absolute_uri{}.
-type parse_result() :: {ok, absolute_uri()}
                      | {error, parse_error()}.
-type parse_error() :: {invalid_scheme, binary()}.

%%===================================================================
%% API
%%===================================================================

-spec scheme(absolute_uri()) -> ersip_uri:scheme().
scheme(#absolute_uri{scheme = Bin}) ->
    {scheme, Bin}.

% @private
-spec parse(binary()) -> parse_result().
parse(Bin) ->
    {SchemeBin, Value} = ersip_uri_parser_aux:split_scheme(Bin),
    case ersip_parser_aux:check_token(SchemeBin) of
        true ->
            {ok, #absolute_uri{scheme = SchemeBin, opaque = Value}};
        false ->
            {error, {invalid_scheme, SchemeBin}}
    end.

% @private
-spec assemble(absolute_uri()) -> iolist().
assemble(#absolute_uri{scheme = Scheme, opaque = Bin}) ->
    [Scheme, $:, Bin].

