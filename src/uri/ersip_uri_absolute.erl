%%%
%%% Copyright (c) 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Absolute URI
%%%

-module(ersip_uri_absolute).


-export([parse_data/1,
         assemble/1]).

-export_type([absolute_uri/0]).

%%===================================================================
%% Types
%%===================================================================

-record(absolute_uri, {opaque :: binary()}).
-type absolute_uri() :: #absolute_uri{}.

%%===================================================================
%% API
%%===================================================================

% @private
-spec parse_data(binary()) -> absolute_uri().
parse_data(Bin) ->
    #absolute_uri{opaque = Bin}.

% @private
-spec assemble(absolute_uri()) -> binary().
assemble(#absolute_uri{opaque = Bin}) ->
    Bin.
