%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Expires and Min-Expires headers
%%%

-module(ersip_hdr_expires).

-export([make/1,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).

-export_type([expires/0]).

%%===================================================================
%% Types
%%===================================================================

-type expires() :: {expires, non_neg_integer()}.
-type parse_result() :: {ok, expires()} | {error, parse_errors()}.
-type parse_errors() :: {invalid_expires, empty_field}
                      | {invalid_expires, binary()}.
-type raw() :: non_neg_integer().

%%===================================================================
%% API
%%===================================================================

%% @doc Create Expires/Min-Expires header from raw representation or
%% from binary.
-spec make(binary() | raw() | non_neg_integer()) -> expires().
make(Bin) when is_binary(Bin) ->
    case parse_expires(Bin) of
        {ok, Expires} ->
            Expires;
        {error, Reason} ->
            error(Reason)
    end;
make(Val) when is_integer(Val), Val >= 0 ->
    {expires, Val}.

%% @doc Parse Expires header.
-spec parse(binary() | ersip_hdr:header()) -> parse_result().
parse(Bin) when is_binary(Bin) ->
    parse_expires(Bin);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, {invalid_expires, empty_field}};
        [ExpiresIOList]  ->
            parse_expires(iolist_to_binary(ExpiresIOList))
    end.

%% @doc Build SIP raw header.
-spec build(binary(), expires()) -> ersip_hdr:header().
build(HdrName, V) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([assemble(V)], Hdr).

%% @doc Assemble header value to iolist().
%% For this header it is equivalent to assemble_bin/2
-spec assemble(expires()) -> binary().
assemble({expires, Val}) ->
    integer_to_binary(Val).

%% @doc Assemble header value to binary().
-spec assemble_bin(expires()) -> binary().
assemble_bin({expires, Val}) ->
    integer_to_binary(Val).

%% @doc Raw representation of the header.
-spec raw(expires()) -> raw().
raw({expires, Val}) ->
    Val.

%%===================================================================
%% Internal implementation
%%===================================================================

%% @private
%% @doc
%% The value of this field is an integral number of seconds (in
%% decimal) between 0 and (2**32)-1, measured from the receipt of the
%% request.
-spec parse_expires(binary()) -> parse_result().
parse_expires(Binary) ->
    case ersip_parser_aux:parse_non_neg_int(Binary) of
        {ok, Int, <<>>} when Int =< 16#FFFFFFFF ->
            {ok, {expires, Int}};
        _ ->
            {error, {invalid_expires, Binary}}
    end.
