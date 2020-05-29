%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Max-Forwards header
%%%

-module(ersip_hdr_maxforwards).
-include("ersip_sip_abnf.hrl").

-export([make/1,
         value/1,
         dec/1,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1,
         raw/1]).

-export_type([maxforwards/0, raw/0]).


%%===================================================================
%% Types
%%===================================================================

-type maxforwards() :: {maxforwards, non_neg_integer()}.
-type raw() :: non_neg_integer().
-type parse_result() :: {ok, maxforwards()} | {error, parse_error()}.
-type parse_error() :: no_maxforwards
                     | {invalid_maxforwards, binary()}.

%%===================================================================
%% API
%%===================================================================

%% @doc Make From/To field from binary(), raw representation or SIP
%% raw header.
-spec make(ersip_hdr:header() | binary() | raw()) -> maxforwards().
make(Number) when is_integer(Number), Number >= 0 ->
    {maxforwards, Number};
make(Bin) when is_binary(Bin) ->
    case parse_maxforwards(Bin) of
        {ok, MaxForwards} ->
            MaxForwards;
        Error ->
            error(Error)
    end;
make(Header) ->
    case parse(Header) of
        {ok, MaxForwards} ->
            MaxForwards;
        Error ->
            error(Error)
    end.

%% @doc Integer value in Max-Forwards header.
-spec value(maxforwards()) -> non_neg_integer().
value({maxforwards, V}) ->
    V.

%% @doc Decrement value in Max-Forwards header.
%% Function raises error if Max-Forwards is 0.
-spec dec(maxforwards()) -> maxforwards().
dec({maxforwards, X}) when X =< 0 ->
    error({error, negative_maxforwards});
dec({maxforwards, V}) ->
    {maxforwards, V-1}.

%% @doc Parse Max-Forwards from binary() of SIP raw header.
-spec parse(binary() | ersip_hdr:header()) -> parse_result().
parse(Bin) when is_binary(Bin) ->
    parse_maxforwards(Bin);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_maxforwards};
        [MaxForwardsIOList]  ->
            parse_maxforwards(iolist_to_binary(MaxForwardsIOList))
    end.

%% @doc Build SIP raw header.
-spec build(binary(), maxforwards()) -> ersip_hdr:header().
build(HdrName, {maxforwards, V}) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([integer_to_binary(V)], Hdr).

%% @doc Serialize Max-Forwards header value to iolist().
-spec assemble(maxforwards()) -> binary().
assemble({maxforwards, V}) ->
    integer_to_binary(V).

%% @doc Serialize Max-Forwards header value to binary().
-spec assemble_bin(maxforwards()) -> binary().
assemble_bin({maxforwards, V}) ->
    integer_to_binary(V).

%% @doc Raw representation of Max-Forwards.
-spec raw(maxforwards()) -> raw().
raw(MaxForwards) ->
    value(MaxForwards).

%%===================================================================
%% Internal implementation
%%===================================================================

%% @private
-spec parse_maxforwards(binary()) -> parse_result().
parse_maxforwards(Binary) ->
    case ersip_parser_aux:parse_non_neg_int(Binary) of
        {ok, Int, <<>>} ->
            {ok, {maxforwards, Int}};
        _ ->
            {error, {invalid_maxforwards, Binary}}
    end.
