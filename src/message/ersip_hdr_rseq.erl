%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP RSeq Header (RFC3262)
%%%

-module(ersip_hdr_rseq).


-export([make/1,
         value/1,
         inc/1,
         parse/1,
         assemble/1,
         assemble_bin/1,
         build/2,
         raw/1
        ]).

-export_type([rseq/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-type rseq() :: {rseq, non_neg_integer()}.
-type raw() :: non_neg_integer().
-type parse_result() :: {ok, rseq()} | {error, parse_error()}.
-type parse_error() :: no_rseq
                     | {invalid_rseq, binary()}.

%%===================================================================
%% API
%%===================================================================

%% @doc Make RSeq from binary() or raw SIP header and raw representation.
-spec make(ersip_hdr:header() | binary() | non_neg_integer()) -> rseq().
make(Number) when is_integer(Number), Number >= 0 ->
    {rseq, Number};
make(Bin) when is_binary(Bin) ->
    case parse_rseq(Bin) of
        {ok, RSeq} -> RSeq;
        {error, Reason} -> error(Reason)
    end;
make(Header) ->
    case parse(Header) of
        {ok, RSeq} -> RSeq;
        {error, Reason} -> error(Reason)
    end.

%% @doc Numberic value of the rseq.
-spec value(rseq()) -> non_neg_integer().
value({rseq, V}) ->
    V.

%% @doc Increment numberic value of the rseq.
-spec inc(rseq()) -> rseq().
inc({rseq, X}) when X >= 16#FFFFFFFF ->
    error(negative_rseq);
inc({rseq, V}) ->
    {rseq, V+1}.

%% @doc Parse RSeq from binary or raw SIP header representation.
-spec parse(ersip_hdr:header() | binary()) -> parse_result().
parse(HeaderBin) when is_binary(HeaderBin) ->
    parse_rseq(HeaderBin);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_rseq};
        [RSeqIOList]  ->
            parse_rseq(iolist_to_binary(RSeqIOList))
    end.

%% @doc Assemble RSeq to iolist()
-spec assemble(rseq()) -> iolist().
assemble({rseq, V}) ->
    [integer_to_binary(V)].

%% @doc Assemble RSeq to binary()
-spec assemble_bin(rseq()) -> binary().
assemble_bin({rseq, _} = RSeq) ->
    iolist_to_binary(assemble(RSeq)).

%% @doc Build raw SIP header.
-spec build(binary(), rseq()) -> ersip_hdr:header().
build(HdrName, {rseq, V}) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([integer_to_binary(V)], Hdr).

%% @doc Raw representation of the RSeq.
-spec raw(rseq()) -> raw().
raw({rseq, V}) ->
    V.

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_rseq(binary()) -> parse_result().
parse_rseq(Binary) ->
    case ersip_parser_aux:parse_non_neg_int(Binary) of
        {ok, Int, <<>>} ->
            {ok, {rseq, Int}};
        _ ->
            {error, {invalid_rseq, Binary}}
    end.
