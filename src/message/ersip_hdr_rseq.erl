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
         build/2]).

-export_type([rseq/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type rseq() :: {rseq, non_neg_integer()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(ersip_hdr:header() | binary() | non_neg_integer()) -> rseq().
make(Number) when is_integer(Number), Number >= 0 ->
    {rseq, Number};
make(Bin) when is_binary(Bin) ->
    case parse_rseq(Bin) of
        {ok, RSeq} ->
            RSeq;
        Error ->
            error(Error)
    end;
make(Header) ->
    case parse(Header) of
        {ok, RSeq} ->
            RSeq;
        Error ->
            error(Error)
    end.

-spec value(rseq()) -> non_neg_integer().
value({rseq, V}) ->
    V.

-spec inc(rseq()) -> rseq().
inc({rseq, X}) when X >= 16#FFFFFFFF ->
    error({error, negative_rseq});
inc({rseq, V}) ->
    {rseq, V+1}.

-spec parse(ersip_hdr:header()) -> Result when
      Result :: {ok, rseq()}
              | {error, Error},
      Error :: no_rseq
             | {invalid_rseq, binary()}.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_rseq};
        [RSeqIOList]  ->
            parse_rseq(iolist_to_binary(RSeqIOList))
    end.

-spec assemble(rseq()) -> iolist().
assemble({rseq, V}) ->
    [integer_to_binary(V)].

-spec assemble_bin(rseq()) -> binary().
assemble_bin({rseq, _} = RSeq) ->
    iolist_to_binary(assemble(RSeq)).

-spec build(HdrName, rseq()) -> ersip_hdr:header() when
      HdrName :: binary().
build(HdrName, {rseq, V}) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([integer_to_binary(V)], Hdr).

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_rseq(binary()) -> Result when
      Result :: {ok, rseq()}
              | {error, Error},
      Error  :: {invalid_rseq, binary()}.
parse_rseq(Binary) ->
    case ersip_parser_aux:parse_non_neg_int(Binary) of
        {ok, Int, <<>>} ->
            {ok, {rseq, Int}};
        _ ->
            {error, {invalid_rseq, Binary}}
    end.


