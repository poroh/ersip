%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP RAck Header (RFC3262)
%%%

-module(ersip_hdr_rack).


-export([make/1,
         make/2,
         rseq/1,
         cseq/1,
         parse/1,
         assemble/1,
         assemble_bin/1,
         build/2]).

-export_type([rack/0]).

%%===================================================================
%% Types
%%===================================================================

-record(rack, {rseq :: ersip_hdr_rseq:rseq(),
               cseq :: ersip_hdr_cseq:cseq()
              }).
-type rack() :: #rack{}.
-type parse_result() :: {ok, rack()} | {error, parse_error()}.
-type parse_error() :: {invalid_rack, term()}.

%%===================================================================
%% API
%%===================================================================

-spec make(ersip_hdr:header() | binary() | non_neg_integer()) -> rack().
make(Bin) when is_binary(Bin) ->
    case parse_rack(Bin) of
        {ok, RAck} -> RAck;
        {error, _} = Error -> error(Error)
    end;
make(Header) ->
    case parse(Header) of
        {ok, RAck} -> RAck;
        {error, _} = Error -> error(Error)
    end.

-spec make(ersip_hdr_rseq:rseq(), ersip_hdr_cseq:cseq()) -> rack().
make(RSeq, CSeq) ->
    #rack{rseq = RSeq,
          cseq = CSeq}.

-spec rseq(rack()) -> ersip_hdr_rseq:rseq().
rseq(#rack{rseq = RSeq}) ->
    RSeq.

-spec cseq(rack()) -> ersip_hdr_cseq:cseq().
cseq(#rack{cseq = CSeq}) ->
    CSeq.

-spec parse(ersip_hdr:header()) -> parse_result().
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_rack};
        [RAckIOList]  ->
            parse_rack(iolist_to_binary(RAckIOList))
    end.

-spec assemble(rack()) -> iolist().
assemble(#rack{rseq = R, cseq = C}) ->
    [ersip_hdr_rseq:assemble(R), " ", ersip_hdr_cseq:assemble(C)].

-spec assemble_bin(rack()) -> binary().
assemble_bin(#rack{} = RAck) ->
    iolist_to_binary(assemble(RAck)).

-spec build(HdrName, rack()) -> ersip_hdr:header() when
      HdrName :: binary().
build(HdrName, #rack{} = RAck) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(RAck), Hdr).

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_rack(binary()) -> parse_result().
parse_rack(Binary) ->
    Parsers = [fun ersip_parser_aux:parse_non_neg_int/1,
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_parser_aux:parse_non_neg_int/1,
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_parser_aux:parse_token/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        {ok, [RSeqNumber, _, CSeqNumber, _, Method], <<>>}
          when RSeqNumber =< 16#FFFFFFFF, CSeqNumber =< 16#FFFFFFFF ->
            RSeq = ersip_hdr_rseq:make(RSeqNumber),
            CSeq = ersip_hdr_cseq:make(ersip_method:make(Method), CSeqNumber),
            {ok, make(RSeq, CSeq)};
        {ok, _, _} ->
            {error, {invalid_rack, Binary}};
        {error, Reason} ->
            {error, {invalid_rack, Reason}}
    end.



