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
         build/2,
         raw/1
        ]).

-export_type([rack/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-record(rack, {rseq :: ersip_hdr_rseq:rseq(),
               cseq :: ersip_hdr_cseq:cseq()
              }).
-type rack() :: #rack{}.
-type parse_result() :: {ok, rack()} | {error, parse_error()}.
-type parse_error() :: {invalid_rack, term()}.
-type raw() :: #{rseq => ersip_hdr_rseq:raw(),
                 cseq => ersip_hdr_cseq:raw()}.

%%===================================================================
%% API
%%===================================================================

%% @doc Make RAck from binary() or raw SIP header and raw
%% representation.
-spec make(ersip_hdr:header() | binary() | raw()) -> rack().
make(Bin) when is_binary(Bin) ->
    case parse_rack(Bin) of
        {ok, RAck} -> RAck;
        {error, _} = Error -> error(Error)
    end;
make(#{rseq := RSeqRaw, cseq := CSeqRaw}) ->
    #rack{rseq = ersip_hdr_rseq:make(RSeqRaw),
          cseq = ersip_hdr_cseq:make(CSeqRaw)
         };
make(Header) ->
    case parse(Header) of
        {ok, RAck} -> RAck;
        {error, _} = Error -> error(Error)
    end.

%% @doc Make RAck from RSeq and CSeq.
-spec make(ersip_hdr_rseq:rseq(), ersip_hdr_cseq:cseq()) -> rack().
make(RSeq, CSeq) ->
    #rack{rseq = RSeq,
          cseq = CSeq}.

%% @doc RSeq part of RAck
-spec rseq(rack()) -> ersip_hdr_rseq:rseq().
rseq(#rack{rseq = RSeq}) ->
    RSeq.

%% @doc CSeq part of RAck
-spec cseq(rack()) -> ersip_hdr_cseq:cseq().
cseq(#rack{cseq = CSeq}) ->
    CSeq.

%% @doc Parse RAck from binary or raw SIP header representation.
-spec parse(ersip_hdr:header()) -> parse_result().
parse(HeaderBin) when is_binary(HeaderBin) ->
    parse_rack(HeaderBin);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, no_rack};
        [RAckIOList]  ->
            parse_rack(iolist_to_binary(RAckIOList))
    end.

%% @doc Assemble RAck to iolist()
-spec assemble(rack()) -> iolist().
assemble(#rack{rseq = R, cseq = C}) ->
    [ersip_hdr_rseq:assemble(R), " ", ersip_hdr_cseq:assemble(C)].

%% @doc Assemble RAck to binary()
-spec assemble_bin(rack()) -> binary().
assemble_bin(#rack{} = RAck) ->
    iolist_to_binary(assemble(RAck)).

%% @doc Build raw SIP header.
-spec build(binary(), rack()) -> ersip_hdr:header().
build(HdrName, #rack{} = RAck) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(RAck), Hdr).

%% @doc Raw representation of the RAck.
-spec raw(rack()) -> raw().
raw(RAck) ->
    #{rseq => ersip_hdr_rseq:raw(rseq(RAck)),
      cseq => ersip_hdr_cseq:raw(cseq(RAck))}.

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
