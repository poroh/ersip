%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP CSeq header
%%

-module(ersip_hdr_cseq).

-export([ make/1,
          make/2,
          make_key/1,
          number/1,
          method/1,
          parse/1,
          build/2,
          assemble/1
        ]).
-export_type([ cseq/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(cseq, { method :: ersip_method:method(),
                number :: cseq_num()
              }).
-type cseq_num() :: non_neg_integer().
-type cseq() :: #cseq{}.


%%%===================================================================
%%% API
%%%===================================================================

-spec make(ersip_hdr:header()) -> cseq().
make(Header) ->
    case parse(Header) of
        { ok, CSeq } ->
            CSeq;
        Error ->
            error(Error)
    end.

-spec make(ersip_method:method(), cseq_num()) -> cseq().
make(Method, Number) ->
    #cseq{ method = Method,
           number = Number }.

-spec make_key(cseq()) -> cseq().
make_key(CSeq) ->
    CSeq.

-spec number(cseq()) -> cseq_num().
number(#cseq{ number = N }) ->
    N.

-spec method(cseq()) -> ersip_method:method().
method(#cseq{ method = M }) ->
    M.

-spec parse(ersip_hdr:header()) -> Result when
      Result :: { ok, cseq() }
              | { error, Error },
      Error :: no_cseq
             | multiple_cseqs
             | { invalid_cseq, binary() }.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            { error, no_cseq };
        [ CSeqIOList ]  ->
            parse_cseq(iolist_to_binary(CSeqIOList));
        _ ->
            { error, multiple_cseqs }
    end.

-spec build(HeaderName :: binary(), cseq()) -> ersip_hdr:header().
build(HdrName, #cseq{} = CSeq) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(CSeq), Hdr).

-spec assemble(cseq()) -> iolist().
assemble(#cseq{ method = Method, number = Num }) ->
    [ integer_to_binary(Num),
      <<" ">>,
      ersip_method:to_binary(Method)
    ].

%%%===================================================================
%%% Internal implementation
%%%===================================================================

parse_cseq(Binary) ->
    Parsers = [ fun ersip_parser_aux:parse_non_neg_int/1,
                fun ersip_parser_aux:parse_lws/1,
                fun ersip_parser_aux:parse_token/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        { ok, [ Number, _, Method ], <<>> } ->
            { ok, make(ersip_method:make(Method), Number) };
        { ok, _, _ } ->
            { error, { invalid_cseq, Binary } };
        { error, _ } = Error ->
            Error
    end.
