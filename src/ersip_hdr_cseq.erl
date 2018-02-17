%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP CSeq header
%%

-module(ersip_hdr_cseq).

-export([ make/2,
          number/1,
          method/1,
          parse/1 ]).
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

-spec make(ersip_method:method(), cseq_num()) -> cseq().
make(Method, Number) ->
    #cseq{ method = Method,
           number = Number }.

-spec number(cseq()) -> cseq_num().
number(#cseq{ number = N }) ->
    N.

-spec method(cseq()) -> ersip_method:method().
method(#cseq{ method = M }) ->
    M.

-spec parse(ersip_hdr:header()) -> Result when
      Result :: { ok, cseq() }
              | { error, term() }.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            { error, no_cseq };
        [ CSeqIOList ]  ->
            parse_cseq(iolist_to_binary(CSeqIOList))
    end.

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
            { ok, make(Method, Number) };
        { ok, _, _ } ->
            { error, { invalid_cseq, Binary } };
        { error, _ } = Error ->
            Error
    end.
