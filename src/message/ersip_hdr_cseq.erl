%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP CSeq header
%%%

-module(ersip_hdr_cseq).

-export([new/1,
         make/1,
         make/2,
         make_key/1,
         number/1,
         set_number/2,
         method/1,
         set_method/2,
         parse/1,
         build/2,
         assemble/1,
         assemble_bin/1,
         raw/1
        ]).

-export_type([cseq/0,
              cseq_num/0,
              raw/0
             ]).

%%===================================================================
%% Types
%%===================================================================

-record(cseq, {method :: ersip_method:method(),
               number :: cseq_num()
              }).
-type cseq_num() :: non_neg_integer().
-type cseq() :: #cseq{}.

-type parse_result() :: {ok, cseq()} | {error, parse_error()}.
-type parse_error() :: {invalid_cseq, binary()}
                     | {invalid_cseq, term()}
                     | no_cseq.
-type raw() :: {non_neg_integer(), binary()}.

%%===================================================================
%% API
%%===================================================================

%% @doc Create new CSeq header from defined method. CSeq number will
%% be set to 0.
-spec new(ersip_method:method()) -> cseq().
new(Method) ->
    %% The sequence number value MUST be expressible as a 32-bit
    %% unsigned integer and MUST be less than 2**31.  As long as it
    %% follows the above guidelines, a client may use any mechanism it
    %% would like to select CSeq header field values.
    #cseq{method = Method, number = 1}.

%% @doc Make CSeq header from binary, raw representation of CSeq of
%% from raw SIP header. If syntax is invalid then this function raises
%% error.
-spec make(ersip_hdr:header() | raw() | binary()) -> cseq().
make(Bin) when is_binary(Bin) ->
    case parse_cseq(Bin) of
        {ok, CSeq} -> CSeq;
        {error, Reason} -> error(Reason)
    end;
make({CSeq, Method}) ->
    make(CSeq, ersip_method:make(Method));
make(Header) ->
    case parse(Header) of
        {ok, CSeq} -> CSeq;
        {error, Reason} -> error(Reason)
    end.

%% @doc Make Cseq from parts: method and valid cseq number.
%% @deprecated
%%
%% This function has two version. make(Method, CSeq) and make(CSeq,
%% Method).  First one is left for backward compatibility and
%% deprected. Second one is more logical but we still consider it as
%% deprected.
-spec make(ersip_method:method() | cseq_num(), cseq_num() | ersip_method:method()) -> cseq().
make({method, _} = Method, Number) when is_integer(Number), Number >= 0 ->
    #cseq{method = Method, number = Number};
make(Number, {method, _} = Method) when is_integer(Number), Number >= 0 ->
    #cseq{method = Method, number = Number}.


%% @doc Make erlang-comparable header data structure.
-spec make_key(cseq()) -> cseq().
make_key(#cseq{} = CSeq) ->
    CSeq.

%% @doc CSeq number from header.
-spec number(cseq()) -> cseq_num().
number(#cseq{number = N}) ->
    N.

%% @doc Set CSeq number of header.
-spec set_number(cseq_num(), cseq()) -> cseq().
set_number(N, #cseq{} = CSeq) ->
    CSeq#cseq{number = N}.

%% @doc Method of header.
-spec method(cseq()) -> ersip_method:method().
method(#cseq{method = M}) ->
    M.

%% @doc Set Method of header.
-spec set_method(ersip_method:method(), cseq()) -> cseq().
set_method(Method, #cseq{} = CSeq) ->
    CSeq#cseq{method = Method}.

%% @doc Parse header from raw SIP header or from binary.
-spec parse(ersip_hdr:header()) -> parse_result().
parse(Binary) when is_binary(Binary) ->
    parse_cseq(Binary);
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] -> {error, no_cseq};
        [CSeqIOList]  ->
            parse_cseq(iolist_to_binary(CSeqIOList))
    end.

%% @doc Build raw SIP header from CSeq header.
-spec build(HeaderName :: binary(), cseq()) -> ersip_hdr:header().
build(HdrName, #cseq{} = CSeq) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value(assemble(CSeq), Hdr).

%% @doc Serialize to iolist.
-spec assemble(cseq()) -> [binary(), ... ].
assemble(#cseq{method = Method, number = Num}) ->
    [integer_to_binary(Num),
     <<" ">>,
     ersip_method:to_binary(Method)
    ].

%% @doc Serialize to binary.
-spec assemble_bin(cseq()) -> binary().
assemble_bin(#cseq{} = CSeq) ->
    iolist_to_binary(assemble(CSeq)).

%% @doc Raw representation in plain Erlang terms.
-spec raw(cseq()) -> raw().
raw(#cseq{method = M, number = N}) ->
    {N, ersip_method:to_binary(M)}.

%%===================================================================
%% Internal implementation
%%===================================================================

-spec parse_cseq(binary()) -> parse_result().
parse_cseq(Binary) ->
    Parsers = [fun ersip_parser_aux:parse_non_neg_int/1,
               fun ersip_parser_aux:parse_lws/1,
               fun ersip_parser_aux:parse_token/1
              ],
    case ersip_parser_aux:parse_all(Binary, Parsers) of
        {ok, [Number, _, Method], <<>>} ->
            {ok, make(ersip_method:make(Method), Number)};
        {ok, _, _} ->
            {error, {invalid_cseq, Binary}};
        {error, Reason} ->
            {error, {invalid_cseq, Reason}}
    end.
