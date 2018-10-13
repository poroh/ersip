%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Header Params
%% Related to From/To/Contact/Route/Record-Route etc. headers.
%%
%% Idea of this module is provide common routines to work with header
%% params:
%%  1. Parsing
%%  2. Assembling (keeping order and case of original parameters).
%%  3. Modification
%%

-module(ersip_hparams).

-export([new/0,
         parse_raw/1,
         assemble/1,
         assemble_bin/1,
         find/2,
         find_raw/2,
         set_raw/3,
         set/5
        ]).
-export_type([hparams/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(hparams, {order  = []  :: [lower_key()],
                  orig   = #{} :: #{lower_key()   => {orig_key(),  orig_value()}},
                  parsed = #{} :: #{parsed_name() => {lower_key(), parsed_value()},
                                    lower_key()   => parsed_name()
                                   }
                 }).
-type hparams()      :: #hparams{}.
-type lower_key()    :: binary().
-type orig_key()     :: binary().
-type parsed_name()  :: atom().
-type parsed_value() :: term().
-type orig_value()   :: binary() | novalue.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> hparams().
new() ->
    #hparams{}.

-spec parse_raw(binary()) -> ersip_parser_aux:parse_result(hparams()).
parse_raw(Binary) ->
    case ersip_parser_aux:parse_params($;, Binary) of
        {error, _} = Error -> Error;
        {ok, Result, Rest} ->
            {ok, fill_hparams(Result), Rest}
    end.

-spec assemble(hparams()) -> iolist().
assemble(#hparams{} = HParams) ->
    KV = [begin
              {Key, Value} = maps:get(LowerKey, HParams#hparams.orig),
              case Value of
                  <<>> ->
                      Key;
                  _ ->
                      [Key, <<"=">>, Value]
              end
          end || LowerKey <- HParams#hparams.order],
    ersip_iolist:join($;, KV).

-spec assemble_bin(hparams()) -> binary().
assemble_bin(#hparams{} = HParams) ->
    iolist_to_binary(assemble(HParams)).

-spec find(parsed_name(), hparams()) -> {ok, parsed_value()} | not_found.
find(ParsedName, #hparams{parsed = P}) when is_atom(ParsedName) ->
    case maps:find(ParsedName, P) of
        error        -> not_found;
        {ok, {_, V}} -> {ok, V}
    end;
find(Name, #hparams{parsed = P} = HP) when is_binary(Name) ->
    LowerKey = ersip_bin:to_lower(Name),
    case maps:find(LowerKey, P) of
        error -> not_found;
        {ok, ParsedName} when is_atom(ParsedName) ->
            find(ParsedName, HP)
    end.

-spec set(parsed_name(), parsed_value(), orig_key(), orig_value(), hparams()) -> hparams().
set(ParsedName, ParsedValue, Key, Value, #hparams{parsed = PMap} = HParams) ->
    case maps:find(ParsedName, PMap) of
        error ->
            LowerKey = ersip_bin:to_lower(Key),
            Order  = HParams#hparams.order ++ [LowerKey],
            Orig   = (HParams#hparams.orig)#{LowerKey => {Key, Value}},
            Parsed = PMap#{ParsedName => {LowerKey, ParsedValue},
                           LowerKey => ParsedName},
            HParams#hparams{order = Order,
                            orig  = Orig,
                            parsed = Parsed};
        {ok, {LowerKey, _}} ->
            Orig   = (HParams#hparams.orig)#{LowerKey => {Key, Value}},
            Parsed = PMap#{ParsedName => {LowerKey, ParsedValue}},
            HParams#hparams{orig = Orig, parsed = Parsed}
    end.

-spec find_raw(orig_key(), hparams()) -> {ok, orig_value()} | not_found.
find_raw(BinName, #hparams{orig = Orig}) when is_binary(BinName) ->
    case maps:find(ersip_bin:to_lower(BinName), Orig) of
        error -> not_found;
        {ok, {_, V}} -> {ok, V}
    end;
find_raw(ParsedName, #hparams{parsed = P, orig = Orig}) when is_atom(ParsedName) ->
    case maps:find(ParsedName, P) of
        error -> not_found;
        {ok, {LowerKey, _}} ->
            {ok, {_, V}} = maps:find(LowerKey, Orig),
            {ok, V}
    end.

%% Set raw value
-spec set_raw(orig_key(), orig_value(), hparams()) -> hparams().
set_raw(Key, Value, #hparams{} = HParams) ->
    LowerKey = ersip_bin:to_lower(Key),
    case maps:find(LowerKey, HParams#hparams.orig) of
        error ->
            Order  = HParams#hparams.order ++ [LowerKey],
            Orig   = (HParams#hparams.orig)#{LowerKey => {Key, Value}},
            HParams#hparams{order = Order, orig  = Orig};
        {ok, _} ->
            Orig   = (HParams#hparams.orig)#{LowerKey => {Key, Value}},
            HParams#hparams{orig  = Orig}
    end.


%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-spec fill_hparams([{orig_key(), orig_value()}]) -> hparams().
fill_hparams(Pairs) ->
    lists:foldl(fun({Key, Value}, HParams) ->
                        set_raw(Key, Value, HParams)
                end,
                new(),
                Pairs).


