%%
%% Copyright (c) 2018, 2019 Dmitry Poroh
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
         parse_known/2,
         assemble/1,
         assemble_bin/1,
         is_empty/1,
         to_list/1,
         to_raw_list/1,
         find/2,
         find_raw/2,
         get/2,
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

-type parse_known_fun() :: fun((lower_key(), orig_value()) -> parse_known_fun_result()).
-type parse_known_fun_result() :: {ok, {parsed_name(), parsed_value()}}
                                | {ok, unknown}
                                | {error, term()}.

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

%% @doc Enrich parameters with parsed values.  ParseKnownFun should
%% return result of the parameter parsing (see type definition).
-spec parse_known(parse_known_fun(), hparams()) -> {ok, hparams()} | {error, term()}.
parse_known(ParseKnownFun, #hparams{order = Order, orig = Orig} = HParams0) ->
    lists:foldl(fun(_LowerKey, {error, _} = Error) ->
                        Error;
                   (LowerKey, {ok, HParams}) ->
                        {_, OrigValue} = maps:get(LowerKey, Orig),
                        case ParseKnownFun(LowerKey, OrigValue) of
                            {ok, unknown} -> {ok, HParams};
                            {ok, {ParsedName, ParsedValue}} ->
                                {ok, set_parsed(LowerKey, ParsedName, ParsedValue, HParams)};
                            {error, _} = Error ->
                                Error
                        end
                end,
                {ok, HParams0},
                Order).

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

-spec is_empty(hparams()) -> boolean().
is_empty(#hparams{order = []}) ->
    true;
is_empty(#hparams{}) ->
    false.

-spec to_list(hparams()) -> Result when
      Result :: [Item],
      Item   :: {parsed_name(), parsed_value()}
              | {lower_key(),   orig_value()}.
to_list(#hparams{} = HParams) ->
    [begin
         case maps:find(LowerKey, HParams#hparams.parsed) of
             {ok, ParsedName} ->
                 {LowerKey, ParsedValue} = maps:get(ParsedName, HParams#hparams.parsed),
                 {ParsedName, ParsedValue};
             error ->
                 {_, Value} = maps:get(LowerKey, HParams#hparams.orig),
                 {LowerKey, Value}
         end
     end || LowerKey <- HParams#hparams.order].

-spec to_raw_list(hparams()) -> [{binary(), binary()} | binary()].
to_raw_list(#hparams{} = HParams) ->
    [begin
         {Key, Value} = maps:get(LowerKey, HParams#hparams.orig),
         case Value of
             <<>> ->
                 Key;
             _ ->
                 {Key, Value}
         end
     end || LowerKey <- HParams#hparams.order].


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

-spec get(parsed_name() | binary(), hparams()) -> parsed_value().
get(Name, #hparams{} =H) ->
    case find(Name, H) of
        not_found ->
            error({no_param, Name});
        {ok, V} -> V
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

-spec set_parsed(lower_key(), parsed_name(), parsed_value(), hparams()) -> hparams().
set_parsed(LowerKey, ParsedName, ParsedValue, #hparams{parsed = PMap0} = HParams) ->
    PMap = PMap0#{ParsedName => {LowerKey, ParsedValue},
                  LowerKey   => ParsedName},
    HParams#hparams{parsed = PMap}.

