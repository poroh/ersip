%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Max-Forwards header
%%

-module(ersip_hdr_maxforwards).
-include("ersip_sip_abnf.hrl").

-export([ make/1,
          value/1,
          dec/1,
          parse/1,
          build/2 ]).

-export_type([ maxforwards/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type maxforwards() :: { maxforwards, non_neg_integer() }.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(ersip_hdr:header() | binary()) -> maxforwards().
make(Number) when is_integer(Number), Number >= 0 ->
    { maxforwards, Number };
make(Bin) when is_binary(Bin) ->
    case parse_maxforwards(Bin) of
        { ok, MaxForwards } ->
            MaxForwards;
        Error ->
            error(Error)
    end;
make(Header) ->
    case parse(Header) of
        { ok, MaxForwards } ->
            MaxForwards;
        Error ->
            error(Error)
    end.

-spec value(maxforwards()) -> non_neg_integer().
value({ maxforwards, V }) ->
    V.

-spec dec(maxforwards()) -> maxforwards().
dec({ maxforwards, X }) when X =< 0 ->
    error({ error, negative_maxforwards });
dec({ maxforwards, V }) ->
    { maxforwards, V-1 }.

-spec parse(ersip_hdr:header()) -> Result when
      Result :: { ok, maxforwards() }
              | { error, Error },
      Error :: no_maxforwards
             | { invalid_maxforwards, binary() }.
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            { error, no_maxforwards };
        [ MaxForwardsIOList ]  ->
            parse_maxforwards(iolist_to_binary(MaxForwardsIOList));
        _ ->
            { error, multiple_maxforwards }
    end.

-spec build(HdrName, maxforwards()) -> ersip_hdr:header() when
      HdrName :: binary().
build(HdrName, { maxforwards, V }) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([ integer_to_binary(V) ], Hdr).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-spec parse_maxforwards(binary()) -> Result when
      Result :: { ok, maxforwards() }
              | { error, Error },
      Error  :: { invalid_maxforwards, binary() }.
parse_maxforwards(Binary) ->
    case ersip_parser_aux:parse_non_neg_int(Binary) of
        { ok, Int, <<>> } ->
            { ok, { maxforwards, Int } };
        _ ->
            { error, { invalid_maxforwards, Binary } }
    end.
