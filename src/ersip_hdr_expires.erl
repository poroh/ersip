%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Expires and Min-Expires headers
%%

-module(ersip_hdr_expires).

-export([make/1,
         parse/1,
         build/2
        ]).

-export_type([expires/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type expires() :: {expires, non_neg_integer()}.
-type parse_result() :: {ok, expires()}
                      | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec make(binary()) -> expires().
make(Bin) when is_binary(Bin) ->
    case parse_expires(Bin) of
        {ok, Expires} ->
            Expires;
        {error, Reason} ->
            error(Reason)
    end.

-spec parse(ersip_hdr:header()) -> parse_result().
parse(Header) ->
    case ersip_hdr:raw_values(Header) of
        [] ->
            {error, {invalid_expires, empty_field}};
        [ExpiresIOList]  ->
            parse_expires(iolist_to_binary(ExpiresIOList));
        _ ->
            {error, {invalid_expires, multiple_fields}}
    end.

-spec build(HdrName, expires()) -> ersip_hdr:header() when
      HdrName :: binary().
build(HdrName, {expires, V}) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_hdr:add_value([integer_to_binary(V)], Hdr).


%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% The value of this field is an integral number of seconds (in
%% decimal) between 0 and (2**32)-1, measured from the receipt of the
%% request.
-spec parse_expires(binary()) -> parse_result().
parse_expires(Binary) ->
    case ersip_parser_aux:parse_non_neg_int(Binary) of
        {ok, Int, <<>>} when Int =< 16#FFFFFFFF ->
            {ok, {expires, Int}};
        _ ->
            {error, {invalid_expires, Binary}}
    end.
