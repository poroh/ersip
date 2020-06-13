%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Route/Record-route headers
%%%

-module(ersip_hdr_route_list).

-export([make/1,
         parse/1,
         build/2]).

-include("ersip_headers.hrl").

%%===================================================================
%% Types
%%===================================================================

-type route_set() :: ersip_route_set:route_set().
-type maybe_rev_route_set() :: {ok, route_set()}
                             | {error, term()}.
-type parse_result() :: {ok, route_set()}
                      | {error, term()}.

%%===================================================================
%% API
%%===================================================================

-spec make(iolist()) -> route_set().
make(Binary) ->
    H0 = ersip_hdr:new(?ERSIPH_ROUTE),
    H1 = ersip_hdr:add_value(Binary, H0),
    case parse(H1) of
        {ok, RouteSet} ->
            RouteSet;
        {error, _} = Error  ->
            error(Error)
    end.

-spec parse(ersip_hdr:header()) -> parse_result().
parse(Header) ->
    MaybeRevRouteSet =
        lists:foldl(fun(IORoute, Acc) ->
                            add_to_maybe_route_set(iolist_to_binary(IORoute), Acc)
                    end,
                    {ok, ersip_route_set:new()},
                    ersip_hdr:raw_values(Header)),
    case MaybeRevRouteSet of
        {ok, RevRouteSet} ->
            {ok, ersip_route_set:reverse(RevRouteSet)};
        {error, Reason} ->
            {error, {invalid_route, Reason}}
    end.

-spec build(HeaderName :: binary(), route_set()) -> ersip_hdr:header().
build(HdrName, {route_set, _} = RouteSet) ->
    Hdr = ersip_hdr:new(HdrName),
    ersip_route_set:foldl(
      fun(Route, HdrAcc) ->
              ersip_hdr:add_value(ersip_hdr_route:assemble(Route), HdrAcc)
      end,
      Hdr,
      RouteSet).

%%===================================================================
%% Internal implementation
%%===================================================================

-spec add_to_maybe_route_set(binary(), maybe_rev_route_set()) -> maybe_rev_route_set().
add_to_maybe_route_set(_, {error, _} = Error) ->
    Error;
add_to_maybe_route_set(Bin, {ok, RouteSet}) ->
    case ersip_hdr_route:parse_hdr(Bin) of
        {ok, Route, <<>>} ->
            {ok, ersip_route_set:add_first(Route, RouteSet)};
        {ok, Route, <<$,, Rest/binary>>} ->
            MaybeRoute = {ok, ersip_route_set:add_first(Route, RouteSet)},
            add_to_maybe_route_set(ersip_bin:trim_head_lws(Rest), MaybeRoute);
        {ok, _, Rest} ->
            {error, {garbage_at_end, Rest}};
        {error, _} = Error ->
            Error
    end.
