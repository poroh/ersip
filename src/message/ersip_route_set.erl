%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Route Set
%%

-module(ersip_route_set).

-export([new/0,
         from_list/1,
         is_empty/1,
         add_first/2,
         add_last/2,
         remove_first/1,
         remove_last/1,
         reverse/1,
         first/1,
         last/1,
         foldl/3
        ]).

-export_type([route_set/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type route() :: ersip_hdr_route:route().
-type route_set() :: {route_set, [route()]}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> route_set().
new() ->
    {route_set, []}.

-spec from_list(list(route())) -> route_set().
from_list(Routes) when is_list(Routes) ->
    {route_set, Routes}.

-spec is_empty(route_set()) -> boolean().
is_empty({route_set, []}) ->
    true;
is_empty({route_set, _}) ->
    false.

-spec add_first(route(), route_set()) -> route_set().
add_first(Route, {route_set, RR}) ->
    {route_set, [Route | RR]}.

-spec add_last(route(), route_set()) -> route_set().
add_last(Route, {route_set, RR}) ->
    {route_set, RR ++ [Route]}.

-spec remove_first(route_set()) -> route_set().
remove_first({route_set, []}) ->
    error({error, route_set_empty});
remove_first({route_set, [_ | RR]}) ->
    {route_set, RR}.

-spec remove_last(route_set()) -> route_set().
remove_last({route_set, []}) ->
    error({error, route_set_empty});
remove_last({route_set, RR}) ->
    {route_set, lists:droplast(RR)}.

-spec reverse(route_set()) -> route_set().
reverse({route_set, RR}) ->
    {route_set, lists:reverse(RR)}.

-spec first(route_set()) -> route().
first({route_set, []}) ->
    error({error, route_set_empty});
first({route_set, [R | _]}) ->
    R.

-spec last(route_set()) -> route().
last({route_set, []}) ->
    error({error, route_set_empty});
last({route_set, L}) ->
    lists:last(L).

-spec foldl(Fun, any(), route_set()) -> any() when
      Fun :: fun((route(), any()) -> any()).
foldl(Fun, Init, {route_set, R}) ->
    lists:foldl(Fun, Init, R).
