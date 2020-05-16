%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP method set
%%%

-module(ersip_method_set).

-export([make/1,
         raw/1,
         new/1,
         has/2,
         intersection/2,
         to_list/1,
         invite_set/0
        ]).
-export_type([set/0, raw/0]).

%%===================================================================
%% Types
%%===================================================================

-type set() :: {method_set, gb_sets:set(ersip_method:method())}.
-type raw() :: [binary()].

%%===================================================================
%% API
%%===================================================================

-spec make(raw()) -> set().
make(BinaryList) when is_list(BinaryList) ->
    new([ersip_method:make(M) || M <- BinaryList]).

-spec new([ersip_method:method()]) -> set().
new(MethodList) ->
    {method_set, gb_sets:from_list(MethodList)}.

-spec has(ersip_method:method(), set()) -> boolean().
has({method, _} = Method, {method_set, Set}) ->
    gb_sets:is_element(Method, Set).

-spec intersection(set(), set()) -> set().
intersection({method_set, M1}, {method_set, M2}) ->
    {method_set, gb_sets:intersection(M1, M2)}.

-spec to_list(set()) -> [ersip_method:method()].
to_list({method_set, Set}) ->
    gb_sets:to_list(Set).

-spec invite_set() -> set().
invite_set() ->
    new([ersip_method:invite(),
         ersip_method:bye(),
         ersip_method:ack(),
         ersip_method:cancel()]).

-spec raw(set()) -> raw().
raw({method_set, Set}) ->
    [ersip_method:to_binary(M) || M <- gb_sets:to_list(Set)].
