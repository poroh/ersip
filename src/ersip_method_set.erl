%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP method set
%%

-module(ersip_method_set).

-export([new/1,
         has/2,
         to_list/1,
         invite_set/0
        ]).
-export_type([set/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type set() :: {method_set, gb_sets:set(ersip_method:method())}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new([ersip_method:method()]) -> set().
new(MethodList) ->
    {method_set, gb_sets:from_list(MethodList)}.

-spec has(ersip_method:method(), set()) -> boolean().
has({method, _} = Method, {method_set, Set}) ->
    gb_sets:is_element(Method, Set).

-spec to_list(set()) -> [ersip_method:method()].
to_list({method_set, Set}) ->
    gb_sets:to_list(Set).

-spec invite_set() -> set().
invite_set() ->
    new([ersip_method:invite(),
         ersip_method:bye(),
         ersip_method:ack(),
         ersip_method:cancel()]).
