%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAC non-invite transaction
%% Side Effects Definition
%%

-module(ersip_uac_se).

-export([ new_trans/1,
          clear_trans/1,
          send/2,
          tu_result/1,
          set_timer/2
        ]).

-type effect()  :: term().

-export_type([ effect/0 ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create new UAC transaction side effect. Transaction may be
%% remembered in some storage to match response.
new_trans(Trans) ->
    { new_trans, [ Trans ] }.

%% @doc Delete UAC transaction side effect. Transaction must be
%% removed if it saved somewhere.
clear_trans(Trans) ->
    { clear_trans, [ Trans ] }.

%% @doc Send message to the nexthop.
send(RawMessage, SendResultFun) when is_binary(RawMessage) ->
    { send, [ RawMessage, SendResultFun ] }.

%% @doc Inform transaction user about transaction result.
tu_result(Result) ->
    { tu_result, [ Result ] }.

%% @doc Set timer for specified time interval. After timeout is
%% expired TimerFun must be called.
-spec set_timer(TimerFun, pos_integer()) -> { set_timer, [ TimerFun | pos_integer() ] } when
      TimerFun :: fun(() -> ersip_uac:result()).
set_timer(TimerFun, Timeout) ->
    { set_timer, [ TimerFun, Timeout ] }.
