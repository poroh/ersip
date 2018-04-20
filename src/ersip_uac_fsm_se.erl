%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAC non-invite transaction
%% Side Effects Definition
%%

-module(ersip_uac_fsm_se).

-export([new_trans/1,
         clear_trans/1,
         send/2,
         tu_result/2,
         set_timer/3
        ]).

-type effect()  :: term().

-export_type([effect/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create new UAC transaction side effect. Transaction may be
%% remembered in some storage to match response.
new_trans(Trans) ->
    {new_trans, [Trans]}.

%% @doc Delete UAC transaction side effect. Transaction must be
%% removed if it saved somewhere.
clear_trans(Trans) ->
    {clear_trans, [Trans]}.

%% @doc Send message to the nexthop.
send(RawMessage, Tid) ->
    {send, [RawMessage, Tid]}.

%% @doc Inform transaction user about transaction result.
tu_result(Result, Tid) ->
    {tu_result, [Result, Tid]}.

%% @doc Set timer for specified time interval. After timeout is
%% expired TimerFun must be called.
set_timer(Timeout, TimerEv, Tid) ->
    {set_timer, [Timeout, TimerEv, Tid]}.
