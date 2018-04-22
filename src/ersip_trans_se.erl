%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Side effects of transaction handlers
%%

-module(ersip_trans_se).

-export([new_trans/1,
         clear_trans/1,
         send/1,
         tu_result/1,
         set_timer/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type effect() :: new_trans()
                | clear_trans()
                | tu_result()
                | send()
                | set_timer().

-type new_trans()   :: {new_trans,   ersip_trans:trans()}.
-type clear_trans() :: {clear_trans, ersip_trans:trans()}.
-type tu_result()   :: {tu_result,   ersip_sipmsg:sipmsg()}.
-type send()        :: {send,        ersip_sipmsg:sipmsg()}.
-type set_timer()   :: {set_timer,   {timeout(), TimerEv :: term()}}.

-export_type([effect/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create new UAC transaction side effect. Transaction may be
%% remembered in some storage to match response.
-spec new_trans(ersip_trans:trans()) -> new_trans().
new_trans(Trans) ->
    {new_trans, Trans}.

%% @doc Delete UAC transaction side effect. Transaction must be
%% removed if it saved somewhere.
-spec clear_trans(ersip_trans:trans()) -> clear_trans().
clear_trans(Trans) ->
    {clear_trans, Trans}.

%% @doc Inform transaction user about transaction result.
-spec tu_result(ersip_sipmsg:sipmsg()) -> tu_result().
tu_result(SipMsg) ->
    {tu_result, SipMsg}.

%% @doc Send respinse to the UAC.
-spec send(ersip_sipmsg:sipmsg()) -> send().
send(SipMsg) ->
    {send, SipMsg}.

%% @doc Set timer for specified time interval. After timeout is
%% expired TimerFun must be called.
-spec set_timer(timeout(), TimerEv :: term()) -> set_timer().
set_timer(Timeout, TimerEv) ->
    {set_timer, {Timeout, TimerEv}}.
