%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Side effects of transaction handlers
%%

-module(ersip_trans_se).

-export([clear_trans/1,
         send/1,
         tu_result/1,
         set_timer/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type effect() :: clear_trans()
                | tu_result()
                | send()
                | set_timer().

-type clear_trans() :: {clear_trans, ersip_trans:trans()}.
-type tu_result()   :: {tu_result,   ersip_sipmsg:sipmsg()}.
-type send()        :: {send,        ersip_sipmsg:sipmsg()}.
-type set_timer()   :: {set_timer,   {timeout(), TimerEv :: term()}}.

-export_type([effect/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Delete transaction. Transaction must be removed if it saved
%% somewhere.
-spec clear_trans(ersip_trans:trans()) -> clear_trans().
clear_trans(Trans) ->
    {clear_trans, Trans}.

%% @doc Inform transaction user about transaction result.
-spec tu_result(ersip_sipmsg:sipmsg()) -> tu_result().
tu_result(SipMsg) ->
    {tu_result, SipMsg}.

%% @doc Send message.
-spec send(ersip_sipmsg:sipmsg()) -> send().
send(SipMsg) ->
    {send, SipMsg}.

%% @doc Set timer for specified time interval. After timeout is
%% expired TimerEv must be sent to the transaction.
-spec set_timer(timeout(), TimerEv :: term()) -> set_timer().
set_timer(Timeout, TimerEv) ->
    {set_timer, {Timeout, TimerEv}}.
