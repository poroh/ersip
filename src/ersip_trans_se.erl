%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Side effects of transaction handlers
%%

-module(ersip_trans_se).

-export([clear_trans/0,
         clear_trans/1,
         send/1,
         tu_result/1,
         set_timer/2
        ]).

-export_type([effect/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type effect() :: clear_trans()
                | tu_result()
                | send()
                | set_timer().

-type clear_trans() :: {clear_trans, ersip_trans_id:transaction_id() | unknown}.
-type tu_result()   :: {tu_result,   ersip_sipmsg:sipmsg()}.
-type send()        :: {send,        ersip_sipmsg:sipmsg()}.
-type set_timer()   :: {set_timer,   {timeout(), TimerEv :: timer_event()}}.
-type timer_event() :: term().
%%%===================================================================
%%% API
%%%===================================================================

%% @doc Delete transaction. Transaction must be removed if it saved
%% somewhere.
-spec clear_trans() -> clear_trans().
clear_trans() ->
    {clear_trans, unknown}.

-spec clear_trans(ersip_trans_id:transaction_id()) -> clear_trans().
clear_trans(TransId) ->
    {clear_trans, TransId}.

%% @doc Inform transaction user about transaction result.
-spec tu_result(ersip_sipmsg:sipmsg()) -> tu_result().
tu_result(SipMsg) ->
    {tu_result, SipMsg}.

%% @doc Send message.
-spec send(RequestOrResp) -> send() when
      RequestOrResp :: ersip_sipmsg:sipmsg()
                     | ersip_request:request().
send(SipMsg) ->
    {send, SipMsg}.

%% @doc Set timer for specified time interval. After timeout is
%% expired TimerFun must be called to process timer event.
-spec set_timer(timeout(), timer_event()) -> set_timer().
set_timer(Timeout, TimerEv) ->
    {set_timer, {Timeout, TimerEv}}.

