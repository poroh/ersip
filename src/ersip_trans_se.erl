%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Side effects of transaction handlers
%%

-module(ersip_trans_se).

-export([clear_trans/1,
         send_request/1,
         send_response/1,
         tu_result/1,
         set_timer/2
        ]).

-export_type([effect/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type effect() :: clear_trans()
                | tu_result()
                | send_request()
                | send_response()
                | set_timer().

-type clear_trans()   :: {clear_trans,   clear_reason()}.
-type tu_result()     :: {tu_result,     ersip_sipmsg:sipmsg()}.
-type send_request()  :: {send_request,  ersip_request:request()}.
-type send_response() :: {send_response, ersip_sipmsg:sipmsg()}.
-type set_timer()     :: {set_timer,     {timeout(), TimerEv :: timer_event()}}.
-type timer_event()   :: term().
-type clear_reason()  :: normal
                       | timeout.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Delete transaction. Transaction must be removed if it saved
%% somewhere.
-spec clear_trans(clear_reason()) -> clear_trans().
clear_trans(Reason) ->
    {clear_trans, Reason}.

%% @doc Inform transaction user about transaction result.
-spec tu_result(ersip_sipmsg:sipmsg()) -> tu_result().
tu_result(SipMsg) ->
    {tu_result, SipMsg}.

%% @doc Send request message.
-spec send_request(Request) -> send_request() when
      Request :: ersip_request:request().
send_request(OutReq) ->
    {send_request, OutReq}.

-spec send_response(Resp) -> send_response() when
      Resp :: ersip_sipmsg:sipmsg().
send_response(SipMsg) ->
    {send_response, SipMsg}.

%% @doc Set timer for specified time interval. After timeout is
%% expired TimerFun must be called to process timer event.
-spec set_timer(timeout(), timer_event()) -> set_timer().
set_timer(Timeout, TimerEv) ->
    {set_timer, {Timeout, TimerEv}}.

