%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Side effects of user agents
%%

-module(ersip_ua_se).

-export([ua_result/1,
         send_response/1,
         send_request/1,
         set_timer/2,
         completed/1
        ]).

-export_type([effect/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type effect() :: ua_result()
                | send_response()
                | send_request()
                | set_timer()
                | completed().

-type ua_result()     :: {ua_result,     ersip_sipmsg:sipmsg()}.
-type send_response() :: {send_response, ersip_sipmsg:sipmsg()}.
-type send_request()  :: {send_request,  ersip_request:request()}.
-type set_timer()     :: {set_timer,     {Timeout :: non_neg_integer(),  TimerEv :: term()}}.
-type completed()     :: {completed,     complete_reason()}.
-type complete_reason() :: normal
                         | timeout
                         | no_ack.


%%%===================================================================
%%% API
%%%===================================================================

-spec ua_result(ersip_sipmsg:sipmsg()) -> ua_result().
ua_result(SipMsg) ->
    {ua_result, SipMsg}.

-spec send_response(ersip_sipmsg:sipmsg()) -> send_response().
send_response(SipMsg) ->
    {send_response, SipMsg}.

-spec send_request(ersip_sipmsg:sipmsg()) -> send_request().
send_request(SipMsg) ->
    {send_request, SipMsg}.

-spec set_timer(Timeout, TimerEv) -> set_timer() when
      Timeout :: non_neg_integer(),
      TimerEv :: term().
set_timer(Timeout, TimerEv) ->
    {set_timer, {Timeout, TimerEv}}.

-spec completed(complete_reason()) -> completed().
completed(Reason) ->
    {completed, Reason}.
