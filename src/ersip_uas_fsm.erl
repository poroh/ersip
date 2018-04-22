%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAS non-invite transaction
%%
%% Pure FSM implementation - transformation events to side effects.
%%

-module(ersip_uas_fsm).

-export([new/4,
         event/2,
         id/1]).
-export_type([uas/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type result()  :: {uas(), [ersip_trans_se:effect()]}.
-type event()   :: enter
                 | retransmit
                 | {send_resp, ersip_status:response_type(), Response :: term()}
                 | {timer, TimerFun :: fun((uas()) -> result())}.
-type request()  :: term().
-type response() :: term().

-record(uas, {id                         :: ersip_trans:tid(),
              state     = fun 'Trying'/2 :: fun((event(), uas()) -> result()),
              last_resp = undefined      :: response() | undefined,
              transport                  :: reliable | unreliable,
              options                    :: ersip:uas_options()
             }).

-type uas() :: #uas{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create new UAS transaction. Result of creation is UAS state
%% and set of side effects that produced because of creation.
%%
%% Request is not interpretted in any way.
-spec new(Id, Reliable, Request, Options) -> result() when
      Id       :: ersip_trans:tid(),
      Reliable :: reliable | unreliable,
      Request  :: request(),
      Options  :: ersip:uas_options().
new(Id, ReliableTranport, Request, Options) ->
    new_impl(Id, ReliableTranport, Request, Options).

%% @doc Process event by UAS.
%%
%% Function retuns new UAS state and side effects that must be done by
%% caller.
%%
%% Defined events:
%% {timer, timer_j}               - timer alarm that was requested by previous side effect
%% {send_resp, RespType, message} - send response with defined type.
%% retransmit                       - message retransmit received by UAC
%%
%% Side effects are defined in module ersip_trans_se
%%
-spec event(Event, uas()) -> result() when
      Event :: {timer, timer_j}
             | {send_resp, ersip_status:response_type(), response()}
             | retransmit.
event(Evt, UAS) ->
    process_event(Evt, UAS).

id(#uas{id = X}) ->
    X.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-define(default_options,
        #{sip_t1 => 500}).
-define(T1(UAS), maps:get(sip_t1, UAS#uas.options)).

new_impl(Id, Reliable, Request, Options) ->
    %% The state machine is initialized in the "Trying" state and is
    %% passed a request other than INVITE or ACK when initialized.
    %% This request is passed up to the TU.
    UAS = #uas{id        = Id,
               options   = maps:merge(?default_options, Options),
               transport = Reliable
              },
    {UAS, [ersip_trans_se:new_trans(UAS),
           ersip_trans_se:tu_result(Request)]}.

%%
%% Trying state
%%
'Trying'(retransmit, UAS) ->
    %% Once in the "Trying" state, any further request
    %% retransmissions are discarded.
    {UAS, []};
'Trying'({send_resp, provisional, Resp}, UAS) ->
    %% While in the "Trying" state, if the TU passes a provisional
    %% response to the server transaction, the server transaction MUST
    %% enter the "Proceeding" state.
    UAS1 = set_state(fun 'Proceeding'/2, UAS),
    UAS2 = UAS1#uas{last_resp = Resp},
    {UAS3, SideEffects} = process_event(enter, UAS2),
    {UAS3,  [ersip_trans_se:send(Resp) | SideEffects]};
'Trying'({send_resp, final, Resp}, UAS) ->
    completed(Resp, UAS).

%%
%% Proceeding state
%%
'Proceeding'(enter, UAS) ->
    {UAS, []};
'Proceeding'({send_resp, provisional, Resp}, UAS) ->
    %% Any further provisional responses that are received from the TU
    %% while in the "Proceeding" state MUST be passed to the transport
    %% layer for transmission.
    UAS1 = UAS#uas{last_resp = Resp},
    {UAS1, [ersip_trans_se:send(Resp)]};
'Proceeding'({send_resp, final, Resp}, UAS) ->
    completed(Resp, UAS);
'Proceeding'(retransmit, UAS) ->
    %% If a retransmission of the request is received while in the
    %% "Proceeding" state, the most recently sent provisional response
    %% MUST be passed to the transport layer for retransmission.
    {UAS, [ersip_trans_se:send(UAS#uas.last_resp)]}.

%%
%% Completed state
%%
'Completed'(enter, UAS) ->
    %% When the server transaction enters the "Completed" state, it
    %% MUST set Timer J to fire in 64*T1 seconds for unreliable
    %% transports, and zero seconds for reliable transports.
    case UAS#uas.transport of
        reliable ->
            %% The server transaction remains in this state until
            %% Timer J fires, at which point it MUST transition to the
            %% "Terminated" state.
            UAS1 = set_state(fun 'Terminated'/2, UAS),
            process_event(enter, UAS1);
        unreliable ->
            set_timer_j(64 * ?T1(UAS), UAS)
    end;
'Completed'(retransmit, UAS) ->
    %% final response to the transport layer for retransmission
    %% whenever a retransmission of the request is received
    {UAS, [ersip_trans_se:send(UAS#uas.last_resp)]};
'Completed'({send_resp, _, _}, UAS) ->
    %% Any other final responses passed by the TU to the server
    %% transaction MUST be discarded while in the "Completed" state.
    {UAS, []};
'Completed'({timer, timer_j}, UAS) ->
    %% The server transaction remains in this state until
    %% Timer J fires, at which point it MUST transition to the
    %% "Terminated" state.
    UAS1 = set_state(fun 'Terminated'/2, UAS),
    process_event(enter, UAS1).

%%
%% Terminated state
%%
'Terminated'(enter, UAS) ->
    %% The server transaction MUST be destroyed the instant it enters
    %% the "Terminated" state
    {UAS, [ersip_trans_se:clear_trans(UAS)]}.

%%
%% Helpers
%%
-spec completed(response(), uas()) -> result().
completed(Resp, UAS) ->
    UAS1 = UAS#uas{last_resp = Resp},
    UAS2 = set_state(fun 'Completed'/2, UAS1),
    {UAS3, SideEffects} = process_event(enter, UAS2),
    {UAS3, [ersip_trans_se:send(Resp) | SideEffects]}.

-spec process_event(Event :: term(), uas()) -> result().
process_event(Event, #uas{state = StateF} = UAS) ->
    StateF(Event, UAS).

-spec set_state(fun((Event :: term(), uas()) -> result()), uas()) -> uas().
set_state(State, UAS) ->
    UAS#uas{state = State}.

-spec set_timer_j(pos_integer(), uas()) -> result().
set_timer_j(Timeout, UAS) ->
    {UAS, [ersip_trans_se:set_timer(Timeout, timer_j)]}.
