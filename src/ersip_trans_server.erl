%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Server non-INVITE transaction
%%
%% Pure FSM implementation - transformation events to side effects.
%%

-module(ersip_trans_server).

-export([new/4,
         event/2,
         id/1]).
-export_type([trans_server/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type result()  :: {trans_server(), [ersip_trans_se:effect()]}.
-type event()   :: enter
                 | retransmit
                 | {send_resp, ersip_status:response_type(), Response :: term()}
                 | {timer, TimerFun :: fun((trans_server()) -> result())}.
-type request()  :: term().
-type response() :: term().

-record(trans_server, {id                         :: ersip_trans:tid(),
                       state     = fun 'Trying'/2 :: fun((event(), trans_server()) -> result()),
                       last_resp = undefined      :: response() | undefined,
                       transport                  :: reliable | unreliable,
                       options                    :: ersip:sip_options()
                      }).

-type trans_server() :: #trans_server{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create new ServerTrans transaction. Result of creation is ServerTrans state
%% and set of side effects that produced because of creation.
%%
%% Request is not interpretted in any way.
-spec new(Id, Reliable, Request, Options) -> result() when
      Id       :: ersip_trans:tid(),
      Reliable :: reliable | unreliable,
      Request  :: request(),
      Options  :: ersip:sip_options().
new(Id, ReliableTranport, Request, Options) ->
    new_impl(Id, ReliableTranport, Request, Options).

%% @doc Process event by ServerTrans.
%%
%% Function retuns new ServerTrans state and side effects that must be done by
%% caller.
%%
%% Defined events:
%% {timer, timer_j}               - timer alarm that was requested by previous side effect
%% {send_resp, RespType, message} - send response with defined type.
%% retransmit                       - message retransmit received by UAC
%%
%% Side effects are defined in module ersip_trans_se
%%
-spec event(Event, trans_server()) -> result() when
      Event :: {timer, timer_j}
             | {send_resp, ersip_status:response_type(), response()}
             | retransmit.
event(Evt, ServerTrans) ->
    process_event(Evt, ServerTrans).

id(#trans_server{id = X}) ->
    X.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-define(default_options,
        #{sip_t1 => 500}).
-define(T1(ServerTrans), maps:get(sip_t1, ServerTrans#trans_server.options)).

new_impl(Id, Reliable, Request, Options) ->
    %% The state machine is initialized in the "Trying" state and is
    %% passed a request other than INVITE or ACK when initialized.
    %% This request is passed up to the TU.
    ServerTrans = #trans_server{id        = Id,
               options   = maps:merge(?default_options, Options),
               transport = Reliable
              },
    {ServerTrans, [ersip_trans_se:new_trans(ServerTrans),
           ersip_trans_se:tu_result(Request)]}.

%%
%% Trying state
%%
'Trying'(retransmit, ServerTrans) ->
    %% Once in the "Trying" state, any further request
    %% retransmissions are discarded.
    {ServerTrans, []};
'Trying'({send_resp, provisional, Resp}, ServerTrans) ->
    %% While in the "Trying" state, if the TU passes a provisional
    %% response to the server transaction, the server transaction MUST
    %% enter the "Proceeding" state.
    ServerTrans1 = set_state(fun 'Proceeding'/2, ServerTrans),
    ServerTrans2 = ServerTrans1#trans_server{last_resp = Resp},
    {ServerTrans3, SideEffects} = process_event(enter, ServerTrans2),
    {ServerTrans3,  [ersip_trans_se:send(Resp) | SideEffects]};
'Trying'({send_resp, final, Resp}, ServerTrans) ->
    completed(Resp, ServerTrans).

%%
%% Proceeding state
%%
'Proceeding'(enter, ServerTrans) ->
    {ServerTrans, []};
'Proceeding'({send_resp, provisional, Resp}, ServerTrans) ->
    %% Any further provisional responses that are received from the TU
    %% while in the "Proceeding" state MUST be passed to the transport
    %% layer for transmission.
    ServerTrans1 = ServerTrans#trans_server{last_resp = Resp},
    {ServerTrans1, [ersip_trans_se:send(Resp)]};
'Proceeding'({send_resp, final, Resp}, ServerTrans) ->
    completed(Resp, ServerTrans);
'Proceeding'(retransmit, ServerTrans) ->
    %% If a retransmission of the request is received while in the
    %% "Proceeding" state, the most recently sent provisional response
    %% MUST be passed to the transport layer for retransmission.
    {ServerTrans, [ersip_trans_se:send(ServerTrans#trans_server.last_resp)]}.

%%
%% Completed state
%%
'Completed'(enter, ServerTrans) ->
    %% When the server transaction enters the "Completed" state, it
    %% MUST set Timer J to fire in 64*T1 seconds for unreliable
    %% transports, and zero seconds for reliable transports.
    case ServerTrans#trans_server.transport of
        reliable ->
            %% The server transaction remains in this state until
            %% Timer J fires, at which point it MUST transition to the
            %% "Terminated" state.
            ServerTrans1 = set_state(fun 'Terminated'/2, ServerTrans),
            process_event(enter, ServerTrans1);
        unreliable ->
            set_timer_j(64 * ?T1(ServerTrans), ServerTrans)
    end;
'Completed'(retransmit, ServerTrans) ->
    %% final response to the transport layer for retransmission
    %% whenever a retransmission of the request is received
    {ServerTrans, [ersip_trans_se:send(ServerTrans#trans_server.last_resp)]};
'Completed'({send_resp, _, _}, ServerTrans) ->
    %% Any other final responses passed by the TU to the server
    %% transaction MUST be discarded while in the "Completed" state.
    {ServerTrans, []};
'Completed'({timer, timer_j}, ServerTrans) ->
    %% The server transaction remains in this state until
    %% Timer J fires, at which point it MUST transition to the
    %% "Terminated" state.
    ServerTrans1 = set_state(fun 'Terminated'/2, ServerTrans),
    process_event(enter, ServerTrans1).

%%
%% Terminated state
%%
'Terminated'(enter, ServerTrans) ->
    %% The server transaction MUST be destroyed the instant it enters
    %% the "Terminated" state
    {ServerTrans, [ersip_trans_se:clear_trans(ServerTrans)]}.

%%
%% Helpers
%%
-spec completed(response(), trans_server()) -> result().
completed(Resp, ServerTrans) ->
    ServerTrans1 = ServerTrans#trans_server{last_resp = Resp},
    ServerTrans2 = set_state(fun 'Completed'/2, ServerTrans1),
    {ServerTrans3, SideEffects} = process_event(enter, ServerTrans2),
    {ServerTrans3, [ersip_trans_se:send(Resp) | SideEffects]}.

-spec process_event(Event :: term(), trans_server()) -> result().
process_event(Event, #trans_server{state = StateF} = ServerTrans) ->
    StateF(Event, ServerTrans).

-spec set_state(fun((Event :: term(), trans_server()) -> result()), trans_server()) -> trans_server().
set_state(State, ServerTrans) ->
    ServerTrans#trans_server{state = State}.

-spec set_timer_j(pos_integer(), trans_server()) -> result().
set_timer_j(Timeout, ServerTrans) ->
    {ServerTrans, [ersip_trans_se:set_timer(Timeout, timer_j)]}.
