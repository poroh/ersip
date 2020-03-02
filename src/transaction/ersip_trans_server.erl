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

-export([new/3,
         event/2,
         to_map/1,
         from_map/1,
         has_final_response/1
        ]).

-export_type([trans_server/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type result()  :: {trans_server(), [ersip_trans_se:effect()]}.
-type request()  :: term().
-type response() :: term().

-record(trans_server, {state     = 'Trying'  :: state(),
                       last_resp = undefined :: response() | undefined,
                       transport             :: reliable | unreliable,
                       options               :: ersip:sip_options()
                      }).

-type trans_server() :: #trans_server{}.

-type trans_server_map() :: #{state => state(),
                              last_resp => response() | undefined,
                              transport => reliable | unreliable,
                              options => ersip:sip_options()
                             }.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create new ServerTrans transaction. Result of creation is ServerTrans state
%% and set of side effects that produced because of creation.
-spec new(Reliable, Request, Options) -> result() when
      Reliable :: reliable | unreliable,
      Request  :: request(),
      Options  :: ersip:sip_options().
new(ReliableTranport, Request, Options) ->
    new_impl(ReliableTranport, Request, Options).

-spec event(Event, trans_server()) -> result() when
      Event :: {timer, timer_j}
             | {send, ersip_sipmsg:sipmsg()}
             | retransmit.
event({received, SipMsg}, ServerTrans) ->
    case ersip_sipmsg:type(SipMsg) of
        request ->
            %% Only can be the retransmit
            process_event(retransmit, ServerTrans);
        response ->
            error({api_error, <<"response cannot match server transaction">>})
    end;
event({send, SipMsg}, ServerTrans) ->
    case ersip_sipmsg:type(SipMsg) of
        request ->
            error({api_error, <<"cannot send request using sever transaction">>});
        response ->
            RespType = ersip_status:response_type(ersip_sipmsg:status(SipMsg)),
            process_event({send_resp, RespType, SipMsg}, ServerTrans)
    end;
event({timer, _} = TimerEv, ServerTrans) ->
    process_event(TimerEv, ServerTrans).

%% @doc Transaction has sent final response
-spec has_final_response(trans_server()) -> boolean().
has_final_response(#trans_server{state = 'Completed'}) ->
    true;
has_final_response(#trans_server{state = 'Terminated'}) ->
    true;
has_final_response(#trans_server{}) ->
    false.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-type state() :: 'Trying' | 'Proceeding' | 'Completed' | 'Terminated'.

-define(default_options,
        #{sip_t1 => 500}).
-define(T1(ServerTrans), maps:get(sip_t1, ServerTrans#trans_server.options)).

new_impl(Reliable, Request, Options) ->
    %% The state machine is initialized in the "Trying" state and is
    %% passed a request other than INVITE or ACK when initialized.
    %% This request is passed up to the TU.
    ServerTrans =
        #trans_server{options   = maps:merge(?default_options, Options),
                      transport = Reliable
                     },
    {ServerTrans, [ersip_trans_se:tu_result(Request)]}.

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
    ServerTrans1 = set_state('Proceeding', ServerTrans),
    ServerTrans2 = ServerTrans1#trans_server{last_resp = Resp},
    {ServerTrans3, SideEffects} = process_event(enter, ServerTrans2),
    {ServerTrans3,  [ersip_trans_se:send_response(Resp) | SideEffects]};
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
    {ServerTrans1, [ersip_trans_se:send_response(Resp)]};
'Proceeding'({send_resp, final, Resp}, ServerTrans) ->
    completed(Resp, ServerTrans);
'Proceeding'(retransmit, ServerTrans) ->
    %% If a retransmission of the request is received while in the
    %% "Proceeding" state, the most recently sent provisional response
    %% MUST be passed to the transport layer for retransmission.
    {ServerTrans, [ersip_trans_se:send_response(ServerTrans#trans_server.last_resp)]}.

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
            ServerTrans1 = set_state('Terminated', ServerTrans),
            process_event(enter, ServerTrans1);
        unreliable ->
            set_timer_j(64 * ?T1(ServerTrans), ServerTrans)
    end;
'Completed'(retransmit, ServerTrans) ->
    %% final response to the transport layer for retransmission
    %% whenever a retransmission of the request is received
    {ServerTrans, [ersip_trans_se:send_response(ServerTrans#trans_server.last_resp)]};
'Completed'({send_resp, _, _}, ServerTrans) ->
    %% Any other final responses passed by the TU to the server
    %% transaction MUST be discarded while in the "Completed" state.
    {ServerTrans, []};
'Completed'({timer, timer_j}, ServerTrans) ->
    %% The server transaction remains in this state until
    %% Timer J fires, at which point it MUST transition to the
    %% "Terminated" state.
    ServerTrans1 = set_state('Terminated', ServerTrans),
    process_event(enter, ServerTrans1).

%%
%% Terminated state
%%
'Terminated'(enter, ServerTrans) ->
    %% The server transaction MUST be destroyed the instant it enters
    %% the "Terminated" state
    {ServerTrans, [ersip_trans_se:clear_trans(normal)]}.

%%
%% Helpers
%%
-spec completed(response(), trans_server()) -> result().
completed(Resp, ServerTrans) ->
    ServerTrans1 = ServerTrans#trans_server{last_resp = Resp},
    ServerTrans2 = set_state('Completed', ServerTrans1),
    {ServerTrans3, SideEffects} = process_event(enter, ServerTrans2),
    {ServerTrans3, [ersip_trans_se:send_response(Resp) | SideEffects]}.

-spec process_event(Event :: term(), trans_server()) -> result().
process_event(Event, #trans_server{state = 'Trying'} = ServerTrans) ->
    'Trying'(Event, ServerTrans);
process_event(Event, #trans_server{state = 'Proceeding'} = ServerTrans) ->
    'Proceeding'(Event, ServerTrans);
process_event(Event, #trans_server{state = 'Completed'} = ServerTrans) ->
    'Completed'(Event, ServerTrans);
process_event(Event, #trans_server{state = 'Terminated'} = ServerTrans) ->
    'Terminated'(Event, ServerTrans).

-spec set_state(state(), trans_server()) -> trans_server().
set_state(State, ServerTrans) ->
    ServerTrans#trans_server{state = State}.

-spec set_timer_j(pos_integer(), trans_server()) -> result().
set_timer_j(Timeout, ServerTrans) ->
    {ServerTrans, [ersip_trans_se:set_timer(Timeout, {timer, timer_j})]}.

-spec to_map(trans_server()) -> trans_server_map().
to_map(Trans) ->
    #{state => Trans#trans_server.state,
      last_resp => Trans#trans_server.last_resp,
      transport => Trans#trans_server.transport,
      options => Trans#trans_server.options}.

-spec from_map(trans_server_map()) -> trans_server().
from_map(Map) ->
  Trans = #trans_server{options = maps:get(options, Map),
                        transport = maps:get(transport, Map)
                       },
  maps:fold(fun (Field, Value, T) ->
                case Field of
                  state -> T#trans_server{state = Value};
                  last_resp -> T#trans_server{last_resp = Value};
                  _ -> T
                end
            end,
            Trans,
            Map).
