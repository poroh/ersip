%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Server INVITE transaction
%%
%% Pure FSM implementation - transformation events to side effects.
%%


-module(ersip_trans_inv_server).

-export([new/3,
         event/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(trans_inv_server, {state     = fun 'Proceeding'/2 :: state(),
                           transport                  :: transport_type(),
                           options                    :: ersip:sip_options(),
                           last_resp                  :: ersip_sipmsg:sipmsg(),
                           timer_g_timeout = 500      :: pos_integer(),
                           clear_reason = normal      :: ersip_trans_se:clear_reason()
                          }).
-type trans_inv_server() :: #trans_inv_server{}.
-type event()   :: term().
-type result()  :: {trans_inv_server(), [ersip_trans_se:effect()]}.
-type state()   :: fun((event(), trans_inv_server()) -> result()).
-type transport_type() :: reliable | unreliable.

%%%===================================================================
%%% API
%%%===================================================================


-spec new(Reliable, Request, Options) -> result() when
      Reliable :: reliable | unreliable,
      Request  :: ersip_sipmsg:sipmsg(),
      Options  :: ersip:sip_options().
new(ReliableTranport, Request, Options) ->
    case ersip_sipmsg:type(Request) of
        request ->
            new_impl(ReliableTranport, Request, Options);
        response ->
            error({api_error, <<"server transaction must be initialyzed with INVITE request">>})
    end.

-spec event(Event, trans_inv_server()) -> result() when
      Event :: {timer, timer_j}
             | {send, ersip_sipmsg:sipmsg()}
             | retransmit.
event({received, SipMsg}, ServerTrans) ->
    case ersip_sipmsg:type(SipMsg) of
        request ->
            %% Only can be ACK or retransmit otherwise it will not
            %% match transaction id.
            case ersip_sipmsg:method(SipMsg) of
                {method, <<"ACK">>} ->
                    process_event({ack, SipMsg}, {ServerTrans, []});
                {method, <<"INVITE">>} ->
                    process_event(retransmit, {ServerTrans, []})
            end;
        response ->
            error({api_error, <<"response cannot match server transaction">>})
    end;
event({send, SipMsg}, ServerTrans) ->
    case ersip_sipmsg:type(SipMsg) of
        request ->
            error({api_error, <<"cannot send request using sever transaction">>});
        response ->
            RespType = ersip_status:response_type(ersip_sipmsg:status(SipMsg)),
            process_event({send_resp, RespType, SipMsg}, {ServerTrans, []})
    end;
event({timer, _} = TimerEv, ServerTrans) ->
    process_event(TimerEv, {ServerTrans, []}).


%%%===================================================================
%%% Internal implementation
%%%===================================================================

-define(default_options,
        #{sip_t1 => 500,
          sip_t2 => 4000,
          sip_t4 => 5000
         }).
-define(T1(InvServerTrans), maps:get(sip_t1, InvServerTrans#trans_inv_server.options)).
-define(T2(InvServerTrans), maps:get(sip_t2, InvServerTrans#trans_inv_server.options)).
-define(T4(InvServerTrans), maps:get(sip_t4, InvServerTrans#trans_inv_server.options)).

-spec new_impl(transport_type(), ersip_sipmsg:sipmsg(), ersip_sip:options()) -> result().
new_impl(Reliable, ReqSipMsg, Options) ->
    %% When a server transaction is constructed for a request, it
    %% enters the "Proceeding" state.  The server transaction MUST
    %% generate a 100 (Trying) response unless it knows that the TU
    %% will generate a provisional or final response within 200 ms, in
    %% which case it MAY generate a 100 (Trying) response.
    TryingSipMsg = ersip_sipmsg:reply(100, ReqSipMsg),
    ServerInvTrans =
        #trans_inv_server{options   = maps:merge(?default_options, Options),
                          transport = Reliable,
                          last_resp = TryingSipMsg
                         },
    {ServerInvTrans, [ersip_trans_se:send_response(TryingSipMsg),
                      ersip_trans_se:tu_result(ReqSipMsg)]}.

-spec 'Proceeding'(event(), trans_inv_server()) -> result().
'Proceeding'({send_resp, RespType, SipMsg}, #trans_inv_server{} = Trans) ->
    Status = ersip_sipmsg:status(SipMsg),
    case RespType of
        provisional ->
            %% The TU passes any number of provisional responses to
            %% the server transaction.  So long as the server
            %% transaction is in the "Proceeding" state, each of these
            %% MUST be passed to the transport layer for transmission.
            %% They are not sent reliably by the transaction layer
            %% (they are not retransmitted by it) and do not cause a
            %% change in the state of the server transaction.
            Trans1 = save_last_resp(SipMsg, Trans),
            {Trans1, [ersip_trans_se:send_response(SipMsg)]};
        final ->
            case Status of
                S when S >= 200 andalso S =< 299 ->
                    %% This modification made by RFC6026:
                    %%
                    %% If the SIP element's TU (Transaction User)
                    %% issues a 2xx response for this transaction
                    %% while the state machine is in the "Proceeding"
                    %% state, the state machine MUST transition to the
                    %% "Accepted" state and set Timer L to 64*T1
                    Trans1 = set_state(fun 'Accepted'/2, Trans),
                    Result = {Trans1, [ersip_trans_se:send_response(SipMsg)]},
                    set_timer_l(64 * ?T1(Trans1), Result);
                S when S >= 300 andalso S =< 699 ->
                    %% While in the "Proceeding" state, if the TU
                    %% passes a response with status code from 300 to
                    %% 699 to the server transaction, the response
                    %% MUST be passed to the transport layer for
                    %% transmission, and the state machine MUST enter
                    %% the "Completed" state.  For unreliable
                    %% transports, timer G is set to fire in T1
                    %% seconds, and is not set to fire for reliable
                    %% transports.
                    Trans1 = save_last_resp(SipMsg, Trans),
                    Trans2 = set_state(fun 'Completed'/2, Trans1),
                    Result =
                        case Trans2#trans_inv_server.transport of
                            reliable ->
                                {Trans2, [ersip_trans_se:send_response(SipMsg)]};
                            unreliable ->
                                Timeout = ?T1(Trans2),
                                Trans3 = Trans2#trans_inv_server{timer_g_timeout = Timeout},
                                Result0 = {Trans3, [ersip_trans_se:send_response(SipMsg)]},
                                set_timer_g(Timeout, Result0)
                        end,
                    process_event(enter, Result)
            end
    end;
'Proceeding'(retransmit, #trans_inv_server{last_resp = LastResp} = Trans) ->
    %% If a request retransmission is received while in the
    %% "Proceeding" state, the most recent provisional response that
    %% was received from the TU MUST be passed to the transport layer
    %% for retransmission.
    {Trans, [ersip_trans_se:send_response(LastResp)]};
'Proceeding'({ack, _}, #trans_inv_server{} = Trans) ->
    %% Out of order ACK - ignore it.
    {Trans, []}.

-spec 'Completed'(event(), trans_inv_server()) -> result().
'Completed'(enter, #trans_inv_server{} = Trans) ->
    %% When the "Completed" state is entered, timer H MUST be set to
    %% fire in 64*T1 seconds for all transports.
    set_timer_h(64*?T1(Trans), {Trans, []});
'Completed'({timer, timer_g}, #trans_inv_server{last_resp = LastResp, timer_g_timeout = TimerGTimeout} = Trans) ->
    %% If timer G fires, the response is passed to the transport layer
    %% once more for retransmission, and timer G is set to fire in
    %% MIN(2*T1, T2) seconds. From then on, when timer G fires, the
    %% response is passed to the transport again for transmission, and
    %% timer G is reset with a value that doubles, unless that value
    %% exceeds T2, in which case it is reset with the value of T2.
    NewTimerGTimeout = erlang:min(2*TimerGTimeout, ?T2(Trans)),
    Trans2 = Trans#trans_inv_server{timer_g_timeout = NewTimerGTimeout},
    Result = {Trans2, [ersip_trans_se:send_response(LastResp)]},
    set_timer_g(NewTimerGTimeout, Result);
'Completed'({ack, _}, #trans_inv_server{} = Trans) ->
    %% If an ACK is received while the server transaction is in the
    %% "Completed" state, the server transaction MUST transition to
    %% the "Confirmed" state.
    Trans1 = set_state(fun 'Confirmed'/2, Trans),
    process_event(enter, {Trans1, []});
'Completed'({timer, timer_h}, #trans_inv_server{} = Trans) ->
    %% If timer H fires while in the "Completed" state, it implies
    %% that the ACK was never received.  In this case, the server
    %% transaction MUST transition to the "Terminated" state, and MUST
    %% indicate to the TU that a transaction failure has occurred.
    terminate(no_ack, Trans);
'Completed'(retransmit, #trans_inv_server{last_resp = LastResp} = Trans) ->
    %% Furthermore, while in the "Completed" state, if a request
    %% retransmission is received, the server SHOULD pass the response
    %% to the transport for retransmission
    {Trans, [ersip_trans_se:send_response(LastResp)]}.

-spec 'Confirmed'(event(), trans_inv_server()) -> result().
'Confirmed'(enter, #trans_inv_server{transport = unreliable} = Trans) ->
    %% When this state is entered, timer I is set to fire in T4
    %% seconds for unreliable transports, and zero seconds for
    %% reliable transports.
    set_timer_i(?T4(Trans), {Trans, []});
'Confirmed'(enter, #trans_inv_server{transport = reliable} = Trans) ->
    process_event({timer, timer_i}, {Trans, []});
'Confirmed'({timer, timer_i}, #trans_inv_server{} = Trans) ->
    %% Once timer I fires, the server MUST transition to the
    %% "Terminated" state.
    terminate(normal, Trans);
'Confirmed'(_Event, #trans_inv_server{} = Trans) ->
    %% ignore acks/retransmits etc.
    {Trans, []}.

%% @doc RFC6026 state:
-spec 'Accepted'(event(), trans_inv_server()) -> result().

'Accepted'(retransmit, #trans_inv_server{} = Trans) ->
    %% While in the "Accepted" state, any retransmissions of the
    %% INVITE received will match this transaction state machine and
    %% will be absorbed by the machine without changing its
    %% state. These retransmissions are not passed onto the TU.
    {Trans, []};
'Accepted'({send_resp, final, SipMsg}, #trans_inv_server{} = Trans) ->
    %% Any retransmission of the 2xx response passed from the TU to
    %% the transaction while in the "Accepted" state MUST be passed to
    %% the transport layer for transmission.
    case ersip_sipmsg:status(SipMsg) of
        Code when Code >= 200 andalso Code =< 299 ->
            {Trans, [ersip_trans_se:send_response(SipMsg)]};
        Code ->
            CodeBin = integer_to_binary(Code),
            error({api_error, <<"unexpected response code ", CodeBin/binary>>})
    end;
'Accepted'({ack, ACKSipMsg}, #trans_inv_server{} = Trans) ->
    %% Any ACKs received from the network while in the "Accepted"
    %% state MUST be passed directly to the TU and not absorbed.
    {Trans, [ersip_trans_se:tu_result(ACKSipMsg)]};
'Accepted'({timer, timer_l}, #trans_inv_server{} = Trans) ->
    %% When Timer L fires and the state machine is in the "Accepted"
    %% state, the machine MUST transition to the "Terminated" state.
    terminate(normal, Trans);
'Accepted'(_Event, #trans_inv_server{} = Trans) ->
    {Trans, []}.

-spec 'Terminated'(event(), trans_inv_server()) -> result().
'Terminated'(_Event, #trans_inv_server{clear_reason = R} = Trans) ->
    {Trans, [ersip_trans_se:clear_trans(R)]}.

-spec process_event(Event :: term(), result()) -> result().
process_event(Event, {#trans_inv_server{state = StateF} = Trans, SE}) ->
    {Trans1, EventSE} = StateF(Event, Trans),
    {Trans1, SE ++ EventSE}.

-spec set_state(state(), trans_inv_server()) -> trans_inv_server().
set_state(State, ServerTrans) ->
    ServerTrans#trans_inv_server{state = State}.

-spec save_last_resp(ersip_sipmsg:sipmsg(), trans_inv_server()) -> trans_inv_server().
save_last_resp(SipMsg, ServerTrans) ->
    ServerTrans#trans_inv_server{last_resp = SipMsg}.

-spec set_timer_g(pos_integer(), result()) -> result().
set_timer_g(Timeout, Result) ->
    set_timer(Timeout, timer_g, Result).

-spec set_timer_h(pos_integer(), trans_inv_server()) -> result().
set_timer_h(Timeout, Result) ->
    set_timer(Timeout, timer_h, Result).

-spec set_timer_i(pos_integer(), trans_inv_server()) -> result().
set_timer_i(Timeout, Result) ->
    set_timer(Timeout, timer_i, Result).

-spec set_timer_l(pos_integer(), result()) -> result().
set_timer_l(Timeout, Result) ->
    set_timer(Timeout, timer_l, Result).


-spec set_timer(pos_integer(), TimerType, result()) -> result() when
      TimerType :: timer_g
                 | timer_h
                 | timer_i
                 | timer_l.
set_timer(Timeout, TimerType, {#trans_inv_server{} = Trans, SE}) ->
    {Trans, SE ++ [ersip_trans_se:set_timer(Timeout, {timer, TimerType})]}.

-spec terminate(Reason,  trans_inv_server()) -> result() when
      Reason :: ersip_trans_se:clear_reason().
terminate(Reason, Trans) ->
    Trans1 = set_state(fun 'Terminated'/2, Trans),
    Trans2 = Trans1#trans_inv_server{clear_reason = Reason},
    process_event('enter', {Trans2, []}).
