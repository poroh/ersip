%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Client INVITE transaction
%%
%% Pure FSM implementation - transformation events to side effects.
%%


-module(ersip_trans_inv_client).

-export([new/3,
         event/2
        ]).


%%%===================================================================
%%% Types
%%%===================================================================

-record(trans_inv_client, {state     = fun 'Calling'/2 :: state(),
                           transport                  :: transport_type(),
                           options                    :: ersip:sip_options(),
                           request                    :: ersip_request:request(),
                           clear_reason = normal      :: ersip_trans_se:clear_reason(),
                           timer_a_timeout = 500      :: pos_integer()
                          }).
-type trans_inv_client() :: #trans_inv_client{}.
-type result() :: {trans_inv_client(), [ersip_trans_se:side_effect()]}.
-type transport_type() :: reliable | unreliable.
-type state()   :: fun((event(), trans_inv_client()) -> result()).
-type event()   :: term().
-type timer_type() :: term().

%%%===================================================================
%%% API
%%%===================================================================

-spec new(Reliable, Request, Options) -> result() when
      Reliable :: reliable | unreliable,
      Request  :: ersip_request:request(),
      Options  :: ersip:sip_options().
new(ReliableTranport, Request, Options) ->
    SipMsg = ersip_request:sipmsg(Request),
    case ersip_sipmsg:type(SipMsg) of
        request ->
            new_impl(ReliableTranport, Request, Options);
        response ->
            error({api_error, <<"client transaction must be initialyzed with INVITE request">>})
    end.

-spec event(Event, trans_inv_client()) -> result() when
      Event :: {timer, timer_type()}
             | {send, ersip_sipmsg:sipmsg()}.
event(_Event, #trans_inv_client{} = Trans) ->
    {Trans, []}.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-define(default_options,
        #{sip_t1 => 500,
          sip_t2 => 4000,
          sip_t4 => 5000
         }).
-define(T1(InvServerTrans), maps:get(sip_t1, InvServerTrans#trans_inv_client.options)).

-spec new_impl(Reliable, Request, Options) -> result() when
      Reliable :: reliable | unreliable,
      Request  :: ersip_request:request(),
      Options  :: ersip:sip_options().
new_impl(TransportType, Request, Options) ->
    %% The initial state, "calling", MUST be entered when the TU
    %% initiates a new client transaction with an INVITE request.  The
    %% client transaction MUST pass the request to the transport layer for
    %% transmission (see Section 18).
    Trans = #trans_inv_client{
               transport = TransportType,
               options   = maps:merge(?default_options, Options),
               request   = Request
              },
    Result0 = {Trans, [ersip_trans_se:send_request(Request)]},
    %% If an unreliable transport is being
    %% used, the client transaction MUST start timer A with a value of T1.
    Result1 =
        case TransportType of
            reliable ->
                Result0;
            unreliable ->
                Timeout = ?T1(Trans),
                {Trans1, SE1} = Result0,
                Trans2 = Trans1#trans_inv_client{timer_a_timeout = Timeout},
                set_timer_a(Timeout, {Trans2, SE1})
        end,
    %% For any transport, the client transaction MUST start timer B with a value
    %% of 64*T1 seconds (Timer B controls transaction timeouts).
    set_timer_b(64 * ?T1(Trans), Result1).

-spec 'Calling'(event(), trans_inv_client()) -> result().
'Calling'({timer, timer_a}, #trans_inv_client{request = Req, timer_a_timeout = TimerATimeout} = Trans) ->
    %% When timer A fires 2*T1 seconds later, the request MUST be
    %% retransmitted again (assuming the client transaction is still in this
    %% state).  This process MUST continue so that the request is
    %% retransmitted with intervals that double after each transmission.
    NewTimerATimeout = 2*TimerATimeout,
    Trans2 = Trans#trans_inv_client{timer_a_timeout = NewTimerATimeout},
    Result = {Trans2, [ersip_trans_se:send_request(Req)]},
    set_timer_a(NewTimerATimeout, Result);
'Calling'({timer, timer_b}, #trans_inv_client{} = Trans) ->
    %% If the client transaction is still in the "Calling" state when timer
    %% B fires, the client transaction SHOULD inform the TU that a timeout
    %% has occurred.
    terminate(timeout, Trans);
'Calling'({resp, provisional, SipMsg}, #trans_inv_client{} = Trans) ->
    %% If the client transaction receives a provisional response while
    %% in the "Calling" state, it transitions to the "Proceeding"
    %% state. In the "Proceeding" state, the client transaction SHOULD
    %% NOT retransmit the request any longer. Furthermore, the
    %% provisional response MUST be passed to the TU.
    Trans1 = set_state(fun 'Proceeding'/2, Trans),
    {Trans1, [ersip_trans_se:tu_result(SipMsg)]};
'Calling'(_Event, #trans_inv_client{} = Trans) ->

    {Trans, []}.

-spec 'Proceeding'(event(), trans_inv_client()) -> result().
'Proceeding'(_Event, #trans_inv_client{} = Trans) ->
    {Trans, []}.

-spec 'Completed'(event(), trans_inv_client()) -> result().
'Completed'(_Event, #trans_inv_client{} = Trans) ->
    {Trans, []}.

%% RFC6026 state:
-spec 'Accepted'(event(), trans_inv_client()) -> result().
'Accepted'(_Event, #trans_inv_client{} = Trans) ->
    {Trans, []}.

-spec 'Terminated'(event(), trans_inv_client()) -> result().
'Terminated'(_Event, #trans_inv_client{} = Trans) ->
    {Trans, []}.

-spec process_event(Event :: term(), result()) -> result().
process_event(Event, {#trans_inv_client{state = StateF} = Trans, SE}) ->
    {Trans1, EventSE} = StateF(Event, Trans),
    {Trans1, SE ++ EventSE}.

-spec set_state(state(), trans_inv_client()) -> trans_inv_client().
set_state(State, Trans) ->
    Trans#trans_inv_client{state = State}.

-spec set_timer_a(pos_integer(), result()) -> result().
set_timer_a(Timeout, Result) ->
    set_timer(Timeout, timer_a, Result).

-spec set_timer_b(pos_integer(), result()) -> result().
set_timer_b(Timeout, Result) ->
    set_timer(Timeout, timer_b, Result).

-spec set_timer(pos_integer(), TimerType, result()) -> result() when
      TimerType :: timer_type().
set_timer(Timeout, TimerType, {#trans_inv_client{} = Trans, SE}) ->
    {Trans, SE ++ [ersip_trans_se:set_timer(Timeout, {timer, TimerType})]}.

-spec terminate(Reason,  trans_inv_client()) -> result() when
      Reason :: ersip_trans_se:clear_reason().
terminate(Reason, Trans) ->
    Trans1 = set_state(fun 'Terminated'/2, Trans),
    Trans2 = Trans1#trans_inv_client{clear_reason = Reason},
    process_event('enter', {Trans2, []}).
