%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Client non-INVITE transaction
%%
%% Pure FSM implementation - transformation events to side effects.
%%

-module(ersip_trans_client).

-export([new/3,
         event/2,
         clear_reason/1
        ]).

-export_type([trans_client/0,
              result/0,
              clear_reason/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type result() :: {trans_client(), [ersip_trans_se:effect()]}.
-type clear_reason() :: completed
                      | timeout.
-type request() :: term().

-record(trans_client, {state       = fun 'Trying'/2 :: non_inv_state(),
                       request                      :: term(),
                       options                      :: map(),
                       reliable_transport           :: reliable | unreliable,
                       timers     = #{}             :: #{reference() => timer_type(),
                                                         timer_type() => reference()},
                       timer_e_timeout = 500        :: pos_integer(),
                       clear_reason    = undefined  :: clear_reason() | undefined
                      }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create new client transaction. Result of creation is client
%% transaction state and set of side effects that produced because of
%% creation.
%%
%% Request is not interpretted in any way.
-spec new(Transport, Request, ersip:sip_options()) -> result() when
      Transport :: reliable | unreliable,
      Request   :: request().
new(Transport, Request, Options) ->
    new_impl(Transport, Request, Options).

%% @doc Process event by client transaction.
%%
%% Function retuns new client transaction state and side effects that
%% must be done by caller.
%%
%% Defined events:
%% {timer, TimerFun}         - timer alarm that was requested by previous side effect
%% {resp, RespType, message} - response with type is recieved by client transcation.
%%
%% Side effects are defined in module ersip_trans_se
%%
-spec event(Event, trans_client()) -> result() when
      Event :: {timer, TimerFun}
             | {resp, ersip_status:response_type(), term()},
      TimerFun :: fun((trans_client()) -> result()).
event({timer, TimerFun}, ClientTrans) ->
    TimerFun(ClientTrans);
event(Evt, ClientTrans) ->
    process_event(Evt, ClientTrans).

%% @doc Get transaction clear reason. It is guaranteed that after
%% clear_trans side effect tranaction has defined clear reason.
-spec clear_reason(trans_client()) -> clear_reason().
clear_reason(#trans_client{clear_reason = X}) ->
    X.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-type non_inv_state() :: fun((Event :: term(), trans_client()) -> result()).


-type timer_type() :: timer_f
                    | timer_e
                    | timer_k.

-type trans_client() :: #trans_client{}.

-define(default_options,
        #{sip_t1    => 500,
          sip_t2    => 4000,
          sip_t4    => 5000
         }).

-define(T1(ClientTrans), maps:get(sip_t1, ClientTrans#trans_client.options)).
-define(T2(ClientTrans), maps:get(sip_t2, ClientTrans#trans_client.options)).
-define(T4(ClientTrans), maps:get(sip_t4, ClientTrans#trans_client.options)).

-spec new_impl(Transport, Request, ersip:sip_options()) -> result() when
      Transport :: reliable | unreliable,
      Request   :: request().
new_impl(ReliableTransport, Request, Options) ->
    ClientTrans = #trans_client{request = Request,
                                options = maps:merge(?default_options, Options),
                                reliable_transport = ReliableTransport,
                                state   = fun 'Trying'/2
                               },
    ClientTrans1 = ClientTrans#trans_client{timer_e_timeout = ?T1(ClientTrans)},
    process_event('enter', ClientTrans1).

%%
%% Trying state
%%
-spec 'Trying'(Event,   trans_client()) -> result() when
      Event :: enter
             | timer_f
             | timer_e
             | {resp, ersip_status:response_type(), Msg :: term()}.
'Trying'(enter, ClientTrans) ->
    %% The "Trying" state is entered when the TU initiates a new client
    %% transaction with a request.  When entering this state, the client
    %% transaction SHOULD set timer F to fire in 64*T1 seconds.  The request
    %% MUST be passed to the transport layer for transmission.  If an
    %% unreliable transport is in use, the client transaction MUST set timer
    %% E to fire in T1 seconds.
    collect_side_effects(ClientTrans,
                         [fun send_request/1,
                          fun set_timer_f/1,
                          fun maybe_set_timer_e/1
                         ]);
'Trying'(timer_f, ClientTrans) ->
    %% If Timer F fires while the client transaction is still in the
    %% "Trying" state, the client transaction SHOULD inform the TU about the
    %% timeout, and then it SHOULD enter the "Terminated" state.
    terminate(timeout, ClientTrans);
'Trying'(timer_e, ClientTrans) ->
    %% If timer E fires while still in this state,
    %% the timer is reset, but this time with a value of MIN(2*T1, T2).
    %% When the timer fires again, it is reset to a MIN(4*T1, T2).  This
    %% process continues so that retransmissions occur with an exponentially
    %% increasing interval that caps at T2.  The default value of T2 is 4s,
    %% and it represents the amount of time a non-INVITE server transaction
    %% will take to respond to a request, if it does not respond
    %% immediately.  For the default values of T1 and T2, this results in
    %% intervals of 500 ms, 1 s, 2 s, 4 s, 4 s, 4 s, etc.
    TimerETimeout = ClientTrans#trans_client.timer_e_timeout * 2,
    Timeout = lists:min([TimerETimeout, ?T2(ClientTrans)]),
    ClientTrans1 = ClientTrans#trans_client{timer_e_timeout = Timeout},
    collect_side_effects(ClientTrans1,
                         [fun maybe_set_timer_e/1,
                          fun send_request/1
                         ]);
'Trying'({resp, ResponseType, Msg}, ClientTrans) ->
    case ResponseType of
        provisional ->
            %% If a provisional response is received while in
            %% the "Trying" state, the response MUST be passed
            %% to the TU, and then the client transaction
            %% SHOULD move to the "Proceeding" state.
            ClientTrans1 = set_state(fun 'Proceeding'/2, ClientTrans),
            {ClientTrans2, SideEffects} = process_event(enter, ClientTrans1),
            {ClientTrans2, [ersip_trans_se:tu_result(Msg) | SideEffects]};
        final ->
            %% If a final response (status codes 200-699) is
            %% received while in the "Trying" state, the
            %% response MUST be passed to the TU, and the
            %% client transaction MUST transition to the
            %% "Completed" state.
            ClientTrans1 = set_state(fun 'Completed'/2, ClientTrans),
            {ClientTrans2, SideEffects} = process_event(enter, ClientTrans1),
            {ClientTrans2, [ersip_trans_se:tu_result(Msg) | SideEffects]}
    end.

%%
%% Proceeding state
%%
-spec 'Proceeding'(Event,   trans_client()) -> result() when
      Event :: enter
             | timer_f
             | timer_e
             | {resp, ersip_status:response_type(), Msg :: term()}.

'Proceeding'(enter, ClientTrans) ->
    %% Note: it is not clear from spec that we need to restrart timer
    %% E here but I think that it follows logic of retransmit. If we
    %% have received provisional response we need to cancel retransmit
    %% timer.
    ClientTrans1 = ClientTrans#trans_client{timer_e_timeout = ?T2(ClientTrans)},
    collect_side_effects(ClientTrans1,
                         [fun maybe_set_timer_e/1]);
'Proceeding'(timer_e, ClientTrans) ->
    %% If Timer E fires while in the "Proceeding" state, the request
    %% MUST be passed to the transport layer for retransmission, and
    %% Timer E MUST be reset with a value of T2 seconds.
    ClientTrans1 = ClientTrans#trans_client{timer_e_timeout = ?T2(ClientTrans)},
    collect_side_effects(ClientTrans1,
                         [fun maybe_set_timer_e/1,
                          fun send_request/1
                         ]);
'Proceeding'(timer_f, ClientTrans) ->
    %% If timer F fires while in the "Proceeding" state, the TU MUST
    %% be informed of a timeout, and the client transaction MUST
    %% transition to the terminated state.
    terminate(timeout, ClientTrans);
'Proceeding'({resp, ResponseType, Msg}, ClientTrans) ->
    case ResponseType of
        provisional ->
            {ClientTrans, []};
        final ->
            %% If a final response (status codes 200-699) is
            %% received while in the "Proceeding" state, the
            %% response MUST be passed to the TU, and the
            %% client transaction MUST transition to the
            %% "Completed" state.
            ClientTrans1 = set_state(fun 'Completed'/2, ClientTrans),
            {ClientTrans2, SideEffects} = process_event(enter, ClientTrans1),
            {ClientTrans2, [ersip_trans_se:tu_result(Msg) | SideEffects]}
    end.

%%
%% Completed state
%%
-spec 'Completed'(Event,   trans_client()) -> result() when
      Event :: enter
             | timer_f
             | timer_e
             | timer_k
             | {resp, ersip_status:response_type(), Msg :: term()}.
'Completed'(enter, #trans_client{reliable_transport = unreliable} = ClientTrans) ->
    %% Once the client transaction enters the "Completed" state, it
    %% MUST set Timer K to fire in T4 seconds for unreliable
    %% transports, and zero seconds for reliable transports.
    collect_side_effects(ClientTrans, [fun set_timer_k/1]);
'Completed'(enter, #trans_client{reliable_transport = reliable} = ClientTrans) ->
    terminate(completed, ClientTrans);
'Completed'({resp, _, _}, ClientTrans) ->
    %% Just ignore response retransmission in 'Completed' state.q
    {ClientTrans, []};
'Completed'(timer_k, ClientTrans) ->
    %% If Timer K fires while in this state, the client transaction
    %% MUST transition to the "Terminated" state.
    terminate(completed, ClientTrans);
'Completed'(_, ClientTrans) ->
    %%  Ignore other timers if any fired
    {ClientTrans, []}.

%%
%% Terminated state
%%
-spec 'Terminated'(Event,   trans_client()) -> result() when
      Event :: enter.
'Terminated'(enter, ClientTrans) ->
    %% Once the transaction is in the terminated state, it MUST be
    %% destroyed immediately.
    {ClientTrans, [ersip_trans_se:clear_trans(ClientTrans)]}.

%%
%% Helpers
%%
-spec terminate(Reason,  trans_client()) -> result() when
      Reason :: timeout
              | completed.
terminate(Reason, ClientTrans) ->
    ClientTrans1 = set_state(fun 'Terminated'/2, ClientTrans),
    ClientTrans2 = ClientTrans1#trans_client{clear_reason = Reason},
    process_event('enter', ClientTrans2).

-spec set_state(fun((Event :: term(), trans_client()) -> result()), trans_client()) -> trans_client().
set_state(State, ClientTrans) ->
    ClientTrans#trans_client{state = State}.

-spec process_event(Event :: term(), trans_client()) -> result().
process_event(Event, #trans_client{state = StateF} = ClientTrans) ->
    StateF(Event, ClientTrans).

-spec send_request(trans_client()) -> {ersip_trans_se:effect(), trans_client()}.
send_request(ClientTrans) ->
    {ersip_trans_se:send(ClientTrans#trans_client.request), ClientTrans}.

-spec maybe_set_timer_e(trans_client()) -> trans_client() | result().
maybe_set_timer_e(#trans_client{reliable_transport = reliable} = ClientTrans) ->
    ClientTrans;
maybe_set_timer_e(#trans_client{reliable_transport = unreliable} = ClientTrans ) ->
    Timeout = ClientTrans#trans_client.timer_e_timeout,
    set_timer(timer_e, Timeout, ClientTrans).

-spec set_timer_f(trans_client()) -> {ersip_trans_se:effect(), trans_client()}.
set_timer_f(ClientTrans) ->
    set_timer(timer_f, 64*?T1(ClientTrans), ClientTrans).

-spec set_timer_k(trans_client()) -> {ersip_trans_se:effect(), trans_client()}.
set_timer_k(#trans_client{reliable_transport = unreliable} = ClientTrans ) ->
    set_timer(timer_k, ?T4(ClientTrans), ClientTrans).

-spec set_timer(timer_type(), pos_integer(), trans_client()) -> {ersip_trans_se:effect(), trans_client()}.
set_timer(Type, Time, ClientTrans) ->
    TimerRef = {Type, make_ref()},
    TimerFun = make_timer_fun(TimerRef),
    {ersip_trans_se:set_timer(Time, TimerFun), add_timer(TimerRef, ClientTrans)}.

-spec add_timer({timer_type(), reference()}, trans_client()) -> trans_client().
add_timer({TimerType, _} = Ref, ClientTrans) ->
    ClientTrans1 = clear_timer(TimerType, ClientTrans),
    Timers = ClientTrans1#trans_client.timers,
    ClientTrans1#trans_client{timers = Timers#{TimerType => Ref,
                                               Ref => TimerType
                                              }}.

-spec clear_timer(timer_type(), trans_client()) -> trans_client().
clear_timer(TimerType, #trans_client{timers = Timers} = ClientTrans) ->
    case maps:find(TimerType, Timers) of
        {ok, Ref} ->
            ClientTrans#trans_client{timers = maps:without([TimerType, Ref], Timers)};
        error ->
            ClientTrans
    end.

-spec make_timer_fun(TimerVal) -> TimerFun when
      TimerFun :: fun((trans_client()) -> result()),
      TimerVal :: {timer_type(), reference()}.
make_timer_fun(TimerVal) ->
    fun(ClientTrans) ->
            timer_fired(TimerVal, ClientTrans)
    end.

-spec timer_fired({timer_type(), reference()}, trans_client()) -> result().
timer_fired(Timer, #trans_client{timers = Timers} = ClientTrans) ->
    case maps:find(Timer, Timers) of
        error ->
            {ClientTrans, []};
        {ok, Type} ->
            ClientTrans1 = clear_timer(Type, ClientTrans),
            process_event(Type, ClientTrans1)
    end.

-spec collect_side_effects(trans_client(), Funs) -> result() when
      Funs :: [  fun((trans_client()) -> trans_client())
                 | fun((trans_client()) -> result())
              ].
collect_side_effects(ClientTrans, Funs) ->
    Record = {ClientTrans, []},
    lists:foldl(fun (F, {#trans_client{} = AccState, AccActions}) ->
                        case F(AccState) of
                            #trans_client{} = UpdatedState ->
                                {UpdatedState, AccActions};
                            {Action, #trans_client{} = UpdatedState} ->
                                {UpdatedState, [Action | AccActions]}
                        end
                end,
                Record,
                Funs).

