%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAC non-invite transaction
%%
%% Pure FSM implementation - transformation events to side effects.
%%

-module(ersip_uac).

-export([ new/4, event/2, clear_reason/1, id/1 ]).

-export_type([ uac/0, result/0, clear_reason/0 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type result() :: { uac(), [ ersip_uac_se:effect() ] }.
-type clear_reason() :: completed
                      | timeout.
-type request() :: term().

-record(uac, { id                           :: ersip_trans:tid(),
               state       = fun 'Trying'/2 :: non_inv_state(),
               request                      :: term(),
               options                      :: map(),
               reliable_transport           :: reliable | unreliable,
               timers     = #{}             :: #{ reference() => timer_type(),
                                                  timer_type() => reference() },
               timer_e_timeout = 500        :: pos_integer(),
               clear_reason    = undefined  :: clear_reason() | undefined
             }).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create new UAC transaction. Result of creation is UAC state
%% and set of side effects that produced because of creation.
%%
%% Request is not interpretted in any way.
-spec new(Id, Transport, Request, ersip:uac_options()) -> result() when
      Id        :: ersip_trans:tid(),
      Transport :: reliable | unreliable,
      Request   :: request().
new(Id, Transport, Request, Options) ->
    new_impl(Id, Transport, Request, Options).

%% @doc Process event by UAC.
%%
%% Function retuns new UAC state and side effects that must be done by
%% caller.
%%
%% Defined events:
%% { timer, TimerFun }         - timer alarm that was requested by previous side effect
%% { resp, RespType, message } - response with type is recieved by UAC.
%%
%% Side effects are defined in module ersip_uac_ser
%%
-spec event(Event, uac()) -> result() when
      Event :: { timer, TimerFun }
             | { resp, ersip_status:response_type(), term() },
      TimerFun :: fun((uac()) -> result()).
event({timer, TimerFun}, UAC) ->
    TimerFun(UAC);
event(Evt, UAC) ->
    process_event(Evt, UAC).

%% @doc Get tranaction ID.
-spec id(uac()) -> ersip_trans:tid().
id(#uac{ id = X }) ->
    X.

%% @doc Get transaction clear reason. It is guaranteed that after
%% clear_trans side effect tranaction has defined clear reason.
-spec clear_reason(uac()) -> clear_reason().
clear_reason(#uac{ clear_reason = X }) ->
    X.

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-type non_inv_state() :: fun((Event :: term(), uac()) -> result()).


-type timer_type() :: timer_f
                    | timer_e
                    | timer_k.

-type uac() :: #uac{}.

-define(default_options,
        #{ sip_t1    => 500,
           sip_t2    => 4000,
           sip_t4    => 5000
         }).

-define(T1(UAC), maps:get(sip_t1, UAC#uac.options)).
-define(T2(UAC), maps:get(sip_t2, UAC#uac.options)).
-define(T4(UAC), maps:get(sip_t4, UAC#uac.options)).

-spec new_impl(Id, Transport, Request, ersip:uac_options()) -> result() when
      Id        :: ersip_trans:tid(),
      Transport :: reliable | unreliable,
      Request   :: request().
new_impl(Id, ReliableTransport, Request, Options) ->
    UAC = #uac{ id      = Id,
                request = Request,
                options = maps:merge(?default_options, Options),
                reliable_transport = ReliableTransport,
                state   = fun 'Trying'/2
              },
    UAC1 = UAC#uac{ timer_e_timeout = ?T1(UAC) },
    { UAC2, SideEffects } =  process_event('enter', UAC1),
    { UAC2, [ ersip_uac_se:new_trans(UAC2) | SideEffects ] }.

%%
%% Trying state
%%
-spec 'Trying'(Event,   uac()) -> result() when
      Event :: enter
             | timer_f
             | timer_e
             | { resp, ersip_status:response_type(), Msg :: term() }.
'Trying'(enter, UAC) ->
    %% The "Trying" state is entered when the TU initiates a new client
    %% transaction with a request.  When entering this state, the client
    %% transaction SHOULD set timer F to fire in 64*T1 seconds.  The request
    %% MUST be passed to the transport layer for transmission.  If an
    %% unreliable transport is in use, the client transaction MUST set timer
    %% E to fire in T1 seconds.
    collect_side_effects(UAC,
                         [ fun send_request/1,
                           fun set_timer_f/1,
                           fun maybe_set_timer_e/1
                         ]);
'Trying'(timer_f, UAC) ->
    %% If Timer F fires while the client transaction is still in the
    %% "Trying" state, the client transaction SHOULD inform the TU about the
    %% timeout, and then it SHOULD enter the "Terminated" state.
    terminate(timeout, UAC);
'Trying'(timer_e, UAC) ->
    %% If timer E fires while still in this state,
    %% the timer is reset, but this time with a value of MIN(2*T1, T2).
    %% When the timer fires again, it is reset to a MIN(4*T1, T2).  This
    %% process continues so that retransmissions occur with an exponentially
    %% increasing interval that caps at T2.  The default value of T2 is 4s,
    %% and it represents the amount of time a non-INVITE server transaction
    %% will take to respond to a request, if it does not respond
    %% immediately.  For the default values of T1 and T2, this results in
    %% intervals of 500 ms, 1 s, 2 s, 4 s, 4 s, 4 s, etc.
    TimerETimeout = UAC#uac.timer_e_timeout * 2,
    Timeout = lists:min([ TimerETimeout, ?T2(UAC) ]),
    UAC1 = UAC#uac{ timer_e_timeout = Timeout },
    collect_side_effects(UAC1,
                         [ fun maybe_set_timer_e/1,
                           fun send_request/1
                         ]);
'Trying'({ resp, ResponseType, Msg }, UAC) ->
    case ResponseType of
        provisional ->
            %% If a provisional response is received while in
            %% the "Trying" state, the response MUST be passed
            %% to the TU, and then the client transaction
            %% SHOULD move to the "Proceeding" state.
            UAC1 = set_state(fun 'Proceeding'/2, UAC),
            { UAC2, SideEffects } = process_event(enter, UAC1),
            { UAC2, [ ersip_uac_se:tu_result(Msg, id(UAC)) | SideEffects ] };
        final ->
            %% If a final response (status codes 200-699) is
            %% received while in the "Trying" state, the
            %% response MUST be passed to the TU, and the
            %% client transaction MUST transition to the
            %% "Completed" state.
            UAC1 = set_state(fun 'Completed'/2, UAC),
            { UAC2, SideEffects } = process_event(enter, UAC1),
            { UAC2, [ ersip_uac_se:tu_result(Msg, id(UAC)) | SideEffects ] }
    end.

%%
%% Proceeding state
%%
-spec 'Proceeding'(Event,   uac()) -> result() when
      Event :: enter
             | timer_f
             | timer_e
             | { resp, ersip_status:response_type(), Msg :: term() }.

'Proceeding'(enter, UAC) ->
    %% Note: it is not clear from spec that we need to restrart timer
    %% E here but I think that it follows logic of retransmit. If we
    %% have received provisional response we need to cancel retransmit
    %% timer.
    UAC1 = UAC#uac{ timer_e_timeout = ?T2(UAC) },
    collect_side_effects(UAC1,
                         [ fun maybe_set_timer_e/1 ]);
'Proceeding'(timer_e, UAC) ->
    %% If Timer E fires while in the "Proceeding" state, the request
    %% MUST be passed to the transport layer for retransmission, and
    %% Timer E MUST be reset with a value of T2 seconds.
    UAC1 = UAC#uac{ timer_e_timeout = ?T2(UAC) },
    collect_side_effects(UAC1,
                         [ fun maybe_set_timer_e/1,
                           fun send_request/1
                         ]);
'Proceeding'(timer_f, UAC) ->
    %% If timer F fires while in the "Proceeding" state, the TU MUST
    %% be informed of a timeout, and the client transaction MUST
    %% transition to the terminated state.
    terminate(timeout, UAC);
'Proceeding'({ resp, ResponseType, Msg }, UAC) ->
    case ResponseType of
        provisional ->
            { UAC, [] };
        final ->
            %% If a final response (status codes 200-699) is
            %% received while in the "Proceeding" state, the
            %% response MUST be passed to the TU, and the
            %% client transaction MUST transition to the
            %% "Completed" state.
            UAC1 = set_state(fun 'Completed'/2, UAC),
            { UAC2, SideEffects } = process_event(enter, UAC1),
            { UAC2, [ ersip_uac_se:tu_result(Msg, id(UAC)) | SideEffects ] }
    end.

%%
%% Completed state
%%
-spec 'Completed'(Event,   uac()) -> result() when
      Event :: enter
             | timer_f
             | timer_e
             | timer_k
             | { resp, ersip_status:response_type(), Msg :: term() }.
'Completed'(enter, #uac{ reliable_transport = unreliable } = UAC) ->
    %% Once the client transaction enters the "Completed" state, it
    %% MUST set Timer K to fire in T4 seconds for unreliable
    %% transports, and zero seconds for reliable transports.
    collect_side_effects(UAC, [ fun set_timer_k/1 ]);
'Completed'(enter, #uac{ reliable_transport = reliable } = UAC) ->
    terminate(completed, UAC);
'Completed'({ resp, _, _ }, UAC) ->
    %% Just ignore response retransmission in 'Completed' state.q
    { UAC, [] };
'Completed'(timer_k, UAC) ->
    %% If Timer K fires while in this state, the client transaction
    %% MUST transition to the "Terminated" state.
    terminate(completed, UAC);
'Completed'(_, UAC) ->
    %%  Ignore other timers if any fired
    { UAC, [] }.

%%
%% Terminated state
%%
-spec 'Terminated'(Event,   uac()) -> result() when
      Event :: enter.
'Terminated'(enter, UAC) ->
    %% Once the transaction is in the terminated state, it MUST be
    %% destroyed immediately.
    { UAC, [ ersip_uac_se:clear_trans(UAC) ] }.

%%
%% Helpers
%%
-spec terminate(Reason,  uac()) -> result() when
      Reason :: timeout
              | completed.
terminate(Reason, UAC) ->
    UAC1 = set_state(fun 'Terminated'/2, UAC),
    UAC2 = UAC1#uac{ clear_reason = Reason },
    process_event('enter', UAC2).

-spec set_state(fun((Event :: term(), uac()) -> result()), uac()) -> uac().
set_state(State, UAC) ->
    UAC#uac{ state = State }.

-spec process_event(Event :: term(), uac()) -> result().
process_event(Event, #uac{ state = StateF } = UAC) ->
    StateF(Event, UAC).

-spec send_request(uac()) -> { ersip_uac_se:effect(), uac() }.
send_request(UAC) ->
    { ersip_uac_se:send(UAC#uac.request, id(UAC)), UAC }.

-spec maybe_set_timer_e(uac()) -> uac() | result().
maybe_set_timer_e(#uac{ reliable_transport = reliable } = UAC) ->
    UAC;
maybe_set_timer_e(#uac{ reliable_transport = unreliable } = UAC ) ->
    Timeout = UAC#uac.timer_e_timeout,
    set_timer(timer_e, Timeout, UAC).

-spec set_timer_f(uac()) -> { ersip_uac_se:effect(), uac() }.
set_timer_f(UAC) ->
    set_timer(timer_f, 64*?T1(UAC), UAC).

-spec set_timer_k(uac()) -> { ersip_uac_se:effect(), uac() }.
set_timer_k(#uac{ reliable_transport = unreliable } = UAC ) ->
    set_timer(timer_k, ?T4(UAC), UAC).

-spec set_timer(timer_type(), pos_integer(), uac()) -> { ersip_uac_se:effect(), uac() }.
set_timer(Type, Time, UAC) ->
    TimerRef = { Type, make_ref() },
    TimerFun = make_timer_fun(TimerRef),
    { ersip_uac_se:set_timer(Time, TimerFun, id(UAC)), add_timer(TimerRef, UAC) }.

-spec add_timer({ timer_type(), reference() }, uac()) -> uac().
add_timer({ TimerType, _ } = Ref, UAC) ->
    UAC1 = clear_timer(TimerType, UAC),
    Timers = UAC1#uac.timers,
    UAC1#uac{ timers = Timers#{ TimerType => Ref,
                                Ref => TimerType
                              } }.

-spec clear_timer(timer_type(), uac()) -> uac().
clear_timer(TimerType, #uac{ timers = Timers } = UAC) ->
    case maps:find(TimerType, Timers) of
        { ok, Ref } ->
            UAC#uac{ timers = maps:without([ TimerType, Ref ], Timers) };
        error ->
            UAC
    end.

-spec make_timer_fun(TimerVal) -> TimerFun when
      TimerFun :: fun((uac()) -> result()),
      TimerVal :: { timer_type(), reference() }.
make_timer_fun(TimerVal) ->
    fun(UAC) ->
            timer_fired(TimerVal, UAC)
    end.

-spec timer_fired({ timer_type(), reference() }, uac()) -> result().
timer_fired(Timer, #uac{ timers = Timers } = UAC) ->
    case maps:find(Timer, Timers) of
        error ->
            { UAC, [] };
        { ok, Type } ->
            UAC1 = clear_timer(Type, UAC),
            process_event(Type, UAC1)
    end.

-spec collect_side_effects(uac(), Funs) -> result() when
      Funs :: [   fun((uac()) -> uac())
                | fun((uac()) -> result())
              ].
collect_side_effects(UAC, Funs) ->
    Record = { UAC, [] },
    lists:foldl(fun (F, { #uac{} = AccState, AccActions }) ->
                        case F(AccState) of
                            #uac{} = UpdatedState ->
                                { UpdatedState, AccActions };
                            { Action, #uac{} = UpdatedState } ->
                                { UpdatedState, [ Action | AccActions ] }
                        end
                end,
                Record,
                Funs).

