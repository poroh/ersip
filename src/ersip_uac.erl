%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAC non-invite transaction
%%

-module(ersip_uac).

-export([ new/3 ]).

-type result() :: { ok, uac(), [ ersip_uac_se:effect() ] }
                | { error, term() }.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Create new UAC transaction. Result of creation is UAC state
%% and set of side effects that produced because of creation.
%% Side effects are chained.
-spec new(IsReliableTransport :: boolean(), ersip:message(), ersip:uac_options()) -> result().
new(IsReliableTransport, Request, Options) ->
    new_impl(IsReliableTransport, Request, Options).

%%%===================================================================
%%% Internal implementation
%%%===================================================================

-type non_inv_state() :: fun((Event :: term(), uac()) -> result()).

-record(uac, { state       = fun 'Trying'/2 :: non_inv_state(),
               request                      :: ersip:message(),
               options                      :: map(),
               raw_message = undefined      :: iolist()        | undefined,
               reliable_transport           :: boolean(),
               timers     = #{}             :: #{ reference() => timer_type() },
               timer_e_timeout   = 500      :: pos_integer()
             }).

-type timer_type() :: timer_f
                    | timer_e.

-type uac() :: #uac{}.

-define(default_options,
        #{ sip_t1    => 500,
           sip_t2    => 4000,
           sip_t4    => 5000
         }).

-define(T1(UAC), maps:get(sip_t1, UAC#uac.options)).
-define(T2(UAC), maps:get(sip_t2, UAC#uac.options)).
-define(T4(UAC), maps:get(sip_t4, UAC#uac.options)).

-spec new_impl(IsReliableTransport :: boolean(), ersip:message(), ersip:uac_options()) -> result().
new_impl(IsReliableTransport, Request, Options) ->
    case check_request(Request) of
        ok ->
            UAC = #uac{ request = Request,
                        options = maps:merge(?default_options, Options),
                        reliable_transport = IsReliableTransport,
                        state   = fun 'Trying'/2
                      },
            UAC1 = UAC#uac{ timer_e_timeout = ?T1(UAC) },
            { ok, UAC2, SideEffects } =  process_event('enter', UAC1),
            { ok, UAC2, [ ersip_uac_se:new_trans(UAC2) | SideEffects ] };
        { error, _ } = Error ->
            Error
    end.

%%
%% Trying state
%%
-spec 'Trying'(Event,   uac()) -> result() when
      Event :: enter
             | timer_f
             | timer_e
             | { recv, ersip:message() }
             | { send_result, ok | { error, term() } }.
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
'Trying'({ send_result, ok }, UAC) ->
    { ok, UAC, [] };
'Trying'({ send_result, { error, Error } }, UAC) ->
    UAC1 = set_state(fun 'Terminated'/2, UAC),
    { ok, UAC2, SideEffects } =  process_event('enter', UAC1),
    { ok, UAC2, [ ersip_uac_se:tu_result({ error, { transport_error, Error } }) | SideEffects ] };
'Trying'({ recv, Msg }, UAC) ->
    case ersip_msg:get(type, Msg) of
        response ->
            case response_class(ersip_msg:get(status, Msg)) of
                provisional ->
                    %% If a provisional response is received while in
                    %% the "Trying" state, the response MUST be passed
                    %% to the TU, and then the client transaction
                    %% SHOULD move to the "Proceeding" state.
                    UAC1 = set_state(fun 'Proceeding'/2, UAC),
                    { ok, UAC2, SideEffects } = process_event(enter, UAC1),
                    { ok, UAC2, [ ersip_uac_se:tu_result(Msg) | SideEffects ] };
                final ->
                    %% If a final response (status codes 200-699) is
                    %% received while in the "Trying" state, the
                    %% response MUST be passed to the TU, and the
                    %% client transaction MUST transition to the
                    %% "Completed" state.
                    UAC1 = set_state(fun 'Completed'/2, UAC),
                    { ok, UAC2, SideEffects } = process_event(enter, UAC1),
                    { ok, UAC2, [ ersip_uac_se:tu_result(Msg) | SideEffects ] }
            end;
        _ ->
            { ok, UAC, [] }
    end.

%%
%% Proceeding state
%%
-spec 'Proceeding'(Event,   uac()) -> result() when
      Event :: enter
             | timer_f
             | timer_e
             | { recv, ersip:message() }
             | { send_result, ok | { error, term() } }.

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
'Proceeding'({ recv, Msg }, UAC) ->
    case ersip_msg:get(type, Msg) of
        response ->
            case response_class(ersip_msg:get(status, Msg)) of
                provisional ->
                    { ok, UAC, [] };
                final ->
                    %% If a final response (status codes 200-699) is
                    %% received while in the "Proceeding" state, the
                    %% response MUST be passed to the TU, and the
                    %% client transaction MUST transition to the
                    %% "Completed" state.
                    UAC1 = set_state(fun 'Completed'/2, UAC),
                    { ok, UAC2, SideEffects } = process_event(enter, UAC1),
                    { ok, UAC2, [ ersip_uac_se:tu_result(Msg) | SideEffects ] }
            end;
        _ ->
            { ok, UAC, [] }
    end;
'Proceeding'({ send_result, _ }, UAC) ->
    { ok, UAC, [] }.

%%
%% Completed state
%%
-spec 'Completed'(Event,   uac()) -> result() when
      Event :: enter
             | timer_f
             | timer_e
             | timer_k
             | { recv, ersip:message() }.
'Completed'(enter, #uac{ reliable_transport = false } = UAC) ->
    %% Once the client transaction enters the "Completed" state, it
    %% MUST set Timer K to fire in T4 seconds for unreliable
    %% transports, and zero seconds for reliable transports.
    collect_side_effects(UAC, [ fun set_timer_k/1 ]);
'Completed'(enter, #uac{ reliable_transport = true } = UAC) ->
    terminate(completed, UAC);
'Completed'({ recv, _ }, UAC) ->
    %% Just ignore response retransmission in 'Completed' state.q
    { ok, UAC, [] };
'Completed'(timer_k, UAC) ->
    %% If Timer K fires while in this state, the client transaction
    %% MUST transition to the "Terminated" state.
    terminate(completed, UAC);
'Completed'(_, UAC) ->
    %%  Ignore other timers if any fired
    { ok, UAC, [] }.

%%
%% Terminated state
%%
-spec 'Terminated'(Event,   uac()) -> result() when
      Event :: enter.
'Terminated'(enter, UAC) ->
    %% Once the transaction is in the terminated state, it MUST be
    %% destroyed immediately.
    { ok, UAC, [ ersip_uac_se:clear_trans(UAC) ] }.

%%
%% Helpers
%%
-spec terminate(Reason,  uac()) -> result() when
      Reason :: timeout
              | completed.
terminate(Reason, UAC) ->
    UAC1 = set_state(fun 'Terminated'/2, UAC),
    { ok, UAC2, SideEffects } =  process_event('enter', UAC1),
    { ok, UAC2, [ ersip_uac_se:tu_result(Reason) | SideEffects ] }.

-spec set_state(fun((Event :: term(), uac()) -> result()), uac()) -> uac().
set_state(State, UAC) ->
    UAC#uac{ state = State }.

process_event(Event, #uac{ state = StateF } = UAC) ->
    StateF(Event, UAC).

timer_fired(Timer, #uac{ timers = Timers } = UAC) ->
    case maps:find(Timer, Timers) of
        error ->
            { ok, UAC, [] };
        { Type, _ } ->
            UAC1 = clear_timer(Type, UAC),
            process_event(Type, UAC1)
    end.

send_request(UAC) ->
    RawRequest =
        case UAC#uac.raw_message of
            undefined ->
                ersip_msg:serialize(UAC#uac.request);
            Msg ->
                Msg
        end,
    SendResultFun =
        fun(Result) ->
                process_event({ send_result, Result }, UAC)
        end,
    { ersip_uac_se:send(RawRequest, SendResultFun),
      UAC#uac{ raw_message = RawRequest } }.

set_timer_f(UAC) ->
    set_timer(timer_f, 64*?T1(UAC), UAC).

maybe_set_timer_e(#uac{ reliable_transport = true } = UAC) ->
    UAC;
maybe_set_timer_e(#uac{ reliable_transport = false } = UAC ) ->
    Timeout = UAC#uac.timer_e_timeout,
    set_timer(timer_e, Timeout, UAC).

set_timer_k(#uac{ reliable_transport = false } = UAC ) ->
    set_timer(timer_k, ?T4(UAC), UAC).

add_timer({ TimerType, _ } = Ref, UAC) ->
    UAC1 = clear_timer(TimerType, UAC),
    Timers = UAC1#uac.timers,
    UAC1#uac{ timers = Timers#{ TimerType => Ref,
                                Ref => TimerType
                              } }.

set_timer(Type, Time, UAC) ->
    TimerRef = { Type, make_ref() },
    TimerFun = make_timer_fun(TimerRef, UAC),
    { ersip_uac_se:set_timer(TimerFun, Time), add_timer(TimerRef, UAC) }.

clear_timer(TimerType, #uac{ timers = Timers } = UAC) ->
    case maps:find(TimerType, Timers) of
        { ok, Ref } ->
            UAC#uac{ timers = maps:without([ TimerType, Ref ], Timers) };
        error ->
            UAC
    end.

collect_side_effects(UAC, Funs) ->
    case collect_side_effects_impl(UAC, Funs) of
        { #uac{} = UAC, SideEffects } ->
            { ok, UAC, SideEffects };
        Error ->
            Error
    end.

collect_side_effects_impl(UAC, Funs) ->
    Record = { UAC, [] },
    lists:foldl(fun (F, { #uac{} = AccState, AccActions }) ->
                        case F(AccState) of
                            #uac{} = UpdatedState ->
                                { UpdatedState, AccActions };
                            { #uac{} = UpdatedState, Action } ->
                                { UpdatedState, [ Action | AccActions ] };
                            Error ->
                                Error
                        end
                end,
                Record,
                Funs).

check_request(Request) ->
    %% A valid SIP request formulated by a UAC MUST, at a minimum, contain
    %% the following header fields: To, From, CSeq, Call-ID, Max-Forwards,
    %% and Via;
    lists:foldl(fun(Item, ok) -> check_item(Item, Request);
                   (_, Error) -> Error end,
                [ type, method, ruri,
                  <<"to">>, <<"from">>, <<"cseq">>, <<"call-id">>, <<"max-forwards">>,
                  <<"via">> ]).

check_item(type, Request) ->
    case ersip_msg:get(type, Request) of
        request -> ok;
        _ ->
            { error, request_message_expected }
    end;
check_item(Item, Request) ->
    case ersip_msg:get(Item, Request) of
        undefined ->
            { error, { not_specified, Item } };
        Item ->
            check_value(Item)
    end.

check_value(_) ->
    %% TODO: check headers value here
    ok.

make_timer_fun(TimerVal, UAC) ->
    fun() ->
            timer_fired(TimerVal, UAC)
    end.

response_class(Code) when is_integer(Code) ->
    case Code div 100 of
        1 ->
            provisional;
        _ ->
            final
    end.
