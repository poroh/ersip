%%
%% Copyright (c) 2019 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Subscription-State tests
%%

-module(ersip_hdr_subscription_state_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

-define(crlf, "\r\n").

rebuild_test() ->
    rebuild(<<"terminated;reason=noresource">>),
    rebuild(<<"terminated;reason=giveup">>),
    rebuild(<<"terminated;reason=deactivated">>),
    rebuild(<<"terminated;reason=probation">>),
    rebuild(<<"terminated;reason=rejected">>),
    rebuild(<<"terminated;reason=invariant">>),
    rebuild(<<"terminated;reason=x-some-my-reason">>),
    rebuild(<<"active;expires=599">>),
    rebuild(<<"terminated;reason=timeout">>),
    rebuild(<<"active">>),
    rebuild(<<"terminated">>),
    rebuild(<<"pending">>),
    rebuild(<<"pending;expires=3600">>),
    rebuild(<<"Active; Expires=60">>),
    rebuild(<<"pending;unknown-prameter=\"very important\"">>),
    ok.

parse_error_test() ->
    parse_error(<<"terminated;reason=\"@\"">>),
    parse_error(<<"terminated;reason=@">>),
    parse_error(<<"terminated;expires=-1">>),
    parse_error(<<"terminated;retry-after=-1">>),
    parse_error(<<"terminated;retry-after=40,*">>),
    parse_error(<<"terminated;a@b=40">>),
    parse_error(<<"terminated;expires=40aaa">>),
    parse_error(<<"terminated@x;expires=40aaa">>),
    parse_error(<<"terminated;reason=\"a@b\"">>),
    parse_error(<<"terminated;?=$">>),
    parse_error(<<"terminated;?">>),
    parse_error(<<"terminated;;;;">>),
    ok.

parse_test() ->
    ?assertEqual(active,     value(<<"active;expires=30">>)),
    ?assertEqual(terminated, value(<<"terminated">>)),
    ?assertEqual({unknown, <<"my-state">>}, value(<<"my-state">>)),

    ?assertEqual(30, expires(<<"active;expires=30">>)),
    ?assertEqual(0,  expires(<<"active;expires=0">>)),
    ?assertEqual(599, expires(<<"active;expires=599">>)),
    ?assertEqual(599, expires(<<"active;  expires=599">>)),
    ?assertEqual(599, expires(<<"active;  expires = 599">>)),
    ?assertEqual(599, expires(<<"active;  expires = 599;my-param=1">>)),
    ?assertEqual(599, expires(<<"active;  my-param=1; expires = 599">>)),
    ?assertEqual(599, expires(<<"active;  my-param=1; expires = 599; my-param-2=2">>)),
    ?assertEqual(undefined, expires(<<"active">>)),

    ?assertEqual(undefined, retry_after(<<"terminated0">>)),
    ?assertEqual(30,  retry_after(<<"terminated;retry-after=30">>)),
    ?assertEqual(30,  retry_after(<<"terminated;a=b;retry-after=30;c=d">>)),

    ?assertEqual(timeout,   event_reason_value(<<"terminated;reason=timeout">>)),
    ?assertEqual(undefined, event_reason_value(<<"terminated">>)),

    ?assertEqual(<<"timeout">>, param(<<"reason">>, <<"terminated;reason=timeout">>)),
    ?assertEqual(<<"1">>, param(<<"my-param">>, <<"terminated;my-param=1">>)),
    ok.

make_error_test() ->
    ?assertError({invalid_subscription_state, _}, ersip_hdr_subscription_state:make(<<"terminated;reason=@">>)),
    ?assertError({invalid_subscription_state, _}, ersip_hdr_subscription_state:make(<<"terminated;@=1">>)),
    ok.

set_param_test() ->
    SubsStateReason = ersip_hdr_subscription_state:make(<<"terminated;reason=deactivated">>),
    SubsStateReasonReject = ersip_hdr_subscription_state:set_param(<<"reason">>, <<"rejected">>, SubsStateReason),
    ?assertEqual(rejected, ersip_hdr_subscription_state:event_reason_value(SubsStateReasonReject, undefined)),

    SubsStateExp = ersip_hdr_subscription_state:make(<<"active;expires=55">>),
    SubsStateExpUp = ersip_hdr_subscription_state:set_param(<<"expires">>, <<"99">>, SubsStateExp),
    ?assertEqual(99, ersip_hdr_subscription_state:expires(SubsStateExpUp, undefined)),

    ok.

set_param_error_test() ->
    SubsState = ersip_hdr_subscription_state:make(<<"terminated;reason=deactivated">>),
    ?assertError({invalid_event_reason, _},  ersip_hdr_subscription_state:set_param(<<"reason">>, <<"@">>, SubsState)),
    ?assertError({invalid_event_reason, _},  ersip_hdr_subscription_state:set_param(<<"reason">>, <<"a@b">>, SubsState)),
    ?assertError({invalid_event_expires, _}, ersip_hdr_subscription_state:set_param(<<"expires">>, <<"-1">>, SubsState)),
    ?assertError({invalid_param, _}, ersip_hdr_subscription_state:set_param(<<"@">>, <<"Value">>, SubsState)),
    ?assertError({invalid_param, _}, ersip_hdr_subscription_state:set_param(<<"@">>, <<>>, SubsState)),
    ok.


build_test() ->
    SubsStateH = create(<<"terminated;reason=noresource">>),
    {ok, SubsState} = ersip_hdr_subscription_state:parse(SubsStateH),
    SubsStateHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(SubsStateH)],
    BuiltSubsStateH = ersip_hdr_subscription_state:build(<<"Subscription-State">>, SubsState),
    BuiltSubsStateHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(BuiltSubsStateH)],
    ?assertEqual(SubsStateHValues, BuiltSubsStateHValues),

    EmptyH = ersip_hdr:new(<<"Subscription-State">>),
    ?assertEqual({error, no_subsciption_state}, ersip_hdr_subscription_state:parse(EmptyH)),
    ok.

assemble_bin_test() ->
    SubsState = ersip_hdr_subscription_state:make(<<"terminated;reason=deactivated">>),
    ?assertEqual(<<"terminated;reason=deactivated">>, ersip_hdr_subscription_state:assemble_bin(SubsState)),
    ok.

required_in_notify_test() ->
    Notify =
        << "NOTIFY sip:app.example.com SIP/2.0" ?crlf
           "Via: SIP/2.0/UDP server19.example.com;branch=z9hG4bKnasaii" ?crlf
           "From: sip:joe@example.com;tag=xyzygg" ?crlf
           "To: sip:app.example.com;tag=123aa9" ?crlf
           "Call-ID: 9987@app.example.com" ?crlf
           "CSeq: 1288 NOTIFY" ?crlf
           "Contact: sip:server19.example.com" ?crlf
           "Event: reg" ?crlf
           "Subscription-State: active;expires=3600" ?crlf
           "Max-Forwards: 70" ?crlf
           "Content-Type: application/reginfo+xml" ?crlf
           "" ?crlf
           "<?xml version=\"1.0\"?>" ?crlf
           "<reginfo xmlns=\"urn:ietf:params:xml:ns:reginfo\"" ?crlf
           "version=\"0\" state=\"full\">" ?crlf
           "<registration aor=\"sip:joe@example.com\" id=\"a7\" state=\"init\"/>" ?crlf
           "</reginfo>" ?crlf
           "">>,
    {ok, SipMsgWEvent} = ersip_sipmsg:parse(Notify, [subscription_state]),
    SubsState = ersip_sipmsg:get(subscription_state, SipMsgWEvent),
    ?assertEqual(active, ersip_hdr_subscription_state:value(SubsState)),

    BadNotify =
        << "NOTIFY sip:app.example.com SIP/2.0" ?crlf
           "Via: SIP/2.0/UDP server19.example.com;branch=z9hG4bKnasaii" ?crlf
           "From: sip:joe@example.com;tag=xyzygg" ?crlf
           "To: sip:app.example.com;tag=123aa9" ?crlf
           "Call-ID: 9987@app.example.com" ?crlf
           "CSeq: 1288 NOTIFY" ?crlf
           "Contact: sip:server19.example.com" ?crlf
           "Max-Forwards: 70" ?crlf
           "Event: reg" ?crlf
           "Content-Type: application/reginfo+xml" ?crlf
           "" ?crlf
           "<?xml version=\"1.0\"?>" ?crlf
           "<reginfo xmlns=\"urn:ietf:params:xml:ns:reginfo\"" ?crlf
           "version=\"0\" state=\"full\">" ?crlf
           "<registration aor=\"sip:joe@example.com\" id=\"a7\" state=\"init\"/>" ?crlf
           "</reginfo>" ?crlf
           "">>,
    ?assertMatch({error, {header_error, {subscription_state, _}}}, ersip_sipmsg:parse(BadNotify, [subscription_state])),
    ok.


%%===================================================================
%% Helpers
%%===================================================================
create(Bin) ->
    H = ersip_hdr:new(<<"Subscription-State">>),
    ersip_hdr:add_value(Bin, H).

rebuild(Bin) ->
    SubsState = ersip_hdr_subscription_state:make(Bin),
    SubsStateBin = iolist_to_binary(ersip_hdr_subscription_state:assemble(SubsState)),
    {ok, SubsState1} = ersip_hdr_subscription_state:parse(SubsStateBin),
    ?assertEqual(SubsState, SubsState1).

parse_error(Bin) ->
    ?assertMatch({error, {invalid_subscription_state, _}}, ersip_hdr_subscription_state:parse(Bin)).

value(Bin) ->
    SubsState = ersip_hdr_subscription_state:make(Bin),
    ersip_hdr_subscription_state:value(SubsState).

expires(Bin) ->
    SubsState = ersip_hdr_subscription_state:make(Bin),
    ersip_hdr_subscription_state:expires(SubsState, undefined).

retry_after(Bin) ->
    SubsState = ersip_hdr_subscription_state:make(Bin),
    ersip_hdr_subscription_state:retry_after(SubsState, undefined).

event_reason_value(Bin) ->
    SubsState = ersip_hdr_subscription_state:make(Bin),
    ersip_hdr_subscription_state:event_reason_value(SubsState, undefined).

param(ParamName, Bin) ->
    SubsState = ersip_hdr_subscription_state:make(Bin),
    {ok, Value} = ersip_hdr_subscription_state:param(ParamName, SubsState),
    Value.
