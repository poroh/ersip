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

expires_modification_test() ->
    SubsState0 = make(<<"active;expires=30">>),
    ?assertEqual(30, ersip_hdr_subscription_state:expires(SubsState0)),
    SubsState1 = ersip_hdr_subscription_state:set_expires(20, SubsState0),
    ?assertEqual(20, ersip_hdr_subscription_state:expires(SubsState1)),
    ?assertEqual(20, expires(assemble(SubsState1))),

    SubsState2 = make(<<"active">>),
    ?assertEqual(undefined, ersip_hdr_subscription_state:expires(SubsState2)),
    SubsState3 = ersip_hdr_subscription_state:set_expires(99, SubsState2),
    ?assertEqual(99, expires(assemble(SubsState3))),
    ok.

retry_after_modification_test() ->
    SubsState0 = make(<<"terminated;retry-after=30">>),
    ?assertEqual(30, ersip_hdr_subscription_state:retry_after(SubsState0)),
    SubsState1 = ersip_hdr_subscription_state:set_retry_after(20, SubsState0),
    ?assertEqual(20, ersip_hdr_subscription_state:retry_after(SubsState1)),
    ?assertEqual(20, retry_after(assemble(SubsState1))),

    SubsState2 = make(<<"terminated">>),
    ?assertEqual(undefined, ersip_hdr_subscription_state:retry_after(SubsState2)),
    SubsState3 = ersip_hdr_subscription_state:set_retry_after(99, SubsState2),
    ?assertEqual(99, retry_after(assemble(SubsState3))),
    ok.

reason_modification_test() ->
    SubsState0 = make(<<"terminated;reason=deactivated">>),
    ?assertEqual(deactivated, ersip_hdr_subscription_state:event_reason_value(SubsState0)),
    SubsState1 = ersip_hdr_subscription_state:set_event_reason_value(timeout, SubsState0),
    ?assertEqual(timeout, ersip_hdr_subscription_state:event_reason_value(SubsState1)),
    ?assertEqual(timeout, event_reason_value(assemble(SubsState1))),

    SubsState2 = make(<<"terminated">>),
    SubsState3 = ersip_hdr_subscription_state:set_event_reason_value(<<"noresource">>, SubsState2),
    ?assertEqual(noresource, event_reason_value(assemble(SubsState3))),

    SubsState4 = make(<<"terminated">>),
    ?assertEqual(undefined, ersip_hdr_subscription_state:event_reason_value(SubsState4)),
    SubsState5 = ersip_hdr_subscription_state:set_event_reason_value(<<"unknown">>, SubsState4),
    ?assertEqual({unknown_reason, <<"unknown">>}, event_reason_value(assemble(SubsState5))),

    SubsState6 = ersip_hdr_subscription_state:set_event_reason_value({unknown_reason, <<"new">>}, SubsState5),
    ?assertEqual({unknown_reason, <<"new">>}, event_reason_value(assemble(SubsState6))),

    SubsState7 = ersip_hdr_subscription_state:set_event_reason_value(<<"new">>, SubsState5),
    ?assertEqual({unknown_reason, <<"new">>}, event_reason_value(assemble(SubsState7))),

    SubsState8 = ersip_hdr_subscription_state:set_event_reason_value(<<"noresource">>, SubsState5),
    ?assertEqual(noresource, event_reason_value(assemble(SubsState8))),
    ok.


raw_test() ->
    ?assertMatch(#{value := <<"terminated">>},  ersip_hdr_subscription_state:raw(make(<<"terminated">>))),
    ?assertMatch(#{value := <<"active">>},      ersip_hdr_subscription_state:raw(make(<<"active;expires=10">>))),
    ?assertMatch(#{expires := 10},              ersip_hdr_subscription_state:raw(make(<<"active;expires=10">>))),
    ?assertMatch(#{retry_after := 10},          ersip_hdr_subscription_state:raw(make(<<"terminated;retry-after=10">>))),
    ?assertMatch(#{reason := <<"noresource">>}, ersip_hdr_subscription_state:raw(make(<<"terminated;reason=noresource;retry-after=10">>))),
    ?assertMatch(#{reason := <<"unknown">>},    ersip_hdr_subscription_state:raw(make(<<"terminated;reason=unknown;retry-after=10">>))),
    ?assertMatch(#{value := <<"unknown">>},     ersip_hdr_subscription_state:raw(make(<<"unknown">>))),
    ok.

make_from_raw_test() ->
    ?assertEqual(<<"terminated">>, assemble(ersip_hdr_subscription_state:make(#{value => <<"terminated">>}))),
    ?assertEqual(<<"active;expires=30">>, assemble(ersip_hdr_subscription_state:make(#{value => <<"active">>, params => #{<<"expires">> => <<"30">>}}))),
    ?assertEqual(30, ersip_hdr_subscription_state:expires(make(#{value => <<"active">>, params => #{<<"expires">> => <<"30">>}}))),
    ?assertEqual(<<"active;expires=30">>, assemble(ersip_hdr_subscription_state:make(#{value => <<"active">>, expires => 30}))),
    ?assertEqual(<<"active;unknown=1">>, assemble(ersip_hdr_subscription_state:make(#{value => <<"active">>, params => #{<<"unknown">> => <<"1">>}}))),
    ?assertEqual(<<"active;unknown">>, assemble(ersip_hdr_subscription_state:make(#{value => <<"active">>, params => #{<<"unknown">> => <<>>}}))),

    ?assertEqual(<<"terminated;retry-after=50">>, assemble(ersip_hdr_subscription_state:make(#{value => <<"terminated">>, retry_after => 50}))),
    ?assertEqual(<<"terminated;reason=noresource">>, assemble(ersip_hdr_subscription_state:make(#{value => <<"terminated">>, reason => <<"noresource">>}))),
    ?assertEqual(<<"terminated;reason=unknown">>, assemble(ersip_hdr_subscription_state:make(#{value => <<"terminated">>, reason => <<"unknown">>}))),

    ?assertError({invalid_params, _}, assemble(ersip_hdr_subscription_state:make(#{value => <<"active">>, params => #{<<"expires">> => <<"@">>}}))),
    ?assertError({invalid_params, _}, assemble(ersip_hdr_subscription_state:make(#{value => <<"terminated">>, params => #{<<"retry-after">> => <<"-1">>}}))),
    ok.


%%===================================================================
%% Helpers
%%===================================================================

make(Src) ->
    ersip_hdr_subscription_state:make(Src).

assemble(SubsState) ->
    ersip_hdr_subscription_state:assemble_bin(SubsState).

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
