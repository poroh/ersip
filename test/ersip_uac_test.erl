%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAC test
%%

-module(ersip_uac_test).

-include_lib("eunit/include/eunit.hrl").

uac_reliable_test() ->
    Tid = <<"transaction id">>,
    { UAC0, SideEffects0 } = ersip_uac:new(Tid, reliable, message, #{}),
    SideEffectsMap0 = maps:from_list(SideEffects0),
    %% New transaction is created:
    ?assertMatch([UAC0], maps:get(new_trans, SideEffectsMap0)),
    %% Message is sent
    ?assertMatch([message, Tid], maps:get(send, SideEffectsMap0)),
    %% Transaction timer is set:
    ?assertMatch([32000,_, Tid], maps:get(set_timer, SideEffectsMap0)),
    [_,TransactionTimer, Tid] = maps:get(set_timer, SideEffectsMap0),
    %% ID is set:
    ?assertEqual(Tid, ersip_uac:id(UAC0)),

    %% Branch 1: (No reply at all)
    %% Check transaction timer fired:
    { UAC1, SideEffects_1_0 } = ersip_uac:event({timer, TransactionTimer}, UAC0),
    SideEffectsMap_1_0 = maps:from_list(SideEffects_1_0),
    %% Transaction is cleared
    ?assertMatch([UAC1], maps:get(clear_trans, SideEffectsMap_1_0)),
    ?assertEqual(timeout, ersip_uac:clear_reason(UAC1)),

    %% Branch 2: (Provisional response and no reply after):
    { UAC_2_0,SideEffects_2_0 } = ersip_uac:event({ resp, provisional, resp_msg }, UAC0),
    %% Provisional response is passed to transaction user:
    SideEffectsMap_2_0 = maps:from_list(SideEffects_2_0),
    ?assertMatch([resp_msg, Tid], maps:get(tu_result, SideEffectsMap_2_0)),
    %% Transaction timer is fired
    { UAC_2_1, SideEffects_2_1 } = ersip_uac:event({timer, TransactionTimer}, UAC_2_0),
    SideEffectsMap_2_1 = maps:from_list(SideEffects_2_1),
    %% Transaction is cleared
    ?assertMatch([UAC_2_1], maps:get(clear_trans, SideEffectsMap_2_1)),
    ?assertEqual(timeout, ersip_uac:clear_reason(UAC_2_1)),

    %% Branch 3: (Two provisional responses and no final reply):
    { UAC_3_0, _SideEffects_3_0 } = ersip_uac:event({ resp, provisional, resp_msg }, UAC0),
    { UAC_3_1,  SideEffects_3_1 } = ersip_uac:event({ resp, provisional, resp_msg }, UAC_3_0),
    %% Second provisional response is NOT passed to transaction user:
    SideEffectsMap_3_1 = maps:from_list(SideEffects_3_1),
    ?assertEqual(undefined, maps:get(tu_result, SideEffectsMap_3_1, undefined)),
    %% Transaction timer is fired
    { UAC_3_2, SideEffects_3_2 } = ersip_uac:event({timer, TransactionTimer}, UAC_3_1),
    SideEffectsMap_3_2 = maps:from_list(SideEffects_3_2),
    %% Transaction is cleared
    ?assertMatch([UAC_3_2], maps:get(clear_trans, SideEffectsMap_3_2)),
    ?assertEqual(timeout, ersip_uac:clear_reason(UAC_3_2)),

    %% Branch 4: Final reply without provisional response:
    { UAC_4_0, SideEffects_4_0 } = ersip_uac:event({ resp, final, resp_msg }, UAC0),
    SideEffectsMap_4_0 = maps:from_list(SideEffects_4_0),
    %% Final response is passed to transaction user:
    ?assertMatch([resp_msg, Tid], maps:get(tu_result, SideEffectsMap_4_0)),
    %% Transaction is cleared
    ?assertMatch([UAC_4_0], maps:get(clear_trans, SideEffectsMap_4_0)),
    ?assertEqual(completed, ersip_uac:clear_reason(UAC_4_0)),

    %% Branch 5: Final reply after provisional response:
    { UAC_5_0, _SideEffects_5_0 } = ersip_uac:event({ resp, provisional, resp_msg }, UAC0),
    { UAC_5_1, SideEffects_5_1 } = ersip_uac:event({ resp, final, resp_msg }, UAC_5_0),
    SideEffectsMap_5_1 = maps:from_list(SideEffects_5_1),
    %% Final response is passed to transaction user:
    ?assertMatch([resp_msg, Tid], maps:get(tu_result, SideEffectsMap_5_1)),
    %% Transaction is cleared
    ?assertMatch([UAC_5_1], maps:get(clear_trans, SideEffectsMap_5_1)),
    ?assertEqual(completed, ersip_uac:clear_reason(UAC_5_1)).

uac_unreliable_test() ->
    Tid = <<"transaction unrel id">>,
    { UAC0, SideEffects0 } = ersip_uac:new(Tid, unreliable, message, #{}),
    SideEffectsMap0 = maps:from_list(SideEffects0),
    %% New transaction is created:
    ?assertMatch([UAC0], maps:get(new_trans, SideEffectsMap0)),
    %% Message is sent
    ?assertMatch([message, Tid], maps:get(send, SideEffectsMap0)),
    %% Transaction timer is set:
    [ [500,   RetransmitTimer, Tid],
      [32000, TransactionTimer, Tid]
    ] = lists:sort(proplists:get_all_values(set_timer, SideEffects0)),
    %% ID is set:
    ?assertEqual(Tid, ersip_uac:id(UAC0)),

    %% --------------------
    %% Branch 1: (No reply at all)
    %% Retransmits #1
    { UAC_1_0, SideEffects_1_0 } = ersip_uac:event({timer, RetransmitTimer}, UAC0),
    SideEffectsMap_1_0 = maps:from_list(SideEffects_1_0),
    ?assertMatch([message, Tid], maps:get(send, SideEffectsMap_1_0)),
    ?assertMatch([1000,_, Tid], maps:get(set_timer, SideEffectsMap_1_0)),
    [_,RetransmitTimer_1_0, Tid] = maps:get(set_timer, SideEffectsMap_1_0),
    %% Retransmits #2
    { UAC_1_1, SideEffects_1_1 } = ersip_uac:event({timer, RetransmitTimer_1_0}, UAC_1_0),
    SideEffectsMap_1_1 = maps:from_list(SideEffects_1_1),
    ?assertMatch([message, Tid], maps:get(send, SideEffectsMap_1_1)),
    ?assertMatch([2000,_, Tid], maps:get(set_timer, SideEffectsMap_1_1)),
    %% Check transaction timer fired:
    { UAC_1_2, SideEffects_1_2 } = ersip_uac:event({timer, TransactionTimer}, UAC_1_1),
    SideEffectsMap_1_2 = maps:from_list(SideEffects_1_2),
    %% Transaction is cleared
    ?assertMatch([UAC_1_2], maps:get(clear_trans, SideEffectsMap_1_2)),
    ?assertEqual(timeout, ersip_uac:clear_reason(UAC_1_2)),

    %% --------------------
    %% Branch 2: (Provisional response and no reply after):
    { UAC_2_0,SideEffects_2_0 } = ersip_uac:event({ resp, provisional, resp_msg }, UAC0),
    %% Provisional response is passed to transaction user:
    SideEffectsMap_2_0 = maps:from_list(SideEffects_2_0),
    ?assertMatch([resp_msg, Tid], maps:get(tu_result, SideEffectsMap_2_0)),
    %% Here we assume that UAC cancels timer E and restart it to T2xs
    ?assertMatch([4000,_, Tid], maps:get(set_timer, SideEffectsMap_2_0)),
    [_,RetransmitTimer_2_0, Tid] = maps:get(set_timer, SideEffectsMap_2_0),

    %% Retransmits after provisional response:
    { UAC_2_1, SideEffects_2_1 } = ersip_uac:event({timer, RetransmitTimer_2_0}, UAC_2_0),
    SideEffectsMap_2_1 = maps:from_list(SideEffects_2_1),
    ?assertMatch([message, Tid], maps:get(send, SideEffectsMap_2_1)),
    ?assertMatch([4000,_, Tid], maps:get(set_timer, SideEffectsMap_2_1)),
    [_,RetransmitTimer_2_1, Tid] = maps:get(set_timer, SideEffectsMap_2_1),

    %% Retransmit #2
    { UAC_2_2, SideEffects_2_2 } = ersip_uac:event({timer, RetransmitTimer_2_1}, UAC_2_1),
    SideEffectsMap_2_2 = maps:from_list(SideEffects_2_2),
    ?assertMatch([message, Tid], maps:get(send, SideEffectsMap_2_2)),
    ?assertMatch([4000,_, Tid], maps:get(set_timer, SideEffectsMap_2_2)),

    %% Transaction timer is fired
    { UAC_2_3, SideEffects_2_3 } = ersip_uac:event({timer, TransactionTimer}, UAC_2_2),
    SideEffectsMap_2_3 = maps:from_list(SideEffects_2_3),
    %% Transaction is cleared
    ?assertMatch([UAC_2_3], maps:get(clear_trans, SideEffectsMap_2_3)),
    ?assertEqual(timeout, ersip_uac:clear_reason(UAC_2_3)),

    %% --------------------
    %% Branch 3: (Two provisional responses and no final reply):
    %% Note: do not check retransmit logic here...
    { UAC_3_0, _SideEffects_3_0 } = ersip_uac:event({ resp, provisional, resp_msg }, UAC0),
    { UAC_3_1,  SideEffects_3_1 } = ersip_uac:event({ resp, provisional, resp_msg }, UAC_3_0),
    %% Second provisional response is NOT passed to transaction user:
    SideEffectsMap_3_1 = maps:from_list(SideEffects_3_1),
    ?assertEqual(undefined, maps:get(tu_result, SideEffectsMap_3_1, undefined)),
    %% Transaction timer is fired
    { UAC_3_2, SideEffects_3_2 } = ersip_uac:event({timer, TransactionTimer}, UAC_3_1),
    SideEffectsMap_3_2 = maps:from_list(SideEffects_3_2),
    %% Transaction is cleared
    ?assertMatch([UAC_3_2], maps:get(clear_trans, SideEffectsMap_3_2)),
    ?assertEqual(timeout, ersip_uac:clear_reason(UAC_3_2)),

    %% --------------------
    %% Branch 4: Final reply without provisional response:
    { UAC_4_0, SideEffects_4_0 } = ersip_uac:event({ resp, final, resp_msg }, UAC0),
    SideEffectsMap_4_0 = maps:from_list(SideEffects_4_0),
    %% Final response is passed to transaction user:
    ?assertMatch([resp_msg, Tid], maps:get(tu_result, SideEffectsMap_4_0)),
    %% Timer K is set...
    ?assertMatch([5000, _, Tid], maps:get(set_timer, SideEffectsMap_4_0)),
    [_, TimerK_4_0, Tid] = maps:get(set_timer, SideEffectsMap_4_0),

    %% TimerK retransmission of response in terminate state:
    { UAC_4_1, SideEffects_4_1 } = ersip_uac:event({ resp, final, resp_msg }, UAC_4_0),
    SideEffectsMap_4_1 = maps:from_list(SideEffects_4_1),
    ?assertEqual(undefined, maps:get(tu_result, SideEffectsMap_4_1, undefined)),

    %% TimerK fired => transaction terminated
    { UAC_4_2, SideEffects_4_2 } = ersip_uac:event({timer, TimerK_4_0}, UAC_4_1),
    SideEffectsMap_4_2 = maps:from_list(SideEffects_4_2),
    ?assertMatch([UAC_4_2], maps:get(clear_trans, SideEffectsMap_4_2)),
    ?assertEqual(completed, ersip_uac:clear_reason(UAC_4_2)),

    %% --------------------
    %% Branch 5: Final reply after provisional response:
    { UAC_5_0, _SideEffects_5_0 } = ersip_uac:event({ resp, provisional, resp_msg }, UAC0),
    { UAC_5_1, SideEffects_5_1 } = ersip_uac:event({ resp, final, resp_msg }, UAC_5_0),
    SideEffectsMap_5_1 = maps:from_list(SideEffects_5_1),
    %% Final response is passed to transaction user:
    ?assertMatch([resp_msg, Tid], maps:get(tu_result, SideEffectsMap_5_1)),
    ?assertMatch([5000, _, Tid], maps:get(set_timer, SideEffectsMap_5_1)),
    [_, TimerK_5_1, Tid] = maps:get(set_timer, SideEffectsMap_5_1),

    %% Timer E/F ignored in Completed state:
    { UAC_5_2, [] } = ersip_uac:event({ timer, RetransmitTimer }, UAC_5_1),
    { UAC_5_3, [] } = ersip_uac:event({ timer, TransactionTimer }, UAC_5_2),

    %% TimerK fired => transaction terminated
    { UAC_5_4, SideEffects_5_4 } = ersip_uac:event({timer, TimerK_5_1}, UAC_5_3),
    SideEffectsMap_5_4 = maps:from_list(SideEffects_5_4),
    ?assertMatch([UAC_5_4], maps:get(clear_trans, SideEffectsMap_5_4)),
    ?assertEqual(completed, ersip_uac:clear_reason(UAC_5_4)).
