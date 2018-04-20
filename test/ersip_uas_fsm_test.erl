%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAS test
%%

-module(ersip_uas_fsm_test).

-include_lib("eunit/include/eunit.hrl").

uas_reliable_test() ->
    Tid = <<"transaction id">>,
    {UAS0, SideEffects0} = ersip_uas_fsm:new(Tid, reliable, message, #{}),
    SideEffectsMap0 = maps:from_list(SideEffects0),
    %% New transaction is created:
    ?assertMatch([UAS0], maps:get(new_trans, SideEffectsMap0)),
    ?assertMatch([message, Tid], maps:get(tu_result, SideEffectsMap0)),
    %% ID is set:
    ?assertEqual(Tid, ersip_uas_fsm:id(UAS0)),

    %% Branch 1: (Retransmit before provisional response is ignored)
    {UAS0, []} = ersip_uas_fsm:event(retransmit, UAS0),

    %% Branch 2: (Sending provisional response)
    {UAS_2_0, SideEffects_2_0} = ersip_uas_fsm:event({send_resp, provisional, resp_msg_prov}, UAS0),
    SideEffectsMap_2_0 = maps:from_list(SideEffects_2_0),
    %% Response with provisional resp:
    ?assertMatch([resp_msg_prov, Tid], maps:get(send, SideEffectsMap_2_0)),

    %% Branch 2.1: (Retransmit after provisional response)
    {UAS_2_1_0, SideEffects_2_1_0} = ersip_uas_fsm:event(retransmit, UAS_2_0),
    SideEffectsMap_2_1_0 = maps:from_list(SideEffects_2_1_0),
    %% Last provisional response sent
    ?assertMatch([resp_msg_prov, Tid], maps:get(send, SideEffectsMap_2_1_0)),
    %% Send one more provisional response:
    {UAS_2_1_1, SideEffects_2_1_1} = ersip_uas_fsm:event({send_resp, provisional, resp_msg_prov_1}, UAS_2_1_0),
    SideEffectsMap_2_1_1 = maps:from_list(SideEffects_2_1_1),
    ?assertMatch([resp_msg_prov_1, Tid], maps:get(send, SideEffectsMap_2_1_1)),
    {_, SideEffects_2_1_2} = ersip_uas_fsm:event(retransmit, UAS_2_1_1),
    SideEffectsMap_2_1_2 = maps:from_list(SideEffects_2_1_2),
    %% Last provisional response sent
    ?assertMatch([resp_msg_prov_1, Tid], maps:get(send, SideEffectsMap_2_1_2)),

    %% Branch 2.2: (Final after provisional response)
    {UAS_2_2_0, SideEffects_2_2_0} = ersip_uas_fsm:event({send_resp, final, resp_msg_final}, UAS_2_0),
    SideEffectsMap_2_2_0 = maps:from_list(SideEffects_2_2_0),
    %% Final response is sent
    ?assertMatch([resp_msg_final, Tid], maps:get(send, SideEffectsMap_2_2_0)),
    %% Transaction is cleared
    ?assertMatch([UAS_2_2_0], maps:get(clear_trans, SideEffectsMap_2_2_0)),

    %% Branch 3: (Sending final response without provisional)
    {UAS_3_0, SideEffects_3_0} = ersip_uas_fsm:event({send_resp, final, resp_msg_final}, UAS0),
    SideEffectsMap_3_0 = maps:from_list(SideEffects_3_0),
    %% Response with provisional resp:
    ?assertMatch([resp_msg_final, Tid], maps:get(send, SideEffectsMap_3_0)),
    %% Transaction is cleared
    ?assertMatch([UAS_3_0], maps:get(clear_trans, SideEffectsMap_3_0)).

uas_unreliable_test() ->
    Tid = <<"unreliable transaction id">>,
    {UAS0, SideEffects0} = ersip_uas_fsm:new(Tid, unreliable, message, #{}),
    SideEffectsMap0 = maps:from_list(SideEffects0),
    %% New transaction is created:
    ?assertMatch([UAS0], maps:get(new_trans, SideEffectsMap0)),
    ?assertMatch([message, Tid], maps:get(tu_result, SideEffectsMap0)),
    %% ID is set:
    ?assertEqual(Tid, ersip_uas_fsm:id(UAS0)),

    %% Branch 1: (Sending final response without provisional)
    {UAS_1_0, SideEffects_1_0} = ersip_uas_fsm:event({send_resp, final, resp_msg_final}, UAS0),
    SideEffectsMap_1_0 = maps:from_list(SideEffects_1_0),
    %% Response with provisional resp:
    ?assertMatch([resp_msg_final, Tid], maps:get(send, SideEffectsMap_1_0)),
    %% Timer J is set:
    ?assertMatch([32000, timer_j, Tid], maps:get(set_timer, SideEffectsMap_1_0)),
    %% Retransmits cause send last response:
    {UAS_1_1, SideEffects_1_1} = ersip_uas_fsm:event(retransmit, UAS_1_0),
    SideEffectsMap_1_1 = maps:from_list(SideEffects_1_1),
    ?assertMatch([resp_msg_final, Tid], maps:get(send, SideEffectsMap_1_1)),
    %% Any other final responses passed by the TU to the server
    %% transaction MUST be discarded while in the "Completed" state.
    {UAS_1_1, []} = ersip_uas_fsm:event({send_resp, final, more_final}, UAS_1_1),
    %% Timer J fired => clear transaction
    {UAS_1_2, SideEffects_1_2} = ersip_uas_fsm:event({timer, timer_j}, UAS_1_1),
    SideEffectsMap_1_2 = maps:from_list(SideEffects_1_2),
    %% Transaction is cleared
    ?assertMatch([UAS_1_2], maps:get(clear_trans, SideEffectsMap_1_2)).
