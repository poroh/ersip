%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Server non-INVITE transaction test
%%

-module(ersip_trans_server_test).

-include_lib("eunit/include/eunit.hrl").

uas_reliable_test() ->
    {UAS0, SideEffects0} = ersip_trans_server:new(reliable, message, #{}),
    SideEffectsMap0 = maps:from_list(SideEffects0),
    ?assertMatch(message, maps:get(tu_result, SideEffectsMap0)),

    %% Branch 1: (Retransmit before provisional response is ignored)
    {UAS0, []} = ersip_trans_server:event(retransmit, UAS0),

    %% Branch 2: (Sending provisional response)
    {UAS_2_0, SideEffects_2_0} = ersip_trans_server:event({send_resp, provisional, resp_msg_prov}, UAS0),
    SideEffectsMap_2_0 = maps:from_list(SideEffects_2_0),
    %% Response with provisional resp:
    ?assertMatch(resp_msg_prov, maps:get(send, SideEffectsMap_2_0)),

    %% Branch 2.1: (Retransmit after provisional response)
    {UAS_2_1_0, SideEffects_2_1_0} = ersip_trans_server:event(retransmit, UAS_2_0),
    SideEffectsMap_2_1_0 = maps:from_list(SideEffects_2_1_0),
    %% Last provisional response sent
    ?assertMatch(resp_msg_prov, maps:get(send, SideEffectsMap_2_1_0)),
    %% Send one more provisional response:
    {UAS_2_1_1, SideEffects_2_1_1} = ersip_trans_server:event({send_resp, provisional, resp_msg_prov_1}, UAS_2_1_0),
    SideEffectsMap_2_1_1 = maps:from_list(SideEffects_2_1_1),
    ?assertMatch(resp_msg_prov_1, maps:get(send, SideEffectsMap_2_1_1)),
    {_, SideEffects_2_1_2} = ersip_trans_server:event(retransmit, UAS_2_1_1),
    SideEffectsMap_2_1_2 = maps:from_list(SideEffects_2_1_2),
    %% Last provisional response sent
    ?assertMatch(resp_msg_prov_1, maps:get(send, SideEffectsMap_2_1_2)),

    %% Branch 2.2: (Final after provisional response)
    {_UAS_2_2_0, SideEffects_2_2_0} = ersip_trans_server:event({send_resp, final, resp_msg_final}, UAS_2_0),
    SideEffectsMap_2_2_0 = maps:from_list(SideEffects_2_2_0),
    %% Final response is sent
    ?assertMatch(resp_msg_final, maps:get(send, SideEffectsMap_2_2_0)),
    %% Transaction is cleared
    ?assertMatch(unknown, maps:get(clear_trans, SideEffectsMap_2_2_0)),

    %% Branch 3: (Sending final response without provisional)
    {_UAS_3_0, SideEffects_3_0} = ersip_trans_server:event({send_resp, final, resp_msg_final}, UAS0),
    SideEffectsMap_3_0 = maps:from_list(SideEffects_3_0),
    %% Response with provisional resp:
    ?assertMatch(resp_msg_final, maps:get(send, SideEffectsMap_3_0)),
    %% Transaction is cleared
    ?assertMatch(unknown, maps:get(clear_trans, SideEffectsMap_3_0)).

uas_unreliable_test() ->
    {UAS0, SideEffects0} = ersip_trans_server:new(unreliable, message, #{}),
    SideEffectsMap0 = maps:from_list(SideEffects0),
    ?assertMatch(message, maps:get(tu_result, SideEffectsMap0)),

    %% Branch 1: (Sending final response without provisional)
    {UAS_1_0, SideEffects_1_0} = ersip_trans_server:event({send_resp, final, resp_msg_final}, UAS0),
    SideEffectsMap_1_0 = maps:from_list(SideEffects_1_0),
    %% Response with provisional resp:
    ?assertMatch(resp_msg_final, maps:get(send, SideEffectsMap_1_0)),
    %% Timer J is set:
    ?assertMatch({32000, timer_j}, maps:get(set_timer, SideEffectsMap_1_0)),
    %% Retransmits cause send last response:
    {UAS_1_1, SideEffects_1_1} = ersip_trans_server:event(retransmit, UAS_1_0),
    SideEffectsMap_1_1 = maps:from_list(SideEffects_1_1),
    ?assertMatch(resp_msg_final, maps:get(send, SideEffectsMap_1_1)),
    %% Any other final responses passed by the TU to the server
    %% transaction MUST be discarded while in the "Completed" state.
    {UAS_1_1, []} = ersip_trans_server:event({send_resp, final, more_final}, UAS_1_1),
    %% Timer J fired => clear transaction
    {_UAS_1_2, SideEffects_1_2} = ersip_trans_server:event({timer, timer_j}, UAS_1_1),
    SideEffectsMap_1_2 = maps:from_list(SideEffects_1_2),
    %% Transaction is cleared
    ?assertMatch(unknown, maps:get(clear_trans, SideEffectsMap_1_2)).
