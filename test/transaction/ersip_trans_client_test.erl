%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Client non-INVITE transaction test
%%

-module(ersip_trans_client_test).

-include_lib("eunit/include/eunit.hrl").

-dialyzer({nowarn_function, uac_reliable_test/0}).
uac_reliable_test() ->
    %%TODO: broken typing for request()
    {ClientTrans0, SideEffects0} = ersip_trans_client:new(reliable, message, #{}),
    SideEffectsMap0 = maps:from_list(SideEffects0),
    %% Message is sent
    ?assertMatch(message, maps:get(send_request, SideEffectsMap0)),
    %% Transaction timer is set:
    ?assertMatch({32000, _}, maps:get(set_timer, SideEffectsMap0)),
    {_, TransactionTimer} = maps:get(set_timer, SideEffectsMap0),

    %% Branch 1: (No reply at all)
    %% Check transaction timer fired:
    {ClientTrans1, SideEffects_1_0} = ersip_trans_client:event(TransactionTimer, ClientTrans0),
    SideEffectsMap_1_0 = maps:from_list(SideEffects_1_0),
    %% Transaction is cleared
    ?assertMatch(timeout, maps:get(clear_trans, SideEffectsMap_1_0)),
    ?assertEqual(timeout, ersip_trans_client:clear_reason(ClientTrans1)),

    %% Branch 2: (Provisional response and no reply after):
    {ClientTrans_2_0,SideEffects_2_0} = ersip_trans_client:event({resp, provisional, resp_msg}, ClientTrans0),
    %% Provisional response is passed to transaction user:
    SideEffectsMap_2_0 = maps:from_list(SideEffects_2_0),
    ?assertMatch(resp_msg, maps:get(tu_result, SideEffectsMap_2_0)),
    %% Transaction timer is fired
    {ClientTrans_2_1, SideEffects_2_1} = ersip_trans_client:event(TransactionTimer, ClientTrans_2_0),
    SideEffectsMap_2_1 = maps:from_list(SideEffects_2_1),
    %% Transaction is cleared
    ?assertMatch(timeout, maps:get(clear_trans, SideEffectsMap_2_1)),
    ?assertEqual(timeout, ersip_trans_client:clear_reason(ClientTrans_2_1)),

    %% Branch 3: (Two provisional responses and no final reply):
    {ClientTrans_3_0, _SideEffects_3_0} = ersip_trans_client:event({resp, provisional, resp_msg}, ClientTrans0),
    {ClientTrans_3_1,  SideEffects_3_1} = ersip_trans_client:event({resp, provisional, resp_msg}, ClientTrans_3_0),
    %% Second provisional response is NOT passed to transaction user:
    SideEffectsMap_3_1 = maps:from_list(SideEffects_3_1),
    ?assertEqual(undefined, maps:get(tu_result, SideEffectsMap_3_1, undefined)),
    %% Transaction timer is fired
    {ClientTrans_3_2, SideEffects_3_2} = ersip_trans_client:event(TransactionTimer, ClientTrans_3_1),
    SideEffectsMap_3_2 = maps:from_list(SideEffects_3_2),
    %% Transaction is cleared
    ?assertMatch(timeout, maps:get(clear_trans, SideEffectsMap_3_2)),
    ?assertEqual(timeout, ersip_trans_client:clear_reason(ClientTrans_3_2)),

    %% Branch 4: Final reply without provisional response:
    {ClientTrans_4_0, SideEffects_4_0} = ersip_trans_client:event({resp, final, resp_msg}, ClientTrans0),
    SideEffectsMap_4_0 = maps:from_list(SideEffects_4_0),
    %% Final response is passed to transaction user:
    ?assertMatch(resp_msg, maps:get(tu_result, SideEffectsMap_4_0)),
    %% Transaction is cleared
    ?assertMatch(normal, maps:get(clear_trans, SideEffectsMap_4_0)),
    ?assertEqual(normal, ersip_trans_client:clear_reason(ClientTrans_4_0)),

    %% Branch 5: Final reply after provisional response:
    {ClientTrans_5_0, _SideEffects_5_0} = ersip_trans_client:event({resp, provisional, resp_msg}, ClientTrans0),
    {ClientTrans_5_1, SideEffects_5_1} = ersip_trans_client:event({resp, final, resp_msg}, ClientTrans_5_0),
    SideEffectsMap_5_1 = maps:from_list(SideEffects_5_1),
    %% Final response is passed to transaction user:
    ?assertMatch(resp_msg, maps:get(tu_result, SideEffectsMap_5_1)),
    %% Transaction is cleared
    ?assertMatch(normal, maps:get(clear_trans, SideEffectsMap_5_1)),
    ?assertEqual(normal, ersip_trans_client:clear_reason(ClientTrans_5_1)).

-dialyzer({nowarn_function, uac_unreliable_test/0}).
uac_unreliable_test() ->
    %%TODO: broken typing for request()
    {ClientTrans0, SideEffects0} = ersip_trans_client:new(unreliable, message, #{}),
    SideEffectsMap0 = maps:from_list(SideEffects0),
    %% Message is sent
    ?assertMatch(message, maps:get(send_request, SideEffectsMap0)),
    %% Transaction timer is set:
    [{500,   RetransmitTimer},
     {32000, TransactionTimer}
    ] = lists:sort(proplists:get_all_values(set_timer, SideEffects0)),

    %% --------------------
    %% Branch 1: (No reply at all)
    %% Retransmits #1
    {ClientTrans_1_0, SideEffects_1_0} = ersip_trans_client:event(RetransmitTimer, ClientTrans0),
    SideEffectsMap_1_0 = maps:from_list(SideEffects_1_0),
    ?assertMatch(message, maps:get(send_request, SideEffectsMap_1_0)),
    ?assertMatch({1000, _}, maps:get(set_timer, SideEffectsMap_1_0)),
    {_, RetransmitTimer_1_0} = maps:get(set_timer, SideEffectsMap_1_0),
    %% Retransmits #2
    {ClientTrans_1_1, SideEffects_1_1} = ersip_trans_client:event(RetransmitTimer_1_0, ClientTrans_1_0),
    SideEffectsMap_1_1 = maps:from_list(SideEffects_1_1),
    ?assertMatch(message, maps:get(send_request, SideEffectsMap_1_1)),
    ?assertMatch({2000, _}, maps:get(set_timer, SideEffectsMap_1_1)),
    %% Check transaction timer fired:
    {ClientTrans_1_2, SideEffects_1_2} = ersip_trans_client:event(TransactionTimer, ClientTrans_1_1),
    SideEffectsMap_1_2 = maps:from_list(SideEffects_1_2),
    %% Transaction is cleared
    ?assertMatch(timeout, maps:get(clear_trans, SideEffectsMap_1_2)),
    ?assertEqual(timeout, ersip_trans_client:clear_reason(ClientTrans_1_2)),

    %% --------------------
    %% Branch 2: (Provisional response and no reply after):
    {ClientTrans_2_0,SideEffects_2_0} = ersip_trans_client:event({resp, provisional, resp_msg}, ClientTrans0),
    %% Provisional response is passed to transaction user:
    SideEffectsMap_2_0 = maps:from_list(SideEffects_2_0),
    ?assertMatch(resp_msg, maps:get(tu_result, SideEffectsMap_2_0)),
    %% Here we assume that ClientTrans cancels timer E and restart it to T2xs
    ?assertMatch({4000, _}, maps:get(set_timer, SideEffectsMap_2_0)),
    {_, RetransmitTimer_2_0} = maps:get(set_timer, SideEffectsMap_2_0),

    %% Retransmits after provisional response:
    {ClientTrans_2_1, SideEffects_2_1} = ersip_trans_client:event(RetransmitTimer_2_0, ClientTrans_2_0),
    SideEffectsMap_2_1 = maps:from_list(SideEffects_2_1),
    ?assertMatch(message, maps:get(send_request, SideEffectsMap_2_1)),
    ?assertMatch({4000, _}, maps:get(set_timer, SideEffectsMap_2_1)),
    {_, RetransmitTimer_2_1} = maps:get(set_timer, SideEffectsMap_2_1),

    %% Retransmit #2
    {ClientTrans_2_2, SideEffects_2_2} = ersip_trans_client:event(RetransmitTimer_2_1, ClientTrans_2_1),
    SideEffectsMap_2_2 = maps:from_list(SideEffects_2_2),
    ?assertMatch(message, maps:get(send_request, SideEffectsMap_2_2)),
    ?assertMatch({4000, _}, maps:get(set_timer, SideEffectsMap_2_2)),

    %% Transaction timer is fired
    {ClientTrans_2_3, SideEffects_2_3} = ersip_trans_client:event(TransactionTimer, ClientTrans_2_2),
    SideEffectsMap_2_3 = maps:from_list(SideEffects_2_3),
    %% Transaction is cleared
    ?assertMatch(timeout, maps:get(clear_trans, SideEffectsMap_2_3)),
    ?assertEqual(timeout, ersip_trans_client:clear_reason(ClientTrans_2_3)),

    %% --------------------
    %% Branch 3: (Two provisional responses and no final reply):
    %% Note: do not check retransmit logic here...
    {ClientTrans_3_0, _SideEffects_3_0} = ersip_trans_client:event({resp, provisional, resp_msg}, ClientTrans0),
    {ClientTrans_3_1,  SideEffects_3_1} = ersip_trans_client:event({resp, provisional, resp_msg}, ClientTrans_3_0),
    %% Second provisional response is NOT passed to transaction user:
    SideEffectsMap_3_1 = maps:from_list(SideEffects_3_1),
    ?assertEqual(undefined, maps:get(tu_result, SideEffectsMap_3_1, undefined)),
    %% Transaction timer is fired
    {ClientTrans_3_2, SideEffects_3_2} = ersip_trans_client:event(TransactionTimer, ClientTrans_3_1),
    SideEffectsMap_3_2 = maps:from_list(SideEffects_3_2),
    %% Transaction is cleared
    ?assertMatch(timeout, maps:get(clear_trans, SideEffectsMap_3_2)),
    ?assertEqual(timeout, ersip_trans_client:clear_reason(ClientTrans_3_2)),

    %% --------------------
    %% Branch 4: Final reply without provisional response:
    {ClientTrans_4_0, SideEffects_4_0} = ersip_trans_client:event({resp, final, resp_msg}, ClientTrans0),
    SideEffectsMap_4_0 = maps:from_list(SideEffects_4_0),
    %% Final response is passed to transaction user:
    ?assertMatch(resp_msg, maps:get(tu_result, SideEffectsMap_4_0)),
    %% Timer K is set...
    ?assertMatch({5000, _}, maps:get(set_timer, SideEffectsMap_4_0)),
    {_, TimerK_4_0} = maps:get(set_timer, SideEffectsMap_4_0),

    %% TimerK retransmission of response in terminate state:
    {ClientTrans_4_1, SideEffects_4_1} = ersip_trans_client:event({resp, final, resp_msg}, ClientTrans_4_0),
    SideEffectsMap_4_1 = maps:from_list(SideEffects_4_1),
    ?assertEqual(undefined, maps:get(tu_result, SideEffectsMap_4_1, undefined)),

    %% TimerK fired => transaction terminated
    {ClientTrans_4_2, SideEffects_4_2} = ersip_trans_client:event(TimerK_4_0, ClientTrans_4_1),
    SideEffectsMap_4_2 = maps:from_list(SideEffects_4_2),
    ?assertMatch(normal, maps:get(clear_trans, SideEffectsMap_4_2)),
    ?assertEqual(normal, ersip_trans_client:clear_reason(ClientTrans_4_2)),

    %% --------------------
    %% Branch 5: Final reply after provisional response:
    {ClientTrans_5_0, _SideEffects_5_0} = ersip_trans_client:event({resp, provisional, resp_msg}, ClientTrans0),
    {ClientTrans_5_1, SideEffects_5_1} = ersip_trans_client:event({resp, final, resp_msg}, ClientTrans_5_0),
    SideEffectsMap_5_1 = maps:from_list(SideEffects_5_1),
    %% Final response is passed to transaction user:
    ?assertMatch(resp_msg, maps:get(tu_result, SideEffectsMap_5_1)),
    ?assertMatch({5000, _}, maps:get(set_timer, SideEffectsMap_5_1)),
    {_, TimerK_5_1} = maps:get(set_timer, SideEffectsMap_5_1),

    %% Timer E/F ignored in Completed state:
    {ClientTrans_5_2, []} = ersip_trans_client:event(RetransmitTimer, ClientTrans_5_1),
    {ClientTrans_5_3, []} = ersip_trans_client:event(TransactionTimer, ClientTrans_5_2),

    %% TimerK fired => transaction terminated
    {ClientTrans_5_4, SideEffects_5_4} = ersip_trans_client:event(TimerK_5_1, ClientTrans_5_3),
    SideEffectsMap_5_4 = maps:from_list(SideEffects_5_4),
    ?assertMatch(normal, maps:get(clear_trans, SideEffectsMap_5_4)),
    ?assertEqual(normal, ersip_trans_client:clear_reason(ClientTrans_5_4)),

    %% --------------
    %% Check has_final_response for different states
    ?assertEqual(true, ersip_trans_client:has_final_response(ClientTrans_1_2)),
    ok.

trans_expire_set_test() ->
    TransExpire = 31713,
    {_, SideEffects0} = ersip_trans_client:new(unreliable, message, #{trans_expire => TransExpire}),
    %% Transaction timer is set:
    [{500,   _RetransmitTimer},
     {TransExpire, _TransactionTimer}
    ] = lists:sort(proplists:get_all_values(set_timer, SideEffects0)),

    {_, SideEffects1} = ersip_trans_client:new(reliable, message, #{trans_expire => TransExpire}),
    %% Transaction timer is set:
    [{TransExpire, _TransactionTimer1}] = lists:sort(proplists:get_all_values(set_timer, SideEffects1)),
    ok.

trans_to_map_test() ->
    {ClientTrans0, _} = ersip_trans_client:new(unreliable, message, #{}),
    Expected0 = #{state => 'Trying',
                  request => message,
                  options => #{sip_t1 => 500,sip_t2 => 4000,sip_t4 => 5000},
                  reliable_transport => unreliable,
                  timers => element(6, ClientTrans0),
                  timer_e_timeout => 500,
                  clear_reason => undefined
                 },
    ?assertEqual(Expected0, ersip_trans_client:to_map(ClientTrans0)),
    ?assertEqual(ClientTrans0, ersip_trans_client:from_map(Expected0)),

    {ClientTrans1, _} = ersip_trans_client:event({resp, provisional, resp_msg}, ClientTrans0),
    Expected1 = #{state => 'Proceeding',
                  request => message,
                  options => #{sip_t1 => 500,sip_t2 => 4000,sip_t4 => 5000},
                  reliable_transport => unreliable,
                  timers => element(6, ClientTrans1),
                  timer_e_timeout => 4000,
                  clear_reason => undefined
                 },
    ?assertEqual(Expected1, ersip_trans_client:to_map(ClientTrans1)),

    {ClientTrans2, _} = ersip_trans_client:event({resp, final, resp_msg}, ClientTrans1),
    Expected2 = #{state => 'Completed',
                  request => message,
                  options => #{sip_t1 => 500,sip_t2 => 4000,sip_t4 => 5000},
                  reliable_transport => unreliable,
                  timers => element(6, ClientTrans2),
                  timer_e_timeout => 4000,
                  clear_reason => undefined
                 },
    ?assertEqual(Expected2, ersip_trans_client:to_map(ClientTrans2)),

    {ClientTrans3, _} = ersip_trans_client:event(timer_k, ClientTrans2),
    Expected3 = #{state => 'Terminated',
                  request => message,
                  options => #{sip_t1 => 500,sip_t2 => 4000,sip_t4 => 5000},
                  reliable_transport => unreliable,
                  timers => element(6, ClientTrans3),
                  timer_e_timeout => 4000,
                  clear_reason => normal
                 },
    ?assertEqual(Expected3, ersip_trans_client:to_map(ClientTrans3)).
