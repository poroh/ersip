%%
%% Copyright (c) 2017 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common SIP transaction interface tests
%%

-module(ersip_trans_test).

-include_lib("eunit/include/eunit.hrl").

transaction_id_test() ->
    UASTid = <<"UAS tid">>,
    { UAS, _ } = ersip_uas:new(UASTid, reliable, message, #{}),
    ?assertEqual(UASTid, ersip_trans:id(UAS)),
    UACTid = <<"UAC tid">>,
    { UAC, _ } = ersip_uac_fsm:new(UACTid, reliable, message, #{}),
    ?assertEqual(UACTid, ersip_trans:id(UAC)).
