%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% UAC
%%%

-module(ersip_uac_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

options_create_all_defaults_test() ->
    BobURI = ersip_uri:make(<<"sip:bob@biloxi.com">>),
    AliceFrom = ersip_hdr_fromto:make(<<"sip:alice@atlanta.com">>),
    SipMsg = ersip_uac:options(BobURI, AliceFrom, []),
    ?assertEqual(ersip_method:options(), ersip_sipmsg:method(SipMsg)),
    _ = check_required_fields(SipMsg),
    ok.

options_create_set_to_tag_test() ->
    BobURI = ersip_uri:make(<<"sip:bob@biloxi.com">>),
    AliceFrom = ersip_hdr_fromto:make(<<"sip:alice@atlanta.com;tag=atlanta">>),
    SipMsg = rebuild(ersip_uac:options(BobURI, AliceFrom, [])),
    OptionsFrom = ersip_sipmsg:from(SipMsg),
    ?assertEqual({tag, <<"atlanta">>}, ersip_hdr_fromto:tag(OptionsFrom)),
    ok.

options_create_set_callid_test() ->
    BobURI = ersip_uri:make(<<"sip:bob@biloxi.com">>),
    AliceFrom = ersip_hdr_fromto:make(<<"sip:alice@atlanta.com">>),
    CallId = ersip_hdr_callid:make(<<"a@b">>),
    SipMsg = ersip_uac:options(BobURI, AliceFrom, [{callid, CallId}]),
    ?assertEqual(CallId, ersip_sipmsg:callid(rebuild(SipMsg))),

    CallId2Bin = <<"another@test">>,
    SipMsg2 = ersip_uac:options(BobURI, AliceFrom, [{<<"Call-ID">>, CallId2Bin}]),
    ?assertEqual(ersip_hdr_callid:make(CallId2Bin), ersip_sipmsg:callid(rebuild(SipMsg2))),
    ok.

options_create_set_to_test() ->
    BobURI = ersip_uri:make(<<"sip:bob@biloxi.com">>),
    AliceFrom = ersip_hdr_fromto:make(<<"sip:alice@atlanta.com">>),
    To = ersip_hdr_fromto:make(<<"sip:a@b">>),
    SipMsg = ersip_uac:options(BobURI, AliceFrom, [{to, To}]),
    ?assertEqual(To, ersip_sipmsg:to(rebuild(SipMsg))),

    To2Bin = <<"sip:another@to">>,
    SipMsg2 = ersip_uac:options(BobURI, AliceFrom, [{<<"To">>, To2Bin}]),
    ?assertEqual(ersip_hdr_fromto:make(To2Bin), ersip_sipmsg:to(rebuild(SipMsg2))),
    ok.

%%===================================================================
%% Helpers
%%===================================================================

rebuild(SipMsg) ->
    {ok, SipMsg1} = ersip_sipmsg:parse(ersip_sipmsg:assemble_bin(SipMsg), []),
    SipMsg1.

check_required_fields(SipMsg) ->
    [begin
         ?assertMatch({ok, _}, ersip_sipmsg:find(HName, SipMsg))
     end || HName <- [from, to, maxforwards, cseq, callid]].
