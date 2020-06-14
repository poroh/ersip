%%%
%%% Copyright (c) 2018, 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP CSeq header tests
%%%

-module(ersip_hdr_cseq_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

-define(crlf, "\r\n").

parse_test() ->
    {ok, CSeq} = ersip_hdr_cseq:parse(create(<<"314159 INVITE">>)),
    ?assertEqual(ersip_method:make(<<"INVITE">>), ersip_hdr_cseq:method(CSeq)),
    ?assertEqual(314159, ersip_hdr_cseq:number(CSeq)),
    ?assertEqual(ersip_hdr_cseq:make_key(make(<<"1 REGISTER">>)), make(<<"1  REGISTER">>)),
    ?assertError({invalid_cseq, _}, ersip_hdr_cseq:make(create(<<"Not valid">>))),

    ?assertEqual({ok, CSeq}, ersip_hdr_cseq:parse(<<"314159 INVITE">>)),
    ok.

parse_fail_test() ->
    ?assertMatch({error, {invalid_cseq, _}}, ersip_hdr_cseq:parse(create(<<"314159 INVITE x">>))),
    ?assertMatch({error, {invalid_cseq, _}}, ersip_hdr_cseq:parse(create(<<"314159 INVITE, 31 ACK">>))),
    ?assertMatch({error, {invalid_cseq, _}}, ersip_hdr_cseq:parse(create(<<"123 INV@TE">>))),
    ?assertMatch({error, {invalid_cseq, _}}, ersip_hdr_cseq:parse(create(<<"abc INVITE">>))),
    ?assertMatch({error, {invalid_cseq, _}}, ersip_hdr_cseq:parse(<<"Not Valid">>)),
    NoValue = ersip_hdr:new(<<"CSeq">>),
    ?assertMatch({error, no_cseq}, ersip_hdr_cseq:parse(NoValue)),
    ok.

reassemble_test() ->
    reassemble(<<"314159 INVITE">>),
    reassemble(<<"1 REGISTER">>),
    reassemble(<<"99 MYMETHOD">>),
    ok.

assemble_bin_test() ->
    ?assertEqual(<<"314159 INVITE">>, ersip_hdr_cseq:assemble_bin(make(<<"314159 INVITE">>))),
    ok.

build_test() ->
    CSeqH = create(<<"314159 INVITE">>),
    CSeqHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(CSeqH)],
    {ok, CSeq} = ersip_hdr_cseq:parse(CSeqH),
    BuiltCSeqH = ersip_hdr_cseq:build(<<"CSeq">>,  CSeq),
    BuiltCSeqHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(BuiltCSeqH)],
    ?assertEqual(CSeqHValues, BuiltCSeqHValues),
    ok.

make_test() ->
    ?assertEqual({43, <<"INVITE">>}, ersip_hdr_cseq:raw(ersip_hdr_cseq:make(create(<<"43 INVITE">>)))),
    ?assertEqual(<<"43 INVITE">>, ersip_hdr_cseq:assemble_bin(ersip_hdr_cseq:make({43, <<"INVITE">>}))),
    ?assertError({invalid_cseq, _}, ersip_hdr_cseq:make(<<"Not valid">>)),
    ok.

raw_test() ->
    ?assertEqual({43, <<"INVITE">>}, ersip_hdr_cseq:raw(ersip_hdr_cseq:make(<<"43 INVITE">>))),
    ?assertMatch({99, _}, ersip_hdr_cseq:raw(ersip_hdr_cseq:make(<<"99 OPTIONS">>))),
    ?assertMatch({99, <<"NEWMETHOD">>}, ersip_hdr_cseq:raw(ersip_hdr_cseq:make(<<"99 NEWMETHOD">>))),
    ok.

cseq_in_message_test() ->
    Msg1 = <<"OPTIONS sip:bob@biloxi.com SIP/2.0" ?crlf
             "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds" ?crlf
             "To: a@b" ?crlf
             "From: a@b;tag=a" ?crlf
             "Call-ID: someCallId" ?crlf
             "CSeq: 314159 OPTIONS" ?crlf
             ?crlf
           >>,
    ?assertMatch({ok, _}, ersip_sipmsg:parse(Msg1, [cseq])),
    {ok, SipMsg1} = ersip_sipmsg:parse(Msg1, [cseq]),
    CSeq1 = ersip_sipmsg:cseq(SipMsg1),
    ?assertEqual(314159, ersip_hdr_cseq:number(CSeq1)),
    ?assertEqual(ersip_method:make(<<"OPTIONS">>), ersip_hdr_cseq:method(CSeq1)),

    Msg2 = <<"OPTIONS sip:bob@biloxi.com SIP/2.0" ?crlf
            "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds" ?crlf
            "To: a@b" ?crlf
            "From: a@b;tag=a" ?crlf
            "Call-ID: someCallId" ?crlf
            "Cseq: 314160 OPTIONS" ?crlf
             ?crlf
          >>,
    ?assertMatch({ok, _}, ersip_sipmsg:parse(Msg2, [cseq])),
    {ok, SipMsg2} = ersip_sipmsg:parse(Msg2, [cseq]),
    CSeq2 = ersip_sipmsg:cseq(SipMsg2),
    ?assertEqual(314160, ersip_hdr_cseq:number(CSeq2)),
    ?assertEqual(ersip_method:make(<<"OPTIONS">>), ersip_hdr_cseq:method(CSeq2)),

    ok.

%%===================================================================
%% Helpers
%%===================================================================

create(Bin) ->
    H = ersip_hdr:new(<<"CSeq">>),
    ersip_hdr:add_values([Bin], H).

make(Bin) ->
    ersip_hdr_cseq:make(Bin).

reassemble(Bin) ->
    ?assertEqual(Bin, ersip_hdr_cseq:assemble_bin(ersip_hdr_cseq:make(Bin))).
