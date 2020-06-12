%%%
%%% Copyright (c) 2019 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% SIP Replaces header tests
%%%

-module(ersip_hdr_replaces_test).

-include_lib("eunit/include/eunit.hrl").

%%===================================================================
%% Cases
%%===================================================================

rebuild_test() ->
    %% RFC 3891:
    rebuild(<<"425928@bobster.example.org;to-tag=7743;from-tag=6472">>),
    rebuild(<<"98732@sip.example.com;from-tag=r33th4x0r;to-tag=ff87ff">>),
    rebuild(<<"12adf2f34456gs5;to-tag=12345;from-tag=54321;early-only">>),
    rebuild(<<"87134@171.161.34.23;to-tag=24796;from-tag=0">>),
    rebuild(<<"87134@171.161.34.23;to-tag=24796;from-tag=0;x=y">>),
    ok.

parse_error_test() ->
    parse_error(<<"">>),
    parse_error(<<"invalid@call@id">>),
    parse_error(<<"invalid@call@id;to-tag=24796;from-tag=0">>),
    parse_error(<<"12adf2f34456gs5;@=1;from-tag=1;to-tag=0">>),
    parse_error(<<"12adf2f34456gs5;from-tag=$;to-tag=0">>),
    parse_error(<<"12adf2f34456gs5;?=$;to-tag=0">>),
    parse_error(<<"12adf2f34456gs5;from-tag=1;to-tag=\"0\"">>),
    parse_error(<<"12adf2f34456gs5;from-tag=\"1\";to-tag=0">>),
    parse_error(<<"12adf2f34456gs5;from-tag=1@;to-tag=0">>),
    parse_error(<<"12adf2f34456gs5;from-tag=1;to-tag=0@">>),
    parse_error(<<"12adf2f34456gs5;from-tag=1;to-tag=0;early-only=false">>),
    parse_error(<<"12adf2f34456gs5;to-tag=0">>),
    parse_error(<<"12adf2f34456gs5;from-tag=0">>),
    parse_error(<<"12adf2f34456gs5;$">>),
    parse_error(<<"12adf2f34456gs5;\"x\"">>),
    parse_error(<<"12adf2f34456gs5;$=?">>),
    ok.

make_error_test() ->
    ?assertError({invalid_replaces, _}, ersip_hdr_replaces:make(<<"invalid@call@id">>)),
    ?assertError({invalid_replaces, _}, ersip_hdr_replaces:make(<<"a@b;@=1;from-tag=1;to-tag=0">>)),
    ok.

build_test() ->
    ReplacesH = create(<<"425928@bobster.example.org;to-tag=7743;from-tag=6472">>),
    {ok, Replaces} = ersip_hdr_replaces:parse(ReplacesH),
    ReplacesHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(ReplacesH)],
    BuiltReplacesH = ersip_hdr_replaces:build(<<"Replaces">>, Replaces),
    BuiltReplacesHValues = [iolist_to_binary(IOListVal) || IOListVal <- ersip_hdr:raw_values(BuiltReplacesH)],
    ?assertEqual(ReplacesHValues, BuiltReplacesHValues),

    EmptyH = ersip_hdr:new(<<"Replaces">>),
    ?assertEqual({error, no_replaces}, ersip_hdr_replaces:parse(EmptyH)),
    ok.

getters_test() ->
    Replaces = ersip_hdr_replaces:make(<<"425928@bobster.example.org;to-tag=7743;from-tag=6472;early-only">>),
    CallId = ersip_hdr_callid:make(<<"425928@bobster.example.org">>),
    LocalTagKey = ersip_hdr_fromto:tag_key({tag, <<"7743">>}),
    RemoteTagKey = ersip_hdr_fromto:tag_key({tag, <<"6472">>}),
    DialogId = ersip_dialog:make_id(LocalTagKey, RemoteTagKey, CallId),
    ?assertEqual(DialogId, ersip_hdr_replaces:dialog_id(Replaces)),
    ?assertEqual(true, ersip_hdr_replaces:early_only(Replaces)),

    ReplacesNoEarly = ersip_hdr_replaces:make(<<"425928@bobster.example.org;to-tag=7743;from-tag=6472">>),
    ?assertEqual(false, ersip_hdr_replaces:early_only(ReplacesNoEarly)),
    ok.

raw_test() ->
    Replaces = ersip_hdr_replaces:make(<<"425928@bobster.example.org;to-tag=7743;from-tag=6472;early-only">>),
    Replaces2 = ersip_hdr_replaces:make(<<"425928@bobster.example.org;to-tag=7743;from-tag=6472">>),
    ?assertMatch(#{call_id := <<"425928@bobster.example.org">>}, ersip_hdr_replaces:raw(Replaces)),
    ?assertMatch(#{to_tag := <<"7743">>}, ersip_hdr_replaces:raw(Replaces)),
    ?assertMatch(#{from_tag := <<"6472">>}, ersip_hdr_replaces:raw(Replaces)),
    ?assertMatch(#{early_only := true}, ersip_hdr_replaces:raw(Replaces)),
    ?assertMatch(#{early_only := false}, ersip_hdr_replaces:raw(Replaces2)),

    ?assertEqual(ersip_hdr_replaces:dialog_id(Replaces),
                 ersip_hdr_replaces:dialog_id(ersip_hdr_replaces:make(ersip_hdr_replaces:raw(Replaces)))),
    ?assertEqual(ersip_hdr_replaces:early_only(Replaces),
                 ersip_hdr_replaces:early_only(ersip_hdr_replaces:make(ersip_hdr_replaces:raw(Replaces)))),

    ?assertEqual(ersip_hdr_replaces:dialog_id(Replaces2),
                 ersip_hdr_replaces:dialog_id(ersip_hdr_replaces:make(ersip_hdr_replaces:raw(Replaces2)))),
    ?assertEqual(ersip_hdr_replaces:early_only(Replaces2),
                 ersip_hdr_replaces:early_only(ersip_hdr_replaces:make(ersip_hdr_replaces:raw(Replaces2)))),
    Raw = #{call_id => <<"425928@bobster.example.org">>, from_tag => <<"7743">>, to_tag => <<"6472">>},
    ?assertError({invalid_param, _}, ersip_hdr_replaces:make(Raw#{params => #{<<"@">> => <<>>}})),
    ?assertError({invalid_param, _}, ersip_hdr_replaces:make(Raw#{params => #{<<"to-tag">> => <<"@">>}})),
    ok.


%%===================================================================
%% Helpers
%%===================================================================

create(Bin) ->
    H = ersip_hdr:new(<<"Replaces">>),
    ersip_hdr:add_value(Bin, H).

rebuild(Bin) ->
    Replaces = ersip_hdr_replaces:make(Bin),
    ReplacesBin = ersip_hdr_replaces:assemble_bin(Replaces),
    {ok, Replaces1} = ersip_hdr_replaces:parse(ReplacesBin),
    ?assertEqual(Replaces, Replaces1).

parse_error(Bin) ->
    ?assertMatch({error, {invalid_replaces, _}}, ersip_hdr_replaces:parse(Bin)).
