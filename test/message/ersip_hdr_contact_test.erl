%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Contact header tests
%%

-module(ersip_hdr_contact_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

rebuild_test() ->
    rebuild(<<"sip:a@b">>),
    rebuild(<<"sip:a@b;q=1">>),
    rebuild(<<"<sip:a@b>">>),
    rebuild(<<"<sip:a@b>;q=1">>),
    rebuild(<<"<sip:a@b>;expires=0">>),
    rebuild(<<"<sip:a@b>;q=1;expires=0">>),
    rebuild(<<"<sip:a@b>;Q=1;ExPiRes=0;a=b">>),
    rebuild(<<"<sip:a@b>;a=b">>),
    rebuild(<<"<sip:a@b>;a">>),
    rebuild(<<"A B <sip:a@b>;a">>),
    ok.


parse_error_test() ->
    parse_error(<<"a@b">>),
    parse_error(<<"sip:a@b;q=a">>),
    parse_error(<<"sip:a@b;q=2">>),
    parse_error(<<"sip:a@b;expires=a">>),
    parse_error(<<"<sip:a@b>;q=a">>),
    parse_error(<<"<sip:a@b>;q=2">>),
    parse_error(<<"<sip:a@b>;expires=a">>),
    parse_error(<<"C <sip:a@b>;expires=-1">>),
    parse_error(<<"A <sip:a@b>;q=a">>),
    parse_error(<<"B <sip:a@b>;q=2">>),
    parse_error(<<"C <sip:a@b>;expires=a">>),
    parse_error(<<"C <sip:a@b>;?=$">>),
    parse_error(<<"C <sip:a@b>;?">>),
    parse_error(<<"<sip:a@b">>),
    parse_error(<<"C <sip:a@b>;;;;;">>),
    parse_error(<<"sip:a@b;v=a, some bad rest">>),
    ok.

make_error_test() ->
    ?assertError({invalid_contact, _}, ersip_hdr_contact:make(<<"a@b">>)),
    ?assertError({invalid_contact, _}, ersip_hdr_contact:make(<<"<a@b>;expires=a">>)),
    ok.

expires_test() ->
    Alice20 = ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>;expires=20">>),
    ?assertEqual(20, ersip_hdr_contact:expires(Alice20, any)),
    Alice30 = ersip_hdr_contact:set_expires(30, Alice20),
    ?assertEqual(30, ersip_hdr_contact:expires(Alice30, any)),
    ?assertEqual(<<"Alice <sip:alice@atlanta.com>;expires=30">>, iolist_to_binary(ersip_hdr_contact:assemble(Alice30))),

    AliceNoExpires = ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>">>),
    ?assertEqual(21, ersip_hdr_contact:expires(AliceNoExpires, 21)),

    Alice45 = ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>;Expires=45">>),
    ?assertEqual(45, ersip_hdr_contact:expires(Alice45, any)),
    ok.

qvalue_test() ->
    AliceNoQValue = ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>">>),
    Alice1 = ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>;q=1.0">>),
    ?assertEqual(ersip_qvalue:make(<<"1.0">>), ersip_hdr_contact:qvalue(Alice1, any)),
    Alice01 = ersip_hdr_contact:set_qvalue(ersip_qvalue:make(<<"0.1">>), Alice1),
    ?assertEqual(ersip_qvalue:make(<<"0.1">>), ersip_hdr_contact:qvalue(Alice01, any)),
    ?assertEqual(ersip_qvalue:make(<<"0.13">>), ersip_hdr_contact:qvalue(AliceNoQValue, ersip_qvalue:make(<<"0.13">>))),
    ok.

uri_test() ->
    Alice = ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>">>),
    AliceURI = ersip_uri:make(<<"sip:alice@atlanta.com">>),
    ?assertEqual(AliceURI, ersip_hdr_contact:uri(Alice)),
    ok.

display_name_test() ->
    Alice = ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>">>),
    ?assertEqual({display_name, [<<"Alice">>]}, ersip_hdr_contact:display_name(Alice)),
    AliceQ = ersip_hdr_contact:make(<<"\"Alice and Bob\" <sip:alice_and_bob@atlanta.com>">>),
    ?assertEqual({display_name, <<"\"Alice and Bob\"">>}, ersip_hdr_contact:display_name(AliceQ)),
    ok.

all_raw_params_test() ->
    ?assertEqual([{<<"expires">>, <<"0">>}],                     all_raw_params(<<"<sip:a@b>;expires=0">>)),
    ?assertEqual([{<<"q">>, <<"1">>}, {<<"expires">>, <<"0">>}], all_raw_params(<<"<sip:a@b>;q=1;expires=0">>)),
    ?assertEqual([{<<"Q">>, <<"1">>}, {<<"ExPiRes">>, <<"0">>}, {<<"a">>, <<"b">>}],
                 all_raw_params(<<"<sip:a@b>;Q=1;ExPiRes=0;a=b">>)),
    ?assertEqual([{<<"a">>, <<"b">>}], all_raw_params(<<"<sip:a@b>;a=b">>)),
    ?assertEqual([<<"a">>], all_raw_params(<<"<sip:a@b>;a">>)),
    ok.

set_param_test() ->
    Alice = ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>">>),
    AliceWExpires = ersip_hdr_contact:set_param(<<"expires">>, <<"30">>, Alice),
    ?assertEqual(30, ersip_hdr_contact:expires(AliceWExpires, undefined)),
    AliceWExpiresQ = ersip_hdr_contact:set_param(<<"q">>, <<"0.1">>, AliceWExpires),
    ?assertEqual(30, ersip_hdr_contact:expires(AliceWExpiresQ, undefined)),
    ?assertEqual(ersip_qvalue:make(<<"0.1">>), ersip_hdr_contact:qvalue(AliceWExpiresQ, undefined)),

    AliceWCustomP = ersip_hdr_contact:set_param(<<"myparam">>, <<"Value">>, Alice),
    ?assertEqual(ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>;myparam=Value">>), AliceWCustomP),
    ?assertEqual({ok, <<"Value">>}, ersip_hdr_contact:param(<<"MyParam">>, AliceWCustomP)),
    ok.

set_param_error_test() ->
    Alice = ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>">>),
    ?assertError({invalid_expires, _}, ersip_hdr_contact:set_param(<<"expires">>, <<"@">>, Alice)),
    ?assertError({invalid_qvalue, _}, ersip_hdr_contact:set_param(<<"q">>, <<"2">>, Alice)),
    ?assertError({invalid_param, _}, ersip_hdr_contact:set_param(<<"@">>, <<"Value">>, Alice)),
    ?assertError({invalid_param, _}, ersip_hdr_contact:set_param(<<"@">>, <<>>, Alice)),
    ok.

assemble_new_test() ->
    NewContact = ersip_hdr_contact:new(ersip_uri:make(<<"sip:a@b">>)),
    ?assertEqual(<<"<sip:a@b>">>, ersip_hdr_contact:assemble_bin(NewContact)),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

rebuild(Bin) ->
    Contact = ersip_hdr_contact:make(Bin),
    ContactBin = iolist_to_binary(ersip_hdr_contact:assemble(Contact)),
    {ok, Contact1} = ersip_hdr_contact:parse(ContactBin),
    ?assertEqual(Contact, Contact1).

parse_error(Bin) ->
    ?assertMatch({error, {invalid_contact, _}}, ersip_hdr_contact:parse(Bin)).

all_raw_params(Bin) ->
    Contact = ersip_hdr_contact:make(Bin),
    ersip_hdr_contact:all_raw_params(Contact).
