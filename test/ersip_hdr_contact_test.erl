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
    parse_error(<<"A <sip:a@b>;q=a">>),
    parse_error(<<"B <sip:a@b>;q=2">>),
    parse_error(<<"C <sip:a@b>;expires=a">>),
    parse_error(<<"C <sip:a@b>;?=$">>),
    parse_error(<<"C <sip:a@b>;?">>),
    parse_error(<<"<sip:a@b">>),
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
    ok.

uri_test() ->
    Alice = ersip_hdr_contact:make(<<"Alice <sip:alice@atlanta.com>">>),
    AliceURI = ersip_uri:make(<<"sip:alice@atlanta.com">>),
    ?assertEqual(AliceURI, ersip_hdr_contact:uri(Alice)),
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
