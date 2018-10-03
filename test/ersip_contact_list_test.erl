%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP list of the contact headers tests
%%

-module(ersip_contact_list_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

contact_list_parse_test() ->
    ?assertEqual(star, make(<<"*">>)),
    ABin = <<"sip:a@b;expires=30">>,
    BBin = <<"mail:a@b">>,
    AContact = ersip_hdr_contact:make(ABin),
    BContact = ersip_hdr_contact:make(BBin),
    ?assertEqual([AContact, BContact], make(<<ABin/binary, ",", BBin/binary>>)),
    ?assertEqual([AContact, BContact], make(<<ABin/binary, " , ", BBin/binary>>)),
    ?assertEqual([AContact, BContact], make(<<" ", ABin/binary, " , ", BBin/binary, " ">>)),
    ?assertEqual([AContact], make(<<ABin/binary>>)),
    ok.

contact_parse_errors_test() ->
    ?assertMatch({error, _}, ersip_hdr_contact_list:parse(create(<<"*,*">>))),
    ?assertMatch({error, _}, ersip_hdr_contact_list:parse(create(<<"*, sip:a@b">>))),
    ?assertMatch({error, _}, ersip_hdr_contact_list:parse(create(<<"sip:a@b, *">>))),
    ?assertMatch({error, _}, ersip_hdr_contact_list:parse(create(<<"a@b, sip:c@d">>))),
    ok.

rebuild_test() ->
    rebuild(<<"*">>),
    rebuild(<<"sip:a@b, sip:a@c">>),
    rebuild(<<"sip:a@b;expires=30, mail:a@b">>),
    rebuild(<<"Joe Doe <sip:joe@example.com>, mail:a@b">>),
    rebuild(<<"<sip:12098580021@192.168.59.76:5061;transport=tcp>;methods=\"INVITE, ACK, BYE, CANCEL, OPTIONS, INFO, MESSAGE, SUBSCRIBE, NOTIFY, PRACK, UPDATE, REFER\", sip:a@b">>),
    ok.

make_error_test() ->
    ?assertError(_, ersip_hdr_contact_list:make(<<"*, *">>)),
    ?assertError(_, ersip_hdr_contact_list:make(<<"sip:a@b, *">>)),
    ?assertError(_, ersip_hdr_contact_list:make(<<"a@b">>)),
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================

create(ContactBin) ->
    HRoute = ersip_hdr:new(<<"Contact">>),
    ersip_hdr:add_value(ContactBin, HRoute).

make(ContactBin) ->
    HContact = create(ContactBin),
    case ersip_hdr_contact_list:parse(HContact) of
        {ok, ContactList} ->
            ContactList;
        Error ->
            error(Error)
    end.

rebuild(Bin) ->
    ContactList = ersip_hdr_contact_list:make(Bin),
    {ok, ContactList1} = ersip_hdr_contact_list:parse(ersip_hdr_contact_list:build(<<"Contact">>, ContactList)),
    ?assertEqual(ContactList, ContactList1).
