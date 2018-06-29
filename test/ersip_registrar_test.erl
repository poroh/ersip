%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Registrar tests
%%

-module(ersip_registrar_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

new_register_noath_basic_test() ->
    Config = ersip_registrar:new_config(any, #{authenticate => false}),

    RegisterSipMsg = register_request(),
    CallId    = ersip_sipmsg:get(callid, RegisterSipMsg),
    CSeq      = ersip_sipmsg:get(cseq, RegisterSipMsg),
    CSeqVal   = ersip_hdr_cseq:number(CSeq),
    [Contact] = ersip_sipmsg:get(contact, RegisterSipMsg),
    Expires   = ersip_sipmsg:get(expires, RegisterSipMsg),
    ReqAOR    = ersip_hdr_fromto:uri(ersip_sipmsg:get(to, RegisterSipMsg)),

    %% Creating new request:
    {Request0, SE0} = ersip_registrar:new_request(RegisterSipMsg, Config),
    ?assertEqual(false, ersip_registrar:is_terminated(Request0)),
    ?assertMatch({find_bindings, _}, SE0),
    {find_bindings, AOR} = SE0,
    ?assertEqual(ReqAOR, AOR),

    %% Processing lookup result:
    {Request1, SE1} = ersip_registrar:lookup_result({ok, []}, Request0),
    ?assertEqual(false, ersip_registrar:is_terminated(Request1)),
    ?assertMatch({update_bindings, _, _}, SE1),
    {update_bindings, AOR, AORUpdate} = SE1,
    {Added, Updated, Removed} = AORUpdate,
    ?assertEqual([], Updated),
    ?assertEqual([], Removed),
    ?assertMatch([_], Added),
    [SavedBinding] = Added,
    ?assertEqual({CallId, CSeqVal}, ersip_registrar_binding:callid_cseq(SavedBinding)),
    SavedContact = ersip_registrar_binding:contact(SavedBinding),
    ?assertEqual(ersip_hdr_contact:set_expires(Expires, Contact), SavedContact),

    %% Processing update bindings result:
    {Request2, SE2} = ersip_registrar:update_result(ok, Request1),
    ?assertEqual(true, ersip_registrar:is_terminated(Request2)),
    ?assertMatch({reply, _ReplySipMsg}, SE2),
    {reply, ReplySipMsg} = SE2,
    ?assertEqual(200, ersip_sipmsg:status(ReplySipMsg)),
    %% Reply SIP message returns registered contacts:
    ?assertMatch([_], ersip_sipmsg:get(contact, ReplySipMsg)),
    [RegContact] = ersip_sipmsg:get(contact, ReplySipMsg),
    ?assertEqual(SavedContact, RegContact),
    %% TODO: The response SHOULD include a Date header field.

    ok.

update_registration_noauth_basic_test() ->
    OldExpires = 3600,
    NewExpires = 2400,
    Config = ersip_registrar:new_config(any, #{authenticate => false}),
    SavedBindings = create_saved_bindings(#{expires => OldExpires, cseq => 4}),

    UpdateRegisterSipMsg = register_request(#{expires => NewExpires, cseq => 5}),
    CallId    = ersip_sipmsg:get(callid,  UpdateRegisterSipMsg),
    CSeq      = ersip_sipmsg:get(cseq,    UpdateRegisterSipMsg),
    [Contact] = ersip_sipmsg:get(contact, UpdateRegisterSipMsg),
    Expires   = ersip_sipmsg:get(expires, UpdateRegisterSipMsg),
    To        = ersip_sipmsg:get(to,      UpdateRegisterSipMsg),
    CSeqVal   = ersip_hdr_cseq:number(CSeq),
    AOR       = ersip_hdr_fromto:uri(To),

    %% Creating update request:
    {Request0, _} = ersip_registrar:new_request(UpdateRegisterSipMsg, Config),
    {Request1, SE1} = ersip_registrar:lookup_result({ok, SavedBindings}, Request0),
    {update_bindings, AOR, UpdateDescr} = SE1,
    ?assertMatch({[], [_], []}, UpdateDescr),
    {_, [UpdatedBinding], _} = UpdateDescr,

    %% Check that CSeqVal and CallID are equal to new request.
    ?assertEqual({CallId, CSeqVal}, ersip_registrar_binding:callid_cseq(UpdatedBinding)),
    %% Check that registered contact is updated
    UpdatedContact = ersip_registrar_binding:contact(UpdatedBinding),
    ?assertEqual(ersip_hdr_contact:set_expires(Expires, Contact), UpdatedContact),

    {_, SE2} = ersip_registrar:update_result(ok, Request1),
    ?assertMatch({reply, _ReplySipMsg}, SE2),
    {reply, ReplySipMsg} = SE2,
    ?assertEqual(200, ersip_sipmsg:status(ReplySipMsg)),
    %% Reply SIP message returns registered contacts:
    ?assertMatch([_], ersip_sipmsg:get(contact, ReplySipMsg)),
    [RegContact] = ersip_sipmsg:get(contact, ReplySipMsg),
    ?assertEqual(UpdatedContact, RegContact),

    ok.

delete_all_contacts_test() ->
    %% Delete contacts using "*"
    Config = ersip_registrar:new_config(any, #{authenticate => false}),
    SavedBindings = create_saved_bindings(#{cseq => 4}),
    DelRegisterSipMsg = register_request(#{expires => 0,
                                           cseq => 5,
                                           contact => <<"*">>
                                          }),
    To  = ersip_sipmsg:get(to, DelRegisterSipMsg),
    AOR = ersip_hdr_fromto:uri(To),

    {Request0, _} = ersip_registrar:new_request(DelRegisterSipMsg, Config),
    {Request1, SE1} = ersip_registrar:lookup_result({ok, SavedBindings}, Request0),
    {update_bindings, AOR, UpdateDescr} = SE1,
    ?assertMatch({[], [], [_]}, UpdateDescr),
    {_, _, DelBindings} = UpdateDescr,
    %% Check that all bindings are removed
    ?assertEqual(SavedBindings, DelBindings),

    {_, SE2} = ersip_registrar:update_result(ok, Request1),
    ?assertMatch({reply, _ReplySipMsg}, SE2),
    {reply, ReplySipMsg} = SE2,

    %% Reply SIP message returns empty contacts:
    ?assertEqual([], ersip_sipmsg:get(contact, ReplySipMsg)),
    ReplySipMsgRebuilt = rebuild_sipmsg(ReplySipMsg),
    ?assertEqual(not_found, ersip_sipmsg:find(contact, ReplySipMsgRebuilt)),

    ok.

unregister_one_contact_test() ->
    Config = ersip_registrar:new_config(any, #{authenticate => false}),
    FirstURI = <<"sip:contact1@example.com">>,
    SecondURI = <<"sip:contact2@example.com">>,
    SavedBindings = create_saved_bindings(#{cseq    => 4,
                                            contact => <<"<", FirstURI/binary,">, <", SecondURI/binary ,">">>
                                           }),
    DelRegisterSipMsg = register_request(#{expires => 0,
                                           cseq => 5,
                                           contact => <<"sip:contact2@example.com">>
                                          }),
    To  = ersip_sipmsg:get(to, DelRegisterSipMsg),
    AOR = ersip_hdr_fromto:uri(To),

    {Request0, _} = ersip_registrar:new_request(DelRegisterSipMsg, Config),
    {Request1, SE1} = ersip_registrar:lookup_result({ok, SavedBindings}, Request0),
    {update_bindings, AOR, UpdateDescr} = SE1,
    ?assertMatch({[], [], [_]}, UpdateDescr),
    {_, _, [DelBinding]} = UpdateDescr,
    %% Check that second binding is removed
    RemovedContact = ersip_registrar_binding:contact(DelBinding),
    ?assertEqual(ersip_uri:make(SecondURI), ersip_hdr_contact:uri(RemovedContact)),

    {_, SE2} = ersip_registrar:update_result(ok, Request1),
    ?assertMatch({reply, _ReplySipMsg}, SE2),
    {reply, ReplySipMsg} = SE2,

    RespContacts = ersip_sipmsg:get(contact, ReplySipMsg),
    ?assertMatch([_], RespContacts),
    [RemainContact] = RespContacts,
    ?assertEqual(ersip_uri:make(FirstURI), ersip_hdr_contact:uri(RemainContact)),
    ok.

update_one_contact_test() ->
    Config = ersip_registrar:new_config(any, #{authenticate => false}),
    FirstURI = <<"sip:contact1@example.com">>,
    SecondURI = <<"sip:contact2@example.com">>,
    OldExpires = 3601,
    UpdatedExpires = 3602,
    SavedBindings = create_saved_bindings(#{cseq    => 4,
                                            expires => OldExpires,
                                            contact => <<"<", FirstURI/binary,">, <", SecondURI/binary ,">">>
                                           }),
    UpdateRegisterSipMsg = register_request(#{cseq => 5,
                                              expires => UpdatedExpires,
                                              contact => <<"sip:contact2@example.com">>
                                             }),
    To  = ersip_sipmsg:get(to, UpdateRegisterSipMsg),
    AOR = ersip_hdr_fromto:uri(To),

    {Request0, _} = ersip_registrar:new_request(UpdateRegisterSipMsg, Config),
    {Request1, SE1} = ersip_registrar:lookup_result({ok, SavedBindings}, Request0),
    {update_bindings, AOR, UpdateDescr} = SE1,
    ?assertMatch({[], [_], []}, UpdateDescr),
    {_, [UpdatedBinding], _} = UpdateDescr,
    %% Check that second binding is removed
    UpdatedContact = ersip_registrar_binding:contact(UpdatedBinding),
    ?assertEqual(ersip_uri:make(SecondURI), ersip_hdr_contact:uri(UpdatedContact)),
    ?assertEqual(UpdatedExpires, ersip_hdr_contact:expires(UpdatedContact, undefined)),

    {_, SE2} = ersip_registrar:update_result(ok, Request1),
    ?assertMatch({reply, _ReplySipMsg}, SE2),
    {reply, ReplySipMsg} = SE2,

    RespContacts = ersip_sipmsg:get(contact, ReplySipMsg),
    ?assertMatch([_, _], RespContacts),
    [Contact1, Contact2] = lists:sort(RespContacts),
    ?assertEqual(OldExpires, ersip_hdr_contact:expires(Contact1, undefined)),
    ?assertEqual(ersip_uri:make(FirstURI), ersip_hdr_contact:uri(Contact1)),
    ?assertEqual(UpdatedExpires, ersip_hdr_contact:expires(Contact2, undefined)),
    ?assertEqual(ersip_uri:make(SecondURI), ersip_hdr_contact:uri(Contact2)),
    ok.

add_one_more_contact_test() ->
    FirstURI = <<"sip:contact1@example.com">>,
    SecondURI = <<"sip:contact2@example.com">>,
    Config = ersip_registrar:new_config(any, #{authenticate => false}),
    SavedBindings = create_saved_bindings(#{cseq => 4,
                                            contact => FirstURI
                                           }),
    UpdateRegisterSipMsg = register_request(#{cseq => 5,
                                              contact => SecondURI
                                             }),
    To  = ersip_sipmsg:get(to, UpdateRegisterSipMsg),
    AOR = ersip_hdr_fromto:uri(To),

    {Request0, _} = ersip_registrar:new_request(UpdateRegisterSipMsg, Config),
    {Request1, SE1} = ersip_registrar:lookup_result({ok, SavedBindings}, Request0),
    {update_bindings, AOR, UpdateDescr} = SE1,
    ?assertMatch({[_], [], []}, UpdateDescr),
    {[NewBinding], _, _} = UpdateDescr,

    NewContact = ersip_registrar_binding:contact(NewBinding),
    ?assertEqual(ersip_uri:make(SecondURI), ersip_hdr_contact:uri(NewContact)),
    {_, SE2} = ersip_registrar:update_result(ok, Request1),
    ?assertMatch({reply, _ReplySipMsg}, SE2),
    {reply, ReplySipMsg} = SE2,

    RespContacts = ersip_sipmsg:get(contact, ReplySipMsg),
    ?assertMatch([_, _], RespContacts),
    [Contact1, Contact2] = lists:sort(RespContacts),
    ?assertEqual(ersip_uri:make(FirstURI), ersip_hdr_contact:uri(Contact1)),
    ?assertEqual(ersip_uri:make(SecondURI), ersip_hdr_contact:uri(Contact2)),

    ok.


request_binding_list_test() ->
    Config = ersip_registrar:new_config(any, #{authenticate => false}),
    FirstURI = <<"sip:contact1@example.com">>,
    SecondURI = <<"sip:contact2@example.com">>,
    SavedBindings = create_saved_bindings(#{cseq    => 4,
                                            contact => <<"<", FirstURI/binary,">, <", SecondURI/binary ,">">>
                                           }),
    {Request0, _} = ersip_registrar:new_request(register_request_all(), Config),
    {Request1, SE1} = ersip_registrar:lookup_result({ok, SavedBindings}, Request0),
    ?assertEqual(true, ersip_registrar:is_terminated(Request1)),
    ?assertMatch({reply, _ReplySipMsg}, SE1),
    {reply, ReplySipMsg} = SE1,

    RespContacts = ersip_sipmsg:get(contact, ReplySipMsg),
    ?assertMatch([_, _], RespContacts),
    [Contact1, Contact2] = lists:sort(RespContacts),
    ?assertEqual(ersip_uri:make(FirstURI), ersip_hdr_contact:uri(Contact1)),
    ?assertEqual(ersip_uri:make(SecondURI), ersip_hdr_contact:uri(Contact2)),

    ok.

auth_successful_test() ->
    %% Check basic successfull authorization flow.
    Config = ersip_registrar:new_config(any, #{authenticate => true}),
    SipMsg = register_request_all(),
    To  = ersip_sipmsg:get(to, SipMsg),
    AOR = ersip_hdr_fromto:uri(To),

    {Request0, SE0} = ersip_registrar:new_request(SipMsg, Config),
    ?assertMatch({authenticate, _SipMsg}, SE0),
    {Request1, SE1} = ersip_registrar:authenticate_result({ok, {authorized, my_auth_info}}, Request0),
    ?assertMatch({authorize, my_auth_info, AOR}, SE1),
    {Request2, SE2} = ersip_registrar:authorize_result({ok, authorized}, Request1),
    ?assertMatch({find_bindings, AOR}, SE2),

    {Request3, SE3} = ersip_registrar:lookup_result({ok, []}, Request2),
    ?assertEqual(true, ersip_registrar:is_terminated(Request3)),
    ?assertMatch({reply, _ReplySipMsg}, SE3),
    {reply, ReplySipMsg} = SE3,
    ?assertEqual(200, ersip_sipmsg:status(ReplySipMsg)),
    ok.

auth_authenticate_unauth_test() ->
    %% Check that authentication procedure is transparent for registrar.
    Config = ersip_registrar:new_config(any, #{authenticate => true}),
    SipMsg = register_request_all(),
    {Request0, SE0} = ersip_registrar:new_request(SipMsg, Config),
    ?assertMatch({authenticate, SipMsg}, SE0),
    AuthReply = ersip_sipmsg:reply(401, SipMsg),
    {Request1, SE1} = ersip_registrar:authenticate_result({ok, {unauthorized, AuthReply}}, Request0),
    ?assertEqual(true, ersip_registrar:is_terminated(Request1)),
    ?assertMatch({reply, AuthReply}, SE1),
    ok.

auth_authenticate_server_error_test() ->
    %% Check that server error cause 500 reply on register request.
    Config = ersip_registrar:new_config(any, #{authenticate => true}),
    SipMsg = register_request_all(),
    {Request0, SE0} = ersip_registrar:new_request(SipMsg, Config),
    ?assertMatch({authenticate, SipMsg}, SE0),
    {Request1, SE1} = ersip_registrar:authenticate_result({error, something_bad_happened}, Request0),
    ?assertEqual(true, ersip_registrar:is_terminated(Request1)),
    ?assertMatch({reply, _}, SE1),
    {reply, ReplySipMsg} = SE1,
    ?assertEqual(500, ersip_sipmsg:status(ReplySipMsg)),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

-define(crlf, "\r\n").
-define(REG_DEFAULT, #{expires => 3600,
                       cseq    => 4,
                       contact => <<"<sip:1000@192.168.100.11:5070;line=69210a2e715cee1>">>
                      }).

register_request() ->
    register_request(?REG_DEFAULT).

register_request(Params) ->
    Msg = register_request_bin(maps:merge(?REG_DEFAULT, Params)),
    create_sipmsg(Msg, make_default_source()).

register_request_all() ->
    Msg = register_request_all_bin(),
    create_sipmsg(Msg, make_default_source()).

register_request_bin(Params) ->
    #{expires := Expires,
      cseq    := CSeq,
      contact := Contact
     } = Params,
    ExpiresBin = integer_to_binary(Expires),
    CSeqBin    = integer_to_binary(CSeq),
    <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: ", CSeqBin/binary, " REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Expires: ", ExpiresBin/binary, ?crlf
      "Content-Length: 0" ?crlf
      "Contact: ", Contact/binary, ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      "User-Agent: Linphone/3.6.1 (eXosip2/4.1.0)" ?crlf
      ?crlf>>.

register_request_all_bin() ->
    <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: 1 REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Content-Length: 0" ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      ?crlf>>.

rebuild_sipmsg(SipMsg) ->
    SipMsgBin = ersip_sipmsg:serialize_bin(SipMsg),
    P  = ersip_parser:new_dgram(SipMsgBin),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    {ok, SipMsg1} = ersip_sipmsg:parse(PMsg, all),
    SipMsg1.

make_default_source() ->
    tcp_source(default_peer()).

make_udp_source() ->
    udp_source(default_peer()).

default_peer() ->
    {{127, 0, 0, 1}, 5060}.

tcp_source(Peer) ->
    ersip_source:new(Peer, tcp_transport(), undefined).

udp_source(Peer) ->
    ersip_source:new(Peer, udp_transport(), undefined).

tcp_transport() ->
    ersip_transport:make(tcp).

udp_transport() ->
    ersip_transport:make(udp).

create_sipmsg(Msg, Source) when is_binary(Msg) ->
    create_sipmsg(Msg, Source, all).

create_sipmsg(Msg, Source, HeadersToParse) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg1 = ersip_msg:set_source(Source, PMsg),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg1, HeadersToParse),
    SipMsg.

create_saved_bindings(Params) ->
    Config = ersip_registrar:new_config(any, #{authenticate => false}),
    %% Creating new request:
    {Request0, _} = ersip_registrar:new_request(register_request(Params), Config),
    {Request1, SE1} = ersip_registrar:lookup_result({ok, []}, Request0),
    {update_bindings, _AOR, {Added, _ ,_}} = SE1,
    {_, _} = ersip_registrar:update_result(ok, Request1),
    Added.
