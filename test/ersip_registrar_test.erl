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
    CallId   = ersip_sipmsg:get(callid, RegisterSipMsg),
    CSeq     = ersip_sipmsg:get(cseq, RegisterSipMsg),
    CSeqVal  = ersip_hdr_cseq:number(CSeq),
    [Contact] = ersip_sipmsg:get(contact, RegisterSipMsg),
    Expires  = ersip_sipmsg:get(expires, RegisterSipMsg),

    %% Creating new request:
    {Request0, SE0} = ersip_registrar:new_request(register_request(), Config),
    ?assertEqual(false, ersip_registrar:is_terminated(Request0)),
    ?assertMatch({find_bindings, _}, SE0),
    {find_bindings, AOR} = SE0,
    ?assertEqual(ersip_uri:make(<<"sip:1000@192.168.100.11:5060">>), AOR),

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


%%%===================================================================
%%% Helpers
%%%===================================================================

-define(crlf, "\r\n").

register_request() ->
    Msg = register_request_bin(),
    create_sipmsg(Msg, make_default_source()).

register_request_bin() ->
    <<"REGISTER sip:192.168.100.11:5060 SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5090;branch=z9hG4bK*77yCNomtXelRpoCGdCfE" ?crlf
      "Via: SIP/2.0/UDP 192.168.100.11:5070;rport;branch=z9hG4bK785703841" ?crlf
      "To: <sip:1000@192.168.100.11:5060>" ?crlf
      "From: <sip:1000@192.168.100.11:5060>;tag=1452599670" ?crlf
      "Call-ID: 1197534344" ?crlf
      "CSeq: 4 REGISTER" ?crlf
      "Max-Forwards: 69" ?crlf
      "Expires: 3600" ?crlf
      "Content-Length: 0" ?crlf
      "Contact: <sip:1000@192.168.100.11:5070;line=69210a2e715cee1>" ?crlf
      "Record-Route: <sip:192.168.100.11:5090;lr>" ?crlf
      "User-Agent: Linphone/3.6.1 (eXosip2/4.1.0)" ?crlf
      ?crlf>>.

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
