%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common dialog support test
%%
%% TODO:
%% 1. rfc2543 compliance test - without tags in response/request
%% 2. In-dialog ACK (check that CSeq passing)
%% 3. In-dialog CANCEL (check that CSeq passing)
%% 4. Check record route with loose route
%% 5. Check record route with strict route
%% 6. Check filling of empty cseq fields
%%


-module(ersip_dialog_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

dialog_create_test() ->
    InvReq = invite_request(),
    InvSipMsg = ersip_request:sipmsg(InvReq),
    InvResp180UAS = invite_reply(180, InvSipMsg),
    ?assertMatch({_, _}, ersip_dialog:uas_new(InvSipMsg, InvResp180UAS)),
    {UASDialogEarly, InvResp180UAC} = ersip_dialog:uas_new(InvSipMsg, InvResp180UAS),

    ?assertMatch({ok, _}, ersip_dialog:uac_new(InvReq, InvResp180UAC)),
    {ok, UACDialogEarly} = ersip_dialog:uac_new(InvReq, InvResp180UAC),

    InvResp200UAS = invite_reply(200, InvSipMsg),
    {UASDialogConfirmed, InvResp200UAC} = ersip_dialog:uas_update(InvResp200UAS, UASDialogEarly),

    {ok, UACDialogConfirmed} = ersip_dialog:uac_update(InvResp200UAS, UACDialogEarly),

    %% ====================
    %% Sending BYE through UAC dialog:
    {_UACDialog1, ByeSipMsgA} = ersip_dialog:uac_request(bye_sipmsg(), UACDialogConfirmed),
    %% Check that message is filled in according to dialog:

    %% 1. The URI in the To field of the request MUST be set to the
    %% remote URI from the dialog state.
    ?assertEqual(ersip_hdr_fromto:uri(ersip_sipmsg:get(to, InvSipMsg)),
                 ersip_hdr_fromto:uri(ersip_sipmsg:get(to, ByeSipMsgA))),

    %% 2. The tag in the To header field of the request MUST be set to
    %% the remote tag of the dialog ID.
    ?assertEqual(ersip_hdr_fromto:tag(ersip_sipmsg:get(to, InvResp180UAC)),
                 ersip_hdr_fromto:tag(ersip_sipmsg:get(to, ByeSipMsgA))),

    %% 3. The From URI of the request MUST be set to the local URI
    %% from the dialog state.
    ?assertEqual(ersip_hdr_fromto:uri(ersip_sipmsg:get(from, InvSipMsg)),
                 ersip_hdr_fromto:uri(ersip_sipmsg:get(from, ByeSipMsgA))),

    %% 4. The tag in the From header field of the request MUST be set to the local tag
    %% of the dialog ID
    ?assertEqual(ersip_hdr_fromto:tag(ersip_sipmsg:get(from, InvSipMsg)),
                 ersip_hdr_fromto:tag(ersip_sipmsg:get(from, ByeSipMsgA))),

    %% 5. The Call-ID of the request MUST be set to the Call-ID of the dialog.
    ?assertEqual(ersip_sipmsg:get(callid, InvSipMsg),
                 ersip_sipmsg:get(callid, ByeSipMsgA)),

    %% 6. Requests within a dialog MUST contain strictly monotonically
    %%    increasing and contiguous CSeq sequence numbers
    %%    (increasing-by-one) in each direction (excepting ACK and CANCEL
    %%    of course, whose numbers equal the requests being acknowledged
    %%    or cancelled).
    ?assert(ersip_hdr_cseq:number(ersip_sipmsg:get(cseq, InvSipMsg))
            < ersip_hdr_cseq:number(ersip_sipmsg:get(cseq, ByeSipMsgA))),

    %% 7. The method field in the CSeq header field value MUST match
    %% the method of the request.
    ?assertEqual(ersip_sipmsg:method(ByeSipMsgA),
                 ersip_hdr_cseq:method(ersip_sipmsg:get(cseq, ByeSipMsgA))),

    %% 8. If the route set is empty, the UAC MUST place the remote target URI
    %% into the Request-URI. The UAC MUST NOT add a Route header field to
    %% the request.
    [RemoteContactA] = ersip_sipmsg:get(contact, InvResp200UAC),
    ?assertEqual(ersip_hdr_contact:uri(RemoteContactA),
                 ersip_sipmsg:ruri(ByeSipMsgA)),

    %% ====================
    %% Sending BYE through UAS dialog:
    {_UASDialog1, ByeSipMsgB} = ersip_dialog:uac_request(bye_sipmsg(), UASDialogConfirmed),
    %% Check that message is filled in according to dialog:

    %% 1. The URI in the To field of the request MUST be set to the
    %% remote URI from the dialog state.
    ?assertEqual(ersip_hdr_fromto:uri(ersip_sipmsg:get(from, InvSipMsg)),
                 ersip_hdr_fromto:uri(ersip_sipmsg:get(to,   ByeSipMsgB))),

    %% 2. The tag in the To header field of the request MUST be set to
    %% the remote tag of the dialog ID.
    ?assertEqual(ersip_hdr_fromto:tag(ersip_sipmsg:get(from, InvResp180UAC)),
                 ersip_hdr_fromto:tag(ersip_sipmsg:get(to,   ByeSipMsgB))),

    %% 3. The From URI of the request MUST be set to the local URI
    %% from the dialog state.
    ?assertEqual(ersip_hdr_fromto:uri(ersip_sipmsg:get(to,   InvSipMsg)),
                 ersip_hdr_fromto:uri(ersip_sipmsg:get(from, ByeSipMsgB))),

    %% 4. The tag in the From header field of the request MUST be set to the local tag
    %% of the dialog ID
    ?assertEqual(ersip_hdr_fromto:tag(ersip_sipmsg:get(to,   InvResp180UAS)),
                 ersip_hdr_fromto:tag(ersip_sipmsg:get(from, ByeSipMsgB))),

    %% 5. The Call-ID of the request MUST be set to the Call-ID of the dialog.
    ?assertEqual(ersip_sipmsg:get(callid, InvSipMsg),
                 ersip_sipmsg:get(callid, ByeSipMsgB)),

    %% 6. Requests within a dialog MUST contain strictly monotonically
    %%    increasing and contiguous CSeq sequence numbers
    %%    (increasing-by-one) in each direction (excepting ACK and CANCEL
    %%    of course, whose numbers equal the requests being acknowledged
    %%    or cancelled).
    %% Filled with new value

    %% 7. The method field in the CSeq header field value MUST match
    %% the method of the request.
    ?assertEqual(ersip_sipmsg:method(ByeSipMsgB),
                 ersip_hdr_cseq:method(ersip_sipmsg:get(cseq, ByeSipMsgB))),

    %% 8. If the route set is empty, the UAC MUST place the remote target URI
    %% into the Request-URI. The UAC MUST NOT add a Route header field to
    %% the request.
    [RemoteContactB] = ersip_sipmsg:get(contact, InvSipMsg),
    ?assertEqual(ersip_hdr_contact:uri(RemoteContactB),
                 ersip_sipmsg:ruri(ByeSipMsgB)),

    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

-define(crlf, "\r\n").

invite_request() ->
    InvSipMsg = create_sipmsg(invite_request_bin(), make_default_source()),
    Target = ersip_uri:make(<<"sip:127.0.0.1">>),
    ersip_request:new(InvSipMsg, ersip_branch:make_random(7), Target).

invite_request_bin() ->
    <<"INVITE sip:bob@biloxi.com SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8" ?crlf
      "Max-Forwards: 70" ?crlf
      "To: Bob <sip:bob@biloxi.com>" ?crlf
      "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
      "Call-ID: a84b4c76e66710" ?crlf
      "CSeq: 314159 INVITE" ?crlf
      "Contact: <sip:alice@pc33.atlanta.com>" ?crlf
      "Content-Type: application/sdp" ?crlf
      "Content-Length: 4" ?crlf
      ?crlf
      "Test">>.

invite_reply(Code, InvSipMsg) ->
    InvResp = ersip_sipmsg:reply(Code, InvSipMsg),
    ersip_sipmsg:set(contact, make_contact(<<"sip:a@127.0.0.1:5070">>), InvResp).

bye_sipmsg() ->
    create_sipmsg(bye_bin(), make_default_source(), []).

bye_bin() ->
    <<"BYE sip:bob@biloxi.com SIP/2.0" ?crlf
      "Max-Forwards: 70" ?crlf
      ?crlf>>.

make_default_source() ->
    tcp_source(default_peer()).

default_peer() ->
    {{127, 0, 0, 1}, 5060}.

tcp_source(Peer) ->
    ersip_source:new(Peer, ersip_transport:tcp(), undefined).

create_sipmsg(Msg, Source) when is_binary(Msg) ->
    create_sipmsg(Msg, Source, all).

create_sipmsg(Msg, Source, HeadersToParse) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg1 = ersip_msg:set_source(Source, PMsg),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg1, HeadersToParse),
    SipMsg.

make_contact(ContactBin) when is_binary(ContactBin) ->
    Contact = ersip_hdr_contact:make(ContactBin),
    [Contact].
