%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Common dialog support test
%%
%% TODO:
%%  - Check that Record-route are ignored for target_refresher
%%     messages
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

    ?assertEqual(no_dialog, ersip_dialog:uas_dialog_id(InvSipMsg)),
    ?assertMatch({_, _}, ersip_dialog:uas_new(InvSipMsg, InvResp180UAS)),
    {UASDialogEarly, InvResp180UAC} = ersip_dialog:uas_new(InvSipMsg, InvResp180UAS),

    ?assertMatch({ok, _}, ersip_dialog:uac_new(InvReq, InvResp180UAC)),
    {ok, UACDialogEarly} = ersip_dialog:uac_new(InvReq, InvResp180UAC),

    InvResp200UAS = invite_reply(200, InvSipMsg),
    {UASDialogConfirmed, InvResp200UAC} = ersip_dialog:uas_pass_response(InvSipMsg, InvResp200UAS, UASDialogEarly),

    {ok, UACDialogConfirmed} = ersip_dialog:uac_update(InvResp200UAS, UACDialogEarly),

    %% ====================
    %% Sending BYE through UAC dialog:
    {_UACDialog1, ByeSipMsgA} = ersip_dialog:uac_request(bye_sipmsg(), UACDialogConfirmed),

    %% --------------------
    %% Check that dialog identifier of UAS is equal to calculated by request:
    ?assertMatch({ok, _}, ersip_dialog:uas_dialog_id(ByeSipMsgA)),
    {ok, ByeUASDialogId} = ersip_dialog:uas_dialog_id(ByeSipMsgA),
    ?assertEqual(ersip_dialog:id(UASDialogConfirmed), ByeUASDialogId),

    %% --------------------
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

    %% --------------------
    %% Check that dialog identifier of UAC is equal to calculated by request:
    ?assertMatch({ok, _}, ersip_dialog:uas_dialog_id(ByeSipMsgB)),
    {ok, ByeBDialogId} = ersip_dialog:uas_dialog_id(ByeSipMsgB),
    ?assertEqual(ersip_dialog:id(UACDialogConfirmed), ByeBDialogId),

    %% --------------------
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

uas_dialog_rfc2543_compiance_test() ->
    %% A UAS MUST be prepared to receive a
    %% request without a tag in the From field, in which case the tag is
    %% considered to have a value of null.
    %%
    %%    This is to maintain backwards compatibility with RFC 2543, which
    %%    did not mandate From tags.
    InvReq = invite_request(),
    InvSipMsg = clear_tag(from, ersip_request:sipmsg(InvReq)),
    InvResp200 = invite_reply(200, InvSipMsg),
    ?assertMatch({_, _}, ersip_dialog:uas_new(InvSipMsg, InvResp200)),
    {Dialog, _} = ersip_dialog:uas_new(InvSipMsg, InvResp200),
    %% If the value of the remote or local tags is null, the tag
    %% parameter MUST be omitted from the To or From header fields,
    %% respectively.
    {_, ByeSipMsgB} = ersip_dialog:uac_request(bye_sipmsg(), Dialog),
    ?assertEqual(undefined, ersip_hdr_fromto:tag(ersip_sipmsg:get(to, ByeSipMsgB))),

    %% Check that message sent without from tag is mached dialog
    %% created by initial invite.
    {ok, DialogA} = ersip_dialog:uac_new(InvReq, InvResp200),
    {_, ByeSipMsgA0} = ersip_dialog:uac_request(bye_sipmsg(), DialogA),
    ByeSipMsgA = clear_tag(from, ByeSipMsgA0),
    {ok, ByeADialogId} = ersip_dialog:uas_dialog_id(ByeSipMsgA),
    ?assertEqual(ersip_dialog:id(Dialog), ByeADialogId),

    ok.

uac_dialog_rfc2543_compiance_test() ->
    %% A UAC MUST be prepared to receive a response without a tag in
    %% the To field, in which case the tag is considered to have a
    %% value of null.
    %%
    %%    This is to maintain backwards compatibility with RFC 2543,
    %%    which did not mandate To tags.
    InvReq = invite_request(),
    InvSipMsg = ersip_request:sipmsg(InvReq),
    InvResp200 = clear_tag(to, invite_reply(200, InvSipMsg)),
    ?assertEqual(undefined, ersip_hdr_fromto:tag(ersip_sipmsg:get(to, InvResp200))),
    ?assertMatch({ok, _}, ersip_dialog:uac_new(InvReq, InvResp200)),
    {ok, Dialog} = ersip_dialog:uac_new(InvReq, InvResp200),
    %% If the value of the remote or local tags is null, the tag
    %% parameter MUST be omitted from the To or From header fields,
    %% respectively.
    {_, ByeSipMsg} = ersip_dialog:uac_request(bye_sipmsg(), Dialog),
    ?assertEqual(undefined, ersip_hdr_fromto:tag(ersip_sipmsg:get(to, ByeSipMsg))),

    %% Check that message sent without from tag is mached dialog
    %% created by initial invite.
    {DialogB, _} = ersip_dialog:uas_new(InvSipMsg, InvResp200),
    {_, ByeSipMsgB0} = ersip_dialog:uac_request(bye_sipmsg(), DialogB),
    ByeSipMsgB = clear_tag(from, ByeSipMsgB0),
    {ok, ByeBDialogId} = ersip_dialog:uas_dialog_id(ByeSipMsgB),
    ?assertEqual(ersip_dialog:id(Dialog), ByeBDialogId),
    ok.

indialog_ack_and_cancel_cseq_test() ->
    %% Requests within a dialog MUST contain strictly monotonically
    %% increasing and contiguous CSeq sequence numbers (increasing-by-one)
    %% in each direction (excepting ACK and CANCEL of course, whose numbers
    %% equal the requests being acknowledged or cancelled).
    {UASDialog0, UACDialog0} = create_uas_uac_dialogs(invite_request()),
    {UASDialog1, ReInviteSipMsg} = ersip_dialog:uac_request(reinvite_sipmsg(), UASDialog0),
    {_, AckSipMsg}    = ersip_dialog:uac_request(ack_sipmsg(), UASDialog1),
    {_, CancelSipMsg} = ersip_dialog:uac_request(cancel_sipmsg(), UASDialog1),
    ?assertEqual(cseq_number(ReInviteSipMsg), cseq_number(AckSipMsg)),
    ?assertEqual(cseq_number(ReInviteSipMsg), cseq_number(CancelSipMsg)),

    {UACDialog1, UACReInviteSipMsg} = ersip_dialog:uac_request(reinvite_sipmsg(), UACDialog0),
    {_, UACAckSipMsg}    = ersip_dialog:uac_request(ack_sipmsg(), UACDialog1),
    {_, UACCancelSipMsg} = ersip_dialog:uac_request(cancel_sipmsg(), UACDialog1),
    ?assertEqual(cseq_number(UACReInviteSipMsg), cseq_number(UACAckSipMsg)),
    ?assertEqual(cseq_number(UACReInviteSipMsg), cseq_number(UACCancelSipMsg)),
    ok.

indialog_ack_and_cancel_cseq_no_cseq_test() ->
    %% Requests within a dialog MUST contain strictly monotonically
    %% increasing and contiguous CSeq sequence numbers (increasing-by-one)
    %% in each direction (excepting ACK and CANCEL of course, whose numbers
    %% equal the requests being acknowledged or cancelled).
    {UASDialog0, UACDialog0} = create_uas_uac_dialogs(invite_request()),
    {UASDialog1, ReInviteSipMsg} = ersip_dialog:uac_request(reinvite_sipmsg(), UASDialog0),
    {_, AckSipMsg}    = ersip_dialog:uac_request(del_cseq(ack_sipmsg()), UASDialog1),
    {_, CancelSipMsg} = ersip_dialog:uac_request(del_cseq(cancel_sipmsg()), UASDialog1),
    ?assertEqual(cseq_number(ReInviteSipMsg), cseq_number(AckSipMsg)),
    ?assertEqual(cseq_number(ReInviteSipMsg), cseq_number(CancelSipMsg)),

    {UACDialog1, UACReInviteSipMsg} = ersip_dialog:uac_request(reinvite_sipmsg(), UACDialog0),
    {_, UACAckSipMsg}    = ersip_dialog:uac_request(del_cseq(ack_sipmsg()), UACDialog1),
    {_, UACCancelSipMsg} = ersip_dialog:uac_request(del_cseq(cancel_sipmsg()), UACDialog1),
    ?assertEqual(cseq_number(UACReInviteSipMsg), cseq_number(UACAckSipMsg)),
    ?assertEqual(cseq_number(UACReInviteSipMsg), cseq_number(UACCancelSipMsg)),
    ok.

uas_message_checking_cseq_test() ->
    %% 1. If the remote sequence number is empty, it MUST be set to
    %% the value of the sequence number in the CSeq header field value
    %% in the request.
    {UASDialog0, UACDialog0} = create_uas_uac_dialogs(invite_request()),
    %% Note that UAC dialog has empty remote sequence number, so we
    %% use initially UAC side as UAS for CSeq checking:
    CSeq = <<"3251">>,
    {_, ReInviteSipMsg} =
        ersip_dialog:uac_request(reinvite_sipmsg(#{cseq => CSeq}), UASDialog0),
    ?assertEqual(empty, ersip_dialog:remote_seq(UACDialog0)),
    {ok, UpdatedDialog} = ersip_dialog:uas_process(ReInviteSipMsg, target_refresh, UACDialog0),
    ?assertEqual(binary_to_integer(CSeq), ersip_dialog:remote_seq(UpdatedDialog)),

    %% If the remote sequence number was not empty, but the sequence
    %% number of the request is lower than the remote sequence number,
    %% the request is out of order and MUST be rejected with a 500
    %% (Server Internal Error) response.
    {_, ReInviteSipMsg1} = ersip_dialog:uac_request(reinvite_sipmsg(), UASDialog0),
    ReInviteSipMsg2 = set_cseq_number(3250, ReInviteSipMsg1),
    ?assertMatch({reply, _}, ersip_dialog:uas_process(ReInviteSipMsg2, target_refresh, UpdatedDialog)),
    {reply, Resp500} = ersip_dialog:uas_process(ReInviteSipMsg2, target_refresh, UpdatedDialog),
    ?assertEqual(500, ersip_sipmsg:status(Resp500)),

    %% Check that in-order message updates cseq:
    CSeqNew = 3252,
    ReInviteSipMsg3 = set_cseq_number(CSeqNew, ReInviteSipMsg1),
    ?assertMatch({ok, _}, ersip_dialog:uas_process(ReInviteSipMsg3, target_refresh, UpdatedDialog)),
    {ok, UpdatedDialog1} = ersip_dialog:uas_process(ReInviteSipMsg3, target_refresh, UpdatedDialog),
    ?assertEqual(CSeqNew, ersip_dialog:remote_seq(UpdatedDialog1)),
    ok.

loose_routing_dialog_test() ->
    %% Create dialogs with defined route set:
    {BobDialog, AliceDialog} = create_uas_uac_dialogs(invite_request(), fun loose_route/2),

    {_, ReInviteFromBob} = ersip_dialog:uac_request(reinvite_sipmsg(), BobDialog),
    RouteBob = ersip_sipmsg:get(route, ReInviteFromBob),
    ?assertEqual(ersip_uri:make(<<"sip:alice@pc33.atlanta.com">>), ersip_sipmsg:ruri(ReInviteFromBob)),
    %% Check requirements:
    %%
    %% Creating route set:
    %% 1. The route set MUST be set to the list of URIs in the
    %%    Record-Route header field from the request, taken in order
    %%    and preserving all URI parameters.
    %%
    %% Filling loose-route request:
    %% 2. If the route set is not empty, and the first URI in the
    %%    route set contains the lr parameter (see Section 19.1.1),
    %%    the UAC MUST place the remote target URI into the
    %%    Request-URI and MUST include a Route header field containing
    %%    the route set values in order, including all parameters.
    ?assertEqual(ersip_uri:make(<<"sip:biloxi.com;lr">>), ersip_hdr_route:uri(ersip_route_set:first(RouteBob))),
    ?assertEqual(ersip_uri:make(<<"sip:atlanta.com;lr">>), ersip_hdr_route:uri(ersip_route_set:last(RouteBob))),

    {_, ReInviteFromAlice} = ersip_dialog:uac_request(reinvite_sipmsg(), AliceDialog),
    RouteAlice = ersip_sipmsg:get(route, ReInviteFromAlice),
    ?assertEqual(ersip_uri:make(<<"sip:bob@192.0.2.4">>), ersip_sipmsg:ruri(ReInviteFromAlice)),
    %% Check requirements:
    %%
    %% Creating route set:
    %% 1. The route set MUST be set to the list of URIs in the
    %%    Record-Route header field from the response, taken in
    %%    reverse order and preserving all URI parameters.
    %%
    %% Filling loose-route request:
    %% 2. If the route set is not empty, and the first URI in the
    %%    route set contains the lr parameter (see Section 19.1.1),
    %%    the UAC MUST place the remote target URI into the
    %%    Request-URI and MUST include a Route header field containing
    %%    the route set values in order, including all parameters.
    ?assertEqual(ersip_uri:make(<<"sip:atlanta.com;lr">>), ersip_hdr_route:uri(ersip_route_set:first(RouteAlice))),
    ?assertEqual(ersip_uri:make(<<"sip:biloxi.com;lr">>), ersip_hdr_route:uri(ersip_route_set:last(RouteAlice))),
    ok.

strict_routing_dialog_test() ->
    %% Create dialogs with defined route set:
    {BobDialog, AliceDialog} = create_uas_uac_dialogs(invite_request(), fun strict_route/2),

    %% Check requirements:
    %%
    %% If the route set is not empty, and its first URI does not
    %% contain the lr parameter, the UAC MUST place the first URI from
    %% the route set into the Request-URI, stripping any parameters
    %% that are not allowed in a Request-URI.  The UAC MUST add a
    %% Route header field containing the remainder of the route set
    %% values in order, including all parameters.  The UAC MUST then
    %% place the remote target URI into the Route header field as the
    %% last value.
    {_, ReInviteFromBob} = ersip_dialog:uac_request(reinvite_sipmsg(), BobDialog),
    RouteBob = ersip_sipmsg:get(route, ReInviteFromBob),
    ?assertEqual(ersip_uri:make(<<"sip:biloxi.com">>), ersip_sipmsg:ruri(ReInviteFromBob)),
    ?assertEqual(ersip_uri:make(<<"sip:atlanta.com">>), ersip_hdr_route:uri(ersip_route_set:first(RouteBob))),
    ?assertEqual(ersip_uri:make(<<"sip:alice@pc33.atlanta.com">>), ersip_hdr_route:uri(ersip_route_set:last(RouteBob))),

    {_, ReInviteFromAlice} = ersip_dialog:uac_request(reinvite_sipmsg(), AliceDialog),
    RouteAlice = ersip_sipmsg:get(route, ReInviteFromAlice),
    ?assertEqual(ersip_uri:make(<<"sip:atlanta.com">>), ersip_sipmsg:ruri(ReInviteFromAlice)),
    ?assertEqual(ersip_uri:make(<<"sip:biloxi.com">>), ersip_hdr_route:uri(ersip_route_set:first(RouteAlice))),
    ?assertEqual(ersip_uri:make(<<"sip:bob@192.0.2.4">>), ersip_hdr_route:uri(ersip_route_set:last(RouteAlice))),
    ok.

target_refresh_test() ->
    %% Create dialogs with defined route set:
    {BobDialog,  AliceDialog} = create_uas_uac_dialogs(invite_request()),
    NewBobContact = <<"sip:bob-new@192.0.2.5">>,
    {BobDialog1, ReInviteFromBob} = ersip_dialog:uac_request(reinvite_sipmsg(#{contact => NewBobContact}), BobDialog),
    {ok, AliceDialogRefreshed} = ersip_dialog:uas_process(ReInviteFromBob, target_refresh, AliceDialog),
    AliceReInviteResp0 = ersip_sipmsg:reply(200, ReInviteFromBob),
    NewAliceContact = <<"sip:alice-new@pc34.atlanta.com">>,
    AliceReInviteResp = ersip_sipmsg:set(contact, make_contact(NewAliceContact), AliceReInviteResp0),
    {ok, BobDialogRefreshed} = ersip_dialog:uac_trans_result(AliceReInviteResp, target_refresh, BobDialog1),

    ?assertEqual(ersip_uri:make(NewAliceContact), remote_target(BobDialogRefreshed)),
    ?assertEqual(ersip_uri:make(NewBobContact),   remote_target(AliceDialogRefreshed)),
    ok.

neg_400_on_star_contact_test() ->
    InvSipMsg0 = ersip_request:sipmsg(invite_request()),
    InvSipMsg  = ersip_sipmsg:set(contact, ersip_hdr_contact_list:make_star(), InvSipMsg0),
    ?assertMatch({reply, _}, ersip_dialog:uas_verify(InvSipMsg)),
    {reply, Resp400} = ersip_dialog:uas_verify(InvSipMsg),
    ?assertEqual(400, ersip_sipmsg:status(Resp400)),
    ok.

neg_400_on_multiple_contact_test() ->
    InvSipMsg0 = ersip_request:sipmsg(invite_request()),
    ContactList = [ersip_hdr_contact:make(<<"sip:alice@pc33.atlanta.com">>), ersip_hdr_contact:make(<<"sip:bob@192.0.2.4">>)],
    InvSipMsg  = ersip_sipmsg:set(contact, ContactList, InvSipMsg0),
    ?assertMatch({reply, _}, ersip_dialog:uas_verify(InvSipMsg)),
    {reply, Resp400} = ersip_dialog:uas_verify(InvSipMsg),
    ?assertEqual(400, ersip_sipmsg:status(Resp400)),
    ok.

neg_400_on_no_contact_test() ->
    InvSipMsg0 = ersip_request:sipmsg(invite_request()),
    ContactList = [],
    InvSipMsg  = ersip_sipmsg:set(contact, ContactList, InvSipMsg0),
    ?assertMatch({reply, _}, ersip_dialog:uas_verify(InvSipMsg)),
    {reply, Resp400} = ersip_dialog:uas_verify(InvSipMsg),
    ?assertEqual(400, ersip_sipmsg:status(Resp400)),
    ok.

neg_400_on_bad_record_route_test() ->
    InvSipMsg = create_sipmsg(invite_request_bin(#{record_route => <<"aaaa">>}), make_default_source(), []),
    ?assertMatch({reply, _}, ersip_dialog:uas_verify(InvSipMsg)),
    {reply, Resp400} = ersip_dialog:uas_verify(InvSipMsg),
    ?assertEqual(400, ersip_sipmsg:status(Resp400)),
    ok.

uas_verify_test() ->
    InvSipMsg1 = create_sipmsg(invite_request_bin(#{}), make_default_source(), []),
    ?assertEqual(ok, ersip_dialog:uas_verify(InvSipMsg1)),
    InvSipMsg2 = create_sipmsg(invite_request_bin(#{record_route => <<"<sip:atlanta.com>">>}), make_default_source(), []),
    ?assertEqual(ok, ersip_dialog:uas_verify(InvSipMsg2)),
    ok.

uac_trans_result_terminates_dialog_test() ->
    {BobDialog,  _} = create_uas_uac_dialogs(invite_request()),
    {BobDialog1, ReInviteFromBob} = ersip_dialog:uac_request(reinvite_sipmsg(), BobDialog),

    %% If the response for a request within a dialog is a 481
    %% (Call/Transaction Does Not Exist) or a 408 (Request Timeout),
    %% the UAC SHOULD terminate the dialog.  A UAC SHOULD also
    %% terminate a dialog if no response at all is received for the
    %% request (the client transaction would inform the TU about the
    %% timeout.)

    %% 1. 481
    AliceReInviteResp481 = ersip_sipmsg:reply(481, ReInviteFromBob),
    ?assertEqual(terminate_dialog, ersip_dialog:uac_trans_result(AliceReInviteResp481, target_refresh, BobDialog1)),
    %% 2. 408
    AliceReInviteResp408 = ersip_sipmsg:reply(408, ReInviteFromBob),
    ?assertEqual(terminate_dialog, ersip_dialog:uac_trans_result(AliceReInviteResp408, target_refresh, BobDialog1)),
    %% 3. timeout
    ?assertEqual(terminate_dialog, ersip_dialog:uac_trans_result(timeout, target_refresh, BobDialog1)),

    %% Dialog does not terminated on other response codes:
    [begin
         Reply = ersip_sipmsg:reply(Code, ReInviteFromBob),
         ?assertMatch({ok, _}, ersip_dialog:uac_trans_result(Reply, target_refresh, BobDialog1))
     end || Code <- [200, 299, 400, 407, 409, 499, 500, 599, 600, 699]],
    ok.

no_contact_means_no_refresh_test() ->
    %% Check last "if present" in clause:
    %%
    %% When a UAS receives a target refresh request, it MUST replace the
    %% dialog's remote target URI with the URI from the Contact header field
    %% in that request, if present.

    NoContact = <<>>,
    {BobDialog,  AliceDialog} = create_uas_uac_dialogs(invite_request()),

    BobRURI   = remote_target(AliceDialog),
    AliceRURI = remote_target(BobDialog),

    {BobDialog1, ReInviteFromBob} = ersip_dialog:uac_request(reinvite_sipmsg(#{contact => NoContact}), BobDialog),

    {ok, AliceDialogAfter} = ersip_dialog:uas_process(ReInviteFromBob, target_refresh, AliceDialog),
    AliceReInviteResp0 = ersip_sipmsg:reply(200, ReInviteFromBob),
    AliceReInviteResp = ersip_sipmsg:remove(contact, AliceReInviteResp0),
    {ok, BobDialogAfter} = ersip_dialog:uac_trans_result(AliceReInviteResp, target_refresh, BobDialog1),

    ?assertEqual(AliceRURI, remote_target(BobDialogAfter)),
    ?assertEqual(BobRURI,   remote_target(AliceDialogAfter)),

    ok.

regular_requests_means_no_refresh_test() ->
    {BobDialog,  AliceDialog} = create_uas_uac_dialogs(invite_request()),

    BobRURI   = remote_target(AliceDialog),
    AliceRURI = remote_target(BobDialog),

    NewBobContact = <<"sip:bob-new@192.0.2.5">>,
    {BobDialog1, InfoFromBob} = ersip_dialog:uac_request(info_sipmsg(#{contact => NewBobContact}), BobDialog),
    {ok, AliceDialogAfter} = ersip_dialog:uas_process(InfoFromBob, regular, AliceDialog),
    AliceInfoResp0 = ersip_sipmsg:reply(200, InfoFromBob),
    NewAliceContact = <<"sip:alice-new@pc34.atlanta.com">>,
    AliceInfoResp = ersip_sipmsg:set(contact, make_contact(NewAliceContact), AliceInfoResp0),
    {ok, BobDialogAfter} = ersip_dialog:uac_trans_result(AliceInfoResp, regular, BobDialog1),

    ?assertEqual(AliceRURI, remote_target(BobDialogAfter)),
    ?assertEqual(BobRURI,   remote_target(AliceDialogAfter)),

    ok.

bad_request_on_bad_contact_test() ->
    {BobDialog,  AliceDialog} = create_uas_uac_dialogs(invite_request()),
    {_, ReInviteFromBob0} = ersip_dialog:uac_request(reinvite_sipmsg(), BobDialog),
    check_400_uas_resp(ersip_sipmsg:set(contact, star, ReInviteFromBob0), AliceDialog),
    check_400_uas_resp(ersip_sipmsg:set(contact, make_contact(<<"unknown:x.y">>), ReInviteFromBob0), AliceDialog),
    ok.

bad_contact_is_ignored_by_uac_test() ->
    {BobDialog,  _AliceDialog} = create_uas_uac_dialogs(invite_request()),
    {BobDialog1, ReInviteFromBob} = ersip_dialog:uac_request(reinvite_sipmsg(), BobDialog),
    AliceReInviteResp0 = ersip_sipmsg:reply(200, ReInviteFromBob),
    AliceReInviteResp = ersip_sipmsg:set(contact, star, AliceReInviteResp0),
    {ok, BobDialog1} = ersip_dialog:uac_trans_result(AliceReInviteResp, target_refresh, BobDialog1),
    ok.

second_provisional_response_test() ->
    %% Check that second provisional response does not change state of
    %% the dialog on UAS side:
    InvReq = invite_request(),
    InvSipMsg = ersip_request:sipmsg(InvReq),
    InvResp180UAS = invite_reply(180, InvSipMsg),
    ?assertMatch({_, _}, ersip_dialog:uas_new(InvSipMsg, InvResp180UAS)),
    {UASDialogEarly, _} = ersip_dialog:uas_new(InvSipMsg, InvResp180UAS),
    ?assertMatch({UASDialogEarly, _}, ersip_dialog:uas_pass_response(InvSipMsg, InvResp180UAS, UASDialogEarly)),
    ok.

uas_check_contact_test() ->
    %% The URI provided in the Contact header field MUST be a SIP or
    %% SIPS URI. If the request that initiated the dialog contained a
    %% SIPS URI in the Request-URI or in the top Record-Route header
    %% field value, if there was any, or the Contact header field if
    %% there was no Record-Route header field, the Contact header
    %% field in the response MUST be a SIPS URI.
    InvSipMsg0 = ersip_request:sipmsg(invite_request()),
    InvSipMsg = ersip_sipmsg:set_ruri(ersip_uri:make(<<"sips:bob@biloxy.com">>), InvSipMsg0),
    InvResp200 = invite_reply(200, InvSipMsg),
    %% 1. Check that we cannot create dialog with SIP URI:
    InvResp200Sip = ersip_sipmsg:set(contact, make_contact(<<"sip:bob@192.0.2.4">>), InvResp200),
    ?assertError({cannot_create_dialog, _}, ersip_dialog:uas_new(InvSipMsg, InvResp200Sip)),
    %% 2. Check that we can create dialog with SIPs URI:
    InvResp200Sips = ersip_sipmsg:set(contact, make_contact(<<"sips:bob@192.0.2.4">>), InvResp200),
    ?assertMatch({_, _}, ersip_dialog:uas_new(InvSipMsg, InvResp200Sips)),
    %% 3. Check that we cannot create dialog with star contact:
    InvResp200Star = ersip_sipmsg:set(contact, star, InvResp200),
    ?assertError({cannot_create_dialog, _}, ersip_dialog:uas_new(InvSipMsg, InvResp200Star)),
    %% 4. Check that we cannot create dialog with star contact in request:
    InvSipMsgStar = ersip_sipmsg:set(contact, star, InvSipMsg),
    ?assertError({cannot_create_dialog, _}, ersip_dialog:uas_new(InvSipMsgStar, InvResp200Sip)),
    %% 5. Check that if top record route contains SIPS RURI then
    %% Contact is checked to be SIPS URI.
    InvSipMsgSIPSRR = set_routes(record_route, [<<"sip:atlanta.com;lr">>, <<"sips:biloxi.com;lr">>], InvSipMsg),
    ?assertError({cannot_create_dialog, _}, ersip_dialog:uas_new(InvSipMsgSIPSRR, InvResp200Sip)),
    %% 6. Check that we can create dialog with SIPS URI:
    InvResp200Sips = ersip_sipmsg:set(contact, make_contact(<<"sips:bob@192.0.2.4">>), InvResp200),
    ?assertMatch({_, _}, ersip_dialog:uas_new(InvSipMsgSIPSRR, InvResp200Sips)),
    %% 7. Bad contact format:
    InvSipMsgBadContct = ersip_request:sipmsg(invite_request(#{contact => <<"@">>})),
    ?assertError({cannot_create_dialog, _}, ersip_dialog:uas_new(InvSipMsgBadContct, InvResp200)),
    ok.

uac_check_contact_test() ->
    %% 12.1.2 UAC Behavior
    %% If the request has a Request-URI or a topmost Route header
    %% field value with a SIPS URI, the Contact header field MUST
    %% contain a SIPS URI.
    InvReq1 = invite_request(#{ruri => <<"sips:bob@biloxi.com">>,
                               contact => <<"sip:alice@pc32.atlanta.com">>}),
    check_new_uac_error(InvReq1),
    InvReq2 = invite_request(#{ruri => <<"sip:bob@biloxi.com">>,
                               contact => <<"sip:alice@pc32.atlanta.com">>,
                               route => [<<"sips:biloxi.com">>]}),
    check_new_uac_error(InvReq2),

    %% Check success consturction:
    InvReq3 = invite_request(#{ruri => <<"sips:bob@biloxi.com">>,
                               contact => <<"sips:alice@pc32.atlanta.com">>}),
    check_new_uac_ok(InvReq3),
    InvReq4 = invite_request(#{ruri => <<"sip:bob@biloxi.com">>,
                               contact => <<"sips:alice@pc32.atlanta.com">>,
                               route => [<<"sips:biloxi.com">>]}),
    check_new_uac_ok(InvReq4),
    ok.

uas_update_after_confirmed_test() ->
    {BobDialog, _} = create_uas_uac_dialogs(invite_request()),
    InvSipMsg = ersip_request:sipmsg(invite_request()),
    Resp200 = invite_reply(200, InvSipMsg),
    ?assertMatch({BobDialog, _RespSipMsg}, ersip_dialog:uas_pass_response(InvSipMsg, Resp200, BobDialog)),
    ok.

is_secure_test() ->
    InvSipMsg = create_sipmsg(invite_request_bin(#{ruri => <<"sips:bob@biloxi.com">>,
                                                   contact => <<"sips:alice@pc32.atlanta.com">>}),
                              tls_source(default_peer()), []),
    Target = ersip_uri:make(<<"sips:127.0.0.1;transport=tls">>),
    InvReq = ersip_request:new(InvSipMsg, ersip_branch:make_random(7), Target),

    InvResp200 = invite_reply(200, InvSipMsg),
    InvResp200Sips = ersip_sipmsg:set(contact, make_contact(<<"sips:bob@192.0.2.4">>), InvResp200),

    %% 12.1.1 UAS behavior
    %% If the request arrived over TLS, and the Request-URI contained
    %% a SIPS URI, the "secure" flag is set to TRUE.
    {BobDialog, InvResp} = ersip_dialog:uas_new(InvSipMsg, InvResp200Sips),
    ?assertEqual(true, ersip_dialog:is_secure(BobDialog)),

    %% 12.1.2 UAC Behavior
    %% If the request was sent over TLS, and the Request-URI contained a
    %% SIPS URI, the "secure" flag is set to TRUE.
    {ok, AliceDialog} = ersip_dialog:uac_new(InvReq, InvResp),
    ?assertEqual(true, ersip_dialog:is_secure(AliceDialog)),

    ok.

check_no_secure_when_on_undefined_source_test() ->
    InvSipMsg = create_sipmsg(invite_request_bin(#{ruri => <<"sips:bob@biloxi.com">>,
                                                   contact => <<"sips:alice@pc32.atlanta.com">>}),
                              undefined, []),

    InvResp200 = invite_reply(200, InvSipMsg),
    InvResp200Sips = ersip_sipmsg:set(contact, make_contact(<<"sips:bob@192.0.2.4">>), InvResp200),

    {BobDialog, _} = ersip_dialog:uas_new(InvSipMsg, InvResp200Sips),
    ?assertEqual(false, ersip_dialog:is_secure(BobDialog)),
    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

-define(crlf, "\r\n").

invite_request() ->
    InvSipMsg = create_sipmsg(invite_request_bin(), make_default_source(), []),
    Target = ersip_uri:make(<<"sip:127.0.0.1">>),
    ersip_request:new(InvSipMsg, ersip_branch:make_random(7), Target).

invite_request(Opts) ->
    InvSipMsg = create_sipmsg(invite_request_bin(Opts), make_default_source(), []),
    Target = ersip_uri:make(<<"sip:127.0.0.1">>),
    ersip_request:new(InvSipMsg, ersip_branch:make_random(7), Target).

invite_request_bin() ->
    invite_request_bin(#{}).

invite_request_bin(Options) ->
    RURI = maps:get(ruri, Options, <<"sip:bob@biloxi.com">>),
    RecordRoute = case Options of
                      #{record_route := RR} ->
                          <<"Record-Route: ", RR/binary, ?crlf>>;
                      _ ->
                          <<>>
                  end,
    Contact = case Options of
                  #{contact := <<>>} ->
                      <<>>;
                  #{contact := ContactVal} ->
                      <<"Contact: ", ContactVal/binary, ?crlf>>;
                  _ ->
                      <<"Contact: <sip:alice@pc33.atlanta.com>", ?crlf>>
              end,
    Route = case Options of
                #{route := Routes} ->
                    IORoutes = [<<"Route: ", R/binary, ?crlf>> || R <- Routes],
                    iolist_to_binary(IORoutes);
                _ ->
                    <<>>
            end,
    <<"INVITE ", RURI/binary, " SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8" ?crlf
      "Max-Forwards: 70" ?crlf
      "To: Bob <sip:bob@biloxi.com>" ?crlf
      "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
      "Call-ID: a84b4c76e66710" ?crlf
      "CSeq: 314159 INVITE" ?crlf,
      Contact/binary,
      RecordRoute/binary,
      Route/binary,
      "Content-Type: application/sdp" ?crlf
      "Content-Length: 4" ?crlf
      ?crlf
      "Test">>.


invite_reply(Code, InvSipMsg) ->
    InvResp = ersip_sipmsg:reply(Code, InvSipMsg),
    ersip_sipmsg:set(contact, make_contact(<<"sip:bob@192.0.2.4">>), InvResp).


create_uas_uac_dialogs(Req) ->
    create_uas_uac_dialogs(Req, fun(_, ReqResp) -> ReqResp end).

create_uas_uac_dialogs(Req, ProxyFun) ->
    InvSipMsg0 = ersip_request:sipmsg(Req),
    InvSipMsg = ProxyFun(request, InvSipMsg0),
    InvResp180UAS = invite_reply(180, InvSipMsg),
    ?assertMatch({_, _}, ersip_dialog:uas_new(InvSipMsg, InvResp180UAS)),
    {UASDialogEarly, InvResp180UAC0} = ersip_dialog:uas_new(InvSipMsg, InvResp180UAS),
    InvResp180UAC = ProxyFun(response, InvResp180UAC0),

    ?assertMatch({ok, _}, ersip_dialog:uac_new(Req, InvResp180UAC)),
    {ok, UACDialogEarly} = ersip_dialog:uac_new(Req, InvResp180UAC),

    InvResp200UAS = invite_reply(200, InvSipMsg),
    {UASDialogConfirmed, _} = ersip_dialog:uas_pass_response(InvSipMsg, InvResp200UAS, UASDialogEarly),
    InvResp200UAC = ProxyFun(response, InvResp200UAS),

    {ok, UACDialogConfirmed} = ersip_dialog:uac_update(InvResp200UAC, UACDialogEarly),
    {UASDialogConfirmed, UACDialogConfirmed}.

bye_sipmsg() ->
    create_sipmsg(bye_bin(), make_default_source(), []).

bye_bin() ->
    <<"BYE sip:bob@biloxi.com SIP/2.0" ?crlf
      "Max-Forwards: 70" ?crlf
      ?crlf>>.

reinvite_sipmsg() ->
    reinvite_sipmsg(#{}).

reinvite_sipmsg(UserOpts) ->
    FullOpts = maps:merge(#{cseq => <<"314160">>,
                            contact => <<"sip:alice@pc33.atlanta.com">>},
                          UserOpts),
    #{cseq    := CSeq,
      contact := ContactOpt
     } = FullOpts,
    Contact = case ContactOpt of
                  <<>> -> <<>>;
                  _ -> <<"Contact: ", ContactOpt/binary, ?crlf>>
              end,
    Bin =
        <<"INVITE sip:bob@biloxi.com SIP/2.0" ?crlf
          "Max-Forwards: 70" ?crlf
          "Content-Type: application/sdp" ?crlf
          "Content-Length: 4" ?crlf,
          Contact/binary,
          "CSeq: ", CSeq/binary, " INVITE" ?crlf
          ?crlf
          "Test">>,
    create_sipmsg(Bin, make_default_source(), []).

info_sipmsg(UserOpts) ->
    FullOpts = maps:merge(#{cseq => <<"314160">>,
                            contact => <<"sip:alice@pc33.atlanta.com">>},
                          UserOpts),
    #{cseq    := CSeq,
      contact := ContactOpt
     } = FullOpts,
    Contact = case ContactOpt of
                  <<>> -> <<>>;
                  _ -> <<"Contact: ", ContactOpt/binary, ?crlf>>
              end,
    Bin =
        <<"INFO sip:bob@biloxi.com SIP/2.0" ?crlf
          "From: Alice <sip:alice@atlanta.com>" ?crlf
          "Max-Forwards: 70" ?crlf,
          Contact/binary,
          "CSeq: ", CSeq/binary, " INFO" ?crlf
          ?crlf>>,
    create_sipmsg(Bin, make_default_source(), []).

ack_sipmsg() ->
    Bin =
        <<"ACK sip:bob@biloxi.com SIP/2.0" ?crlf
          "Max-Forwards: 70" ?crlf
          "Content-Type: application/sdp" ?crlf
          "Content-Length: 4" ?crlf
          "CSeq: 314160 ACK" ?crlf
          ?crlf
          "Test">>,
    create_sipmsg(Bin, make_default_source(), []).

cancel_sipmsg() ->
    Bin =
        <<"CANCEL sip:bob@biloxi.com SIP/2.0" ?crlf
          "Max-Forwards: 70" ?crlf
          "Content-Type: application/sdp" ?crlf
          "Content-Length: 4" ?crlf
          "CSeq: 314160 CANCEL" ?crlf
          ?crlf
          "Test">>,
    create_sipmsg(Bin, make_default_source(), []).

make_default_source() ->
    tcp_source(default_peer()).

default_peer() ->
    {{127, 0, 0, 1}, 5060}.

tcp_source(Peer) ->
    ersip_source:new(Peer, ersip_transport:tcp(), undefined).

tls_source(Peer) ->
    ersip_source:new(Peer, ersip_transport:tls(), undefined).

create_sipmsg(Msg, Source, HeadersToParse) when is_binary(Msg) ->
    P  = ersip_parser:new_dgram(Msg),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg1 = ersip_msg:set_source(Source, PMsg),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg1, HeadersToParse),
    SipMsg.

make_contact(ContactBin) when is_binary(ContactBin) ->
    Contact = ersip_hdr_contact:make(ContactBin),
    [Contact].

clear_tag(H, SipMsg) when H == from; H == to ->
    FromOrTo0 = ersip_sipmsg:get(H, SipMsg),
    FromOrTo = ersip_hdr_fromto:set_tag(undefined, FromOrTo0),
    ersip_sipmsg:set(H, FromOrTo, SipMsg).

cseq_number(SipMsg) ->
    ersip_hdr_cseq:number(ersip_sipmsg:get(cseq, SipMsg)).

set_cseq_number(Seq, Req) ->
    CSeq0 = ersip_sipmsg:get(cseq, Req),
    CSeq  = ersip_hdr_cseq:set_number(Seq, CSeq0),
    ersip_sipmsg:set(cseq, CSeq, Req).

loose_route(request, ReqSipMsg) ->
    %% Add proxy record route:
    RRRoutes = [<<"sip:atlanta.com;lr">>, <<"sip:biloxi.com;lr">>],
    set_routes(record_route, RRRoutes, ReqSipMsg);
loose_route(response, RespSipMsg) ->
    RespSipMsg.

strict_route(request, ReqSipMsg) ->
    %% Add proxy record route:
    RRRoutes = [<<"sip:atlanta.com">>, <<"sip:biloxi.com">>],
    set_routes(record_route, RRRoutes, ReqSipMsg);
strict_route(response, RespSipMsg) ->
    RespSipMsg.

set_routes(Header, Routes, SipMsg) ->
    RRSet0 = ersip_route_set:new(),
    RRSet = add_routes(Routes, RRSet0),
    ersip_sipmsg:set(Header, RRSet, SipMsg).

add_routes([], RouteSet) ->
    RouteSet;
add_routes([URI|Rest], RouteSet0) ->
    Route = ersip_hdr_route:make_route(ersip_uri:make(URI)),
    RouteSet = ersip_route_set:add_first(Route, RouteSet0),
    add_routes(Rest, RouteSet).

remote_target(Dialog) ->
    %% Trick to extract remote target is to send message and get RURI
    %% from it.
    {_, SipMsg} = ersip_dialog:uac_request(reinvite_sipmsg(), Dialog),
    ersip_sipmsg:ruri(SipMsg).

check_400_uas_resp(Req, Dialog) ->
    ?assertMatch({reply, _}, ersip_dialog:uas_process(Req, target_refresh, Dialog)),
    {reply, Resp400} = ersip_dialog:uas_process(Req, target_refresh, Dialog),
    ?assertEqual(400, ersip_sipmsg:status(Resp400)).

check_new_uac_error(Req) ->
    InvSipMsg = ersip_request:sipmsg(Req),
    InvResp200 = invite_reply(200, InvSipMsg),
    ?assertMatch({error, _}, ersip_dialog:uac_new(Req, InvResp200)).

check_new_uac_ok(Req) ->
    InvSipMsg = ersip_request:sipmsg(Req),
    InvResp200 = invite_reply(200, InvSipMsg),
    ?assertMatch({ok, _}, ersip_dialog:uac_new(Req, InvResp200)).

del_cseq(SipMsg) ->
    ersip_sipmsg:remove(cseq, SipMsg).
