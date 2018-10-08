%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP dialog support
%%
%% Initial requests:
%%    UAC:
%%      1. Call uac_new when response with code 101..299 is received on request
%%      2. Call uac_update when another response is received on initial request
%%
%%    UAS:
%%      1. Call uas_verify/1 to check that is well-formed
%%      2. Pass for processing and get response
%%      3. Call uas_new/2 to create server-side transaction
%%      4. Call uas_pass_response/2 for each next response.
%%
%% In-dialog requests:
%%    UAC:
%%      1. Call uac_request to make request matching dialog to send
%%      2. Call uac_trans_result when response on dialog request is received
%%
%%    UAS:
%%      1. Call uas_dialog_id to get dialog identifier and find dialog state
%%      2. Call uas_process to update dialog state related to received request.
%%
%% Request types:
%%   1. Regular
%%   2. Target refresher - request that can change remote target of the dialog
%%      for INVITE method it is only INVITE itself.
%%      for subscriptions it is SUBSCRIBE and NOTIFY methods
%%

-module(ersip_dialog).

-export([id/1,
         uas_verify/1,
         uas_new/2,
         uas_pass_response/3,
         uac_new/2,
         uac_update/2,
         uac_request/2,
         uac_trans_result/3,
         uas_dialog_id/1,
         uas_process/3,
         remote_seq/1,
         is_secure/1
        ]).

-export_type([dialog/0,
              id/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(dialog, {callid              :: ersip_hdr_callid:callid(),
                 local_tag           :: ersip_hdr_fromto:tag(),
                 local_uri           :: ersip_uri:uri(),
                 local_seq  = empty  :: ersip_hdr_cseq:cseq_num() | empty,
                 remote_tag          :: ersip_hdr_fromto:tag() | undefined,
                 remote_uri          :: ersip_uri:uri(),
                 remote_seq = empty  :: ersip_hdr_cseq:cseq_num() | empty,
                 remote_target       :: ersip_uri:uri(),
                 secure              :: boolean(),
                 route_set           :: ersip_route_set:route_set(),
                 state               :: state()
                }).
-record(dialog_id, {local_tag  :: ersip_hdr_fromto:tag_key(),
                    remote_tag :: ersip_hdr_fromto:tag_key() | undefined,
                    callid     :: ersip_hdr_callid:callid()
                   }).

-type dialog() :: #dialog{}.
-type id()     :: #dialog_id{}.
-type state() :: early | confirmed.
-type request_type() :: target_refresh
                      | regular.

-type cc_check_fun() :: fun((ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> cc_check_result()).
-type cc_check_result() :: ok | {error, term()}.
-type uas_process_result() :: {ok, dialog()}
                            | {reply, ersip_sipmsg:sipmsg()}.

-type uas_result() :: {dialog(), ersip_sipmsg:sipmsg()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Unique identifier of the dialog.
-spec id(dialog()) -> id().
id(#dialog{callid = CallId, local_tag = LocalTag, remote_tag = RemoteTag}) ->
    %% RFC3261: Secion 12:
    %%
    %% A dialog is identified at each UA with a dialog ID, which consists
    %% of a Call-ID value, a local tag and a remote tag.
    RemoteTagKey =
        case RemoteTag of
            undefined ->
                undefined;
            _ ->
                ersip_hdr_fromto:tag_key(RemoteTag)
        end,
    #dialog_id{local_tag  = ersip_hdr_fromto:tag_key(LocalTag),
               remote_tag = RemoteTagKey,
               callid     = ersip_hdr_callid:make_key(CallId)
              }.

-spec uas_verify(ersip_sipmsg:sipmsg()) -> ok | {reply, ersip_sipmsg:sipmsg()}.
uas_verify(ReqSipMsg) ->
    case check_contact(ReqSipMsg) of
        ok ->
            case check_record_route(ReqSipMsg) of
                ok ->
                    ok;
                {error, {bad_record_route, _}} ->
                    Reply = ersip_reply:new(400, [{reason, <<"Invalid Record-Route">>}]),
                    {reply, ersip_sipmsg:reply(Reply, ReqSipMsg)}
            end;
        {error, multiple_contact_forbidden} ->
            Reply = ersip_reply:new(400, [{reason, <<"Invalid Too Many Contacts">>}]),
            {reply, ersip_sipmsg:reply(Reply, ReqSipMsg)};
        {error, invalid_star_contact} ->
            Reply = ersip_reply:new(400, [{reason, <<"Star Contact Forbidden">>}]),
            {reply, ersip_sipmsg:reply(Reply, ReqSipMsg)};
        {error, contact_required} ->
            Reply = ersip_reply:new(400, [{reason, <<"No Contact Specified">>}]),
            {reply, ersip_sipmsg:reply(Reply, ReqSipMsg)}
    end.

%% @doc New dialog on UAS side.
%%
%% Function get request that used to create dialog and preliminary
%% response to this request. As result new dialog is created and
%% updated response is returned.
%%
%% Implements 12.1.1 UAS behavior
-spec uas_new(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> uas_result().
uas_new(Request, Response) ->
    %% Check request/response pair can create dialog.
    case uas_can_create_dialog(Request, Response) of
        {error, Reason} ->
            error({cannot_create_dialog, Reason});
        ok ->
            ok
    end,
    OutResponse = uas_update_response(Request, Response),
    Dialog = uas_create(Request, Response),
    {Dialog, OutResponse}.

-spec uas_pass_response(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg(), dialog()) -> uas_result().
uas_pass_response(ReqSipMsg, RespSipMsg, #dialog{state = confirmed} = Dialog) ->
    UpdatedResp = uas_update_response(ReqSipMsg, RespSipMsg),
    {Dialog, UpdatedResp};
uas_pass_response(ReqSipMsg, RespSipMsg, #dialog{state = early} = Dialog) ->
    State = state_by_response(RespSipMsg),
    UpdatedResp = uas_update_response(ReqSipMsg, RespSipMsg),
    case State of
        early ->
            {Dialog, UpdatedResp};
        confirmed ->
            {Dialog#dialog{state = confirmed}, UpdatedResp}
    end.

%% @doc New dialog on UAC side.
%%
%% Implements 12.1.2 UAC behavior
-spec uac_new(ersip_request:request(), ersip_sipmsg:sipmsg()) -> {ok, dialog()} | {error, term()}.
uac_new(Req, Response) ->
    case uac_can_create_dialog(ersip_request:sipmsg(Req), Response) of
        {error, _} = Err ->
            Err;
        ok ->
            Dialog = uac_create(Req, Response),
            {ok, Dialog}
    end.

-spec uac_update(ersip_sipmsg:sipmsg(), dialog()) -> {ok, dialog()} | terminate_dialog.
uac_update(Response, #dialog{} = Dialog) ->
    uac_trans_result(Response, target_refresh, Dialog).

%% 12.2.1 UAC Behavior
%% 12.2.1.1 Generating the Request
-spec uac_request(ersip_sipmsg:sipmsg(), dialog()) -> {dialog(), ersip_sipmsg:sipmsg()}.
uac_request(Req0, #dialog{} = Dialog0) ->
    %% The URI in the To field of the request MUST be set to the
    %% remote URI from the dialog state.  The tag in the To header
    %% field of the request MUST be set to the remote tag of the
    %% dialog ID.
    #dialog{remote_uri = RemoteURI, remote_tag = RemoteTag} = Dialog0,
    To0   = get_or_create(to, Req0),
    To1   = ersip_hdr_fromto:set_uri(RemoteURI, To0),
    To    = ersip_hdr_fromto:set_tag(RemoteTag, To1),
    Req1  = ersip_sipmsg:set(to, To, Req0),
    %% The From URI of the request MUST be set to the local URI from
    %% the dialog state.  The tag in the From header field of the
    %% request MUST be set to the local tag of the dialog ID.
    #dialog{local_uri = LocalURI, local_tag = LocalTag} = Dialog0,
    From0 = get_or_create(from, Req0),
    From1 = ersip_hdr_fromto:set_uri(LocalURI, From0),
    From  = ersip_hdr_fromto:set_tag(LocalTag, From1),
    Req2  = ersip_sipmsg:set(from, From, Req1),
    %% The Call-ID of the request MUST be set to the Call-ID of the dialog.
    Req3  = ersip_sipmsg:set(callid, Dialog0#dialog.callid, Req2),
    Req4 = maybe_update_cseq(Req0, Dialog0, Req3),
    CSeq = ersip_sipmsg:get(cseq, Req4),
    Dialog1 = Dialog0#dialog{local_seq = ersip_hdr_cseq:number(CSeq)},
    %% The UAC uses the remote target and route set to build the
    %% Request-URI and Route header field of the request.
    #dialog{remote_target = RemoteTarget, route_set = RouteSet} = Dialog1,
    Req5 = fill_request_route(RemoteTarget, RouteSet, Req4),
    {Dialog1, Req5}.

%% 12.2.1 UAC Behavior
%% 12.2.1.2 Processing the Responses
-spec uac_trans_result(ersip_sipmsg:sipmsg() | timeout, request_type(), dialog()) -> {ok, dialog()} | terminate_dialog.
uac_trans_result(timeout, _, #dialog{}) ->
    %% First: The UAC will receive responses to the request from the
    %% transaction layer.  If the client transaction returns a
    %% timeout, this is treated as a 408 (Request Timeout) response.
    %%
    %% Second: If the response for a request within a dialog is a 481
    %% (Call/Transaction Does Not Exist) or a 408 (Request Timeout),
    %% the UAC SHOULD terminate the dialog.
    terminate_dialog;
uac_trans_result(Resp, ReqType, #dialog{} = Dialog) ->
    case ersip_sipmsg:status(Resp) of
        Code when Code >= 200 andalso Code =< 299 andalso ReqType == target_refresh ->
            %% When a UAC receives a 2xx response to a target refresh
            %% request, it MUST replace the dialog's remote target URI
            %% with the URI from the Contact header field in that
            %% response, if present.
            Dialog1 = Dialog#dialog{state = confirmed},
            case ersip_sipmsg:find(contact, Resp) of
                not_found ->
                    {ok, Dialog1};
                {ok, [Contact]} ->
                    {ok, Dialog1#dialog{remote_target = ersip_hdr_contact:uri(Contact)}};
                _ ->
                    %% In all other cases contact is invalid and we ignore it
                    {ok, Dialog1}
            end;
        481 ->
            %% If the response for a request within a dialog is a 481
            %% (Call/Transaction Does Not Exist) or a 408 (Request
            %% Timeout), the UAC SHOULD terminate the dialog
            terminate_dialog;
        408 ->
            terminate_dialog;
        _ ->
            {ok, Dialog}
    end.


%% 12.2.2 UAS Behavior
-spec uas_dialog_id(ersip_sipmsg:sipmsg()) -> {ok, id()} | no_dialog.
uas_dialog_id(RequestSipMsg) ->
    %% If the request has a tag in the To header field, the UAS core
    %% computes the dialog identifier corresponding to the request and
    %% compares it with existing dialogs.  If there is a match, this
    %% is a mid-dialog request.
    To     = ersip_sipmsg:get(to, RequestSipMsg),
    From   = ersip_sipmsg:get(from, RequestSipMsg),
    CallID = ersip_sipmsg:get(callid, RequestSipMsg),
    case ersip_hdr_fromto:tag_key(To) of
        undefined ->
            no_dialog;
        LocalTag ->
            RemoteTag = ersip_hdr_fromto:tag_key(From),
            {ok, #dialog_id{callid = CallID,
                            local_tag = LocalTag,
                            remote_tag = RemoteTag}}
    end.

-spec uas_process(ersip_sipmsg:sipmsg(), request_type(), dialog()) -> uas_process_result().
uas_process(RequestSipMsg, ReqType, #dialog{remote_seq = empty} = Dialog0) ->
    %% If the remote sequence number is empty, it MUST be set to the value
    %% of the sequence number in the CSeq header field value in the request.
    RemoteCSeq = ersip_sipmsg:get(cseq, RequestSipMsg),
    RemoteCSeqNum = ersip_hdr_cseq:number(RemoteCSeq),
    Dialog1 = Dialog0#dialog{remote_seq = RemoteCSeqNum},
    uas_maybe_update_target(RequestSipMsg, ReqType, Dialog1);
uas_process(RequestSipMsg, ReqType,  #dialog{remote_seq = StoredRCSeq} = Dialog0) ->
    %% If the remote sequence number was not empty, but the sequence number
    %% of the request is lower than the remote sequence number, the request
    %% is out of order and MUST be rejected with a 500 (Server Internal
    %% Error) response.  If the remote sequence number was not empty, and
    %% the sequence number of the request is greater than the remote
    %% sequence number, the request is in order
    RemoteCSeq = ersip_sipmsg:get(cseq, RequestSipMsg),
    RemoteCSeqNum = ersip_hdr_cseq:number(RemoteCSeq),
    case RemoteCSeqNum =< StoredRCSeq of
        true ->
            {reply, ersip_sipmsg:reply(500, RequestSipMsg)};
        false ->
            %% Note that RFC 3261 has clause:
            %%
            %% Requests sent within a dialog, as any other requests,
            %% are atomic.  If a particular request is accepted by the
            %% UAS, all the state changes associated with it are
            %% performed.  If the request is rejected, none of the
            %% state changes are performed.
            %%
            %% So to be fully conformant we need to update remote_seq
            %% iff response on request is 2xx. But I've checked all
            %% major implementations (pjsip, sofia sip, exosip). All
            %% of them update remote_seq after the check and do not
            %% care about response.
            Dialog1 = Dialog0#dialog{remote_seq = RemoteCSeqNum},
            uas_maybe_update_target(RequestSipMsg, ReqType, Dialog1)
    end.

-spec remote_seq(dialog()) -> ersip_hdr_cseq:cseq_num() | empty.
remote_seq(#dialog{remote_seq = RCSeq}) ->
    RCSeq.

-spec is_secure(dialog()) -> boolean().
is_secure(#dialog{secure = Secure}) ->
    Secure.

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

-spec uas_create(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> dialog().
uas_create(Request, Response) ->
    %% If the request arrived over TLS, and the Request-URI contained
    %% a SIPS URI, the "secure" flag is set to TRUE.
    IsSecure =
        case ersip_sipmsg:source(Request) of
            undefined -> false;
            Source ->
                SIPSRURI = (ersip_uri:scheme(ersip_sipmsg:ruri(Request)) == {scheme, sips}),
                ersip_source:is_tls(Source) andalso SIPSRURI
        end,
    %% The route set MUST be set to the list of URIs in the
    %% Record-Route header field from the request, taken in order and
    %% preserving all URI parameters.  If no Record-Route header field
    %% is present in the request, the route set MUST be set to the
    %% empty set.
    RouteSet =
        case ersip_sipmsg:find(record_route, Request) of
            not_found ->
                ersip_route_set:new();
            {ok, Set} ->
                Set
        end,

    ReqCSeq      = ersip_sipmsg:get(cseq,    Request),
    ReqFrom      = ersip_sipmsg:get(from,    Request),
    ReqTo        = ersip_sipmsg:get(to,      Request),
    [ReqContact] = ersip_sipmsg:get(contact, Request),
    RespTo       = ersip_sipmsg:get(to,      Response),
    State        = state_by_response(Response),

    #dialog{secure     = IsSecure,
            route_set  = RouteSet,
            %% The remote target MUST be set to the URI
            %% from the Contact header field of the request.
            remote_target = ersip_hdr_contact:uri(ReqContact),
            %% The local sequence number MUST be empty.
            local_seq  = empty,
            %% The remote sequence number MUST be set to the value of the
            %% sequence number in the CSeq header field of the request.  The
            %% local sequence number MUST be empty.
            remote_seq = ersip_hdr_cseq:number(ReqCSeq),
            %% The call identifier component of the dialog ID
            %% MUST be set to the value of the Call-ID in the request.
            callid     = ersip_sipmsg:get(callid, Request),
            %% The local tag component of the dialog ID MUST be
            %% set to the tag in the To field in the response to
            %% the request (which always includes a tag)
            local_tag  = ersip_hdr_fromto:tag(RespTo),
            %% the remote tag component of the dialog ID MUST be
            %% set to the tag from the From field in the request
            remote_tag = ersip_hdr_fromto:tag(ReqFrom),
            %% The remote URI MUST be set to the URI in the From field
            remote_uri = ersip_hdr_fromto:uri(ReqFrom),
            %% local URI MUST be set to the URI in the To field
            local_uri  = ersip_hdr_fromto:uri(ReqTo),
            %%
            state = State
           }.

-spec uac_create(ersip_request:request(), ersip_sipmsg:sipmsg()) -> dialog().
uac_create(Request, Response) ->
    ReqSipMsg = ersip_request:sipmsg(Request),
    %% If the request was sent over TLS, and the Request-URI contained
    %% a SIPS URI, the "secure" flag is set to TRUE.
    NextHop  = ersip_request:nexthop(Request),
    Transport = ersip_transport:make_by_uri(NextHop),
    SIPSRURI = (ersip_uri:scheme(ersip_sipmsg:ruri(ReqSipMsg)) == {scheme, sips}),
    IsSecure = ersip_transport:is_tls(Transport) andalso SIPSRURI,

    %% The route set MUST be set to the list of URIs in the
    %% Record-Route header field from the response, taken in reverse
    %% order and preserving all URI parameters.  If no Record-Route
    %% header field is present in the response, the route set MUST be
    %% set to the empty set.
    RouteSet =
        case ersip_sipmsg:find(record_route, Response) of
            not_found ->
                ersip_route_set:new();
            {ok, Set} ->
                ersip_route_set:reverse(Set)
        end,
    [RespContact] = ersip_sipmsg:get(contact, Response),
    RespTo        = ersip_sipmsg:get(to,      Response),

    ReqCseq       = ersip_sipmsg:get(cseq,    ReqSipMsg),
    ReqFrom       = ersip_sipmsg:get(from,    ReqSipMsg),
    ReqTo         = ersip_sipmsg:get(to,      ReqSipMsg),

    #dialog{secure    = IsSecure,
            route_set = RouteSet,
            %% The remote target MUST be set to the URI from the
            %% Contact header field of the response.
            remote_target = ersip_hdr_contact:uri(RespContact),
            %% The local sequence number MUST be set to the value of the sequence
            %% number in the CSeq header field of the request.
            local_seq    = ersip_hdr_cseq:number(ReqCseq),
            %% The remote sequence number MUST be empty
            remote_seq   = empty,
            %% The call identifier component of the dialog ID MUST be
            %% set to the value of the Call-ID in the request.
            callid       = ersip_sipmsg:get(callid, ReqSipMsg),
            %% The local tag component of the dialog ID MUST be set to
            %% the tag in the From field in the request,
            local_tag    = ersip_hdr_fromto:tag(ReqFrom),
            %% the remote tag component of the dialog ID MUST be set
            %% to the tag in the To field of the response
            remote_tag   = ersip_hdr_fromto:tag(RespTo),
            %% The remote URI MUST be set to the URI in the To field
            remote_uri   = ersip_hdr_fromto:uri(ReqTo),
            %% the local URI MUST be set to the URI in the From field.
            local_uri    = ersip_hdr_fromto:uri(ReqFrom),
            %%
            state = state_by_response(Response)
           }.

-spec uas_update_response(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
uas_update_response(Request, Response) ->
    %% When a UAS responds to a request with a response that
    %% establishes a dialog (such as a 2xx to INVITE), the UAS MUST
    %% copy all Record-Route header field values from the request into
    %% the response (including the URIs, URI parameters, and any
    %% Record-Route header field parameters, whether they are known or
    %% unknown to the UAS) and MUST maintain the order of those
    %% values. The UAS MUST add a Contact header field to the
    %% response.
    ersip_sipmsg:copy(record_route, Request, Response).

-spec uas_can_create_dialog(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> cc_check_result().
uas_can_create_dialog(Request, Response) ->
    check_all([fun cc_check_contact/2,
               fun cc_check_record_route/2,
               fun cc_check_uas_sips/2
              ], Request, Response).

-spec check_all([cc_check_fun()], ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> cc_check_result().
check_all([], _, _) ->
    ok;
check_all([F | Rest], Request, Response) ->
    case F(Request, Response) of
        ok ->
            check_all(Rest, Request, Response);
        {error, _} = Err ->
            Err
    end.

-spec cc_check_contact(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> cc_check_result().
cc_check_contact(Request, Response) ->
    case check_contact(Request) of
        {error, Reason} ->
            {error, {request_contact, Reason}};
        ok ->
            case check_contact(Response) of
                ok ->
                    ok;
                {error, Reason} ->
                    {error, {response_contact, Reason}}
            end
    end.


-spec check_contact(ersip_sipmsg:sipmsg()) -> cc_check_result().
check_contact(SipMsg) ->
    %% 8.1.1.8 Contact
    %%
    %% The Contact header field MUST be present and contain
    %% exactly one SIP or SIPS URI in any request that can
    %% result in the establishment of a dialog.
    case ersip_sipmsg:find(contact, SipMsg) of
        {ok, [_]} ->
            ok;
        {ok, List} when is_list(List) ->
            {error, multiple_contact_forbidden};
        {ok, star} ->
            {error, invalid_star_contact};
        not_found ->
            {error, contact_required};
        {error, _} = Err ->
            Err
    end.

-spec cc_check_record_route(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> cc_check_result().
cc_check_record_route(Request, _) ->
    check_record_route(Request).

-spec check_record_route(ersip_sipmsg:sipmsg()) -> ok | {error, {bad_record_route, term()}}.
check_record_route(Request) ->
    case ersip_sipmsg:find(record_route, Request) of
        {ok, _} ->
            ok;
        not_found ->
            ok;
        {error, Reason} ->
            {error, {bad_record_route, Reason}}
    end.

%% @private
%%
%% @doc Check that response contact is compatible by scheme with next hop (proxy or target)
-spec cc_check_uas_sips(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> cc_check_result().
cc_check_uas_sips(Request, Response) ->
    %% If the request that initiated the dialog contained a SIPS URI
    %% in the Request-URI or in the top Record-Route header field
    %% value, if there was any, or the Contact header field if there
    %% was no Record-Route header field, the Contact header field in
    %% the response MUST be a SIPS URI.
    SIPSRURI = ersip_uri:scheme(ersip_sipmsg:ruri(Request)) == {scheme, sips},
    SIPSNextHop =
        case ersip_sipmsg:find(record_route, Request) of
            not_found ->
                %% Note: contact is checked before.
                [Contact] = ersip_sipmsg:get(contact, Request),
                ersip_uri:scheme(ersip_hdr_contact:uri(Contact)) == {scheme, sips};
            {ok, RRSet} ->
                TopRR = ersip_route_set:first(RRSet),
                ersip_uri:scheme(ersip_hdr_route:uri(TopRR)) == {scheme, sips}
        end,
    case SIPSRURI orelse SIPSNextHop of
        true ->
            %% Note: contact is checked before.
            [RespContact] = ersip_sipmsg:get(contact, Response),
            case ersip_uri:scheme(ersip_hdr_contact:uri(RespContact)) of
                {scheme, sips} ->
                    ok;
                Scheme ->
                    {error, {response_contact_must_be_sips, Scheme}}
            end;
        false ->
            ok
    end.

%% @private
-spec uac_can_create_dialog(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> cc_check_result().
uac_can_create_dialog(Request, Response) ->
    %% When a UAC sends a request that can establish a dialog (such as
    %% an INVITE) it MUST provide a SIP or SIPS URI with global scope
    %% (i.e., the same SIP URI can be used in messages outside this
    %% dialog) in the Contact header field of the request.
    check_all([fun cc_check_contact/2,
               fun cc_check_uac_sips/2
              ], Request, Response).

-spec cc_check_uac_sips(ersip_sipmsg:sipmsg(), ersip_sipmsg:sipmsg()) -> cc_check_result().
cc_check_uac_sips(Request, _Response) ->
    %% If the request has a Request-URI or a topmost Route header field
    %% value with a SIPS URI, the Contact header field MUST contain a SIPS
    %% URI.
    SIPSRURI = ersip_uri:scheme(ersip_sipmsg:ruri(Request)) == {scheme, sips},
    SIPSRoute =
        case ersip_sipmsg:find(route, Request) of
            not_found ->
                %% Note: contact is checked before.
                false;
            {ok, RRSet} ->
                TopRR = ersip_route_set:first(RRSet),
                ersip_uri:scheme(ersip_hdr_route:uri(TopRR)) == {scheme, sips}
        end,
    %% Note: contact is checked before.
    case SIPSRURI orelse SIPSRoute of
        true ->
            [ReqContact] = ersip_sipmsg:get(contact, Request),
            case ersip_uri:scheme(ersip_hdr_contact:uri(ReqContact)) of
                {scheme, sips} ->
                    ok;
                Scheme ->
                    {error, {request_contact_must_be_sips, Scheme}}
            end;
        false ->
            ok
    end.

-spec state_by_response(ersip_sipmsg:sipmsg()) -> state().
state_by_response(RespSipMsg) ->
    case ersip_status:response_type(ersip_sipmsg:status(RespSipMsg)) of
        provisional ->
            early;
        final ->
            confirmed
    end.


-spec get_or_create(ersip_siphdr:known_header(), ersip_sipmsg:sipmsg()) -> term().
get_or_create(cseq, SipMsg) ->
    case ersip_sipmsg:find(cseq, SipMsg) of
        {ok, V} ->
            V;
        not_found ->
            ersip_hdr_cseq:new(ersip_sipmsg:method(SipMsg))
    end;
get_or_create(FromTo, SipMsg)
  when FromTo == from orelse FromTo == to ->
    case ersip_sipmsg:find(FromTo, SipMsg) of
        {ok, V} ->
            V;
        not_found ->
            ersip_hdr_fromto:new()
    end.

-spec fill_request_route(ersip_uri:uri(), ersip_route_set:route_set(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
fill_request_route(RemoteTarget, RouteSet, ReqSipMsg) ->
    case ersip_route_set:is_empty(RouteSet) of
        true ->
            %% If the route set is empty, the UAC MUST place the
            %% remote target URI into the Request-URI.  The UAC MUST
            %% NOT add a Route header field to the request.
            ersip_sipmsg:set_ruri(RemoteTarget, ReqSipMsg);
        false ->
            FirstRoute = ersip_route_set:first(RouteSet),
            case ersip_hdr_route:is_loose_route(FirstRoute) of
                true ->
                    fill_request_loose_route(RemoteTarget, RouteSet, ReqSipMsg);
                false ->
                    fill_request_strict_route(RemoteTarget, RouteSet, ReqSipMsg)
            end
    end.

%% If the route set is not empty, and the first URI in the route set
%% contains the lr parameter (see Section 19.1.1),
-spec fill_request_loose_route(ersip_uri:uri(), ersip_route_set:route_set(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
fill_request_loose_route(RemoteTarget, RouteSet, Req0) ->
    %% the UAC MUST place the remote target URI into the Request-URI
    Req1 = ersip_sipmsg:set_ruri(RemoteTarget, Req0),
    %% and MUST include a Route header field containing the route set
    %% values in order, including all parameters.
    ersip_sipmsg:set(route, RouteSet, Req1).

%% If the route set is not empty, and its first URI does not contain
%% the lr parameter
-spec fill_request_strict_route(ersip_uri:uri(), ersip_route_set:route_set(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
fill_request_strict_route(RemoteTarget, RouteSet0, Req0) ->
    FirstRoute = ersip_route_set:first(RouteSet0),
    %% the UAC MUST place the first URI from the route set into the
    %% Request-URI, stripping any parameters that are not allowed in a
    %% Request-URI.
    CleanURI = ersip_uri:clear_not_allowed_parts(ruri, ersip_hdr_route:uri(FirstRoute)),
    Req1 = ersip_sipmsg:set_ruri(CleanURI, Req0),
    %% The UAC MUST add a Route header field containing the remainder
    %% of the route set values in order, including all parameters.  The
    %% UAC MUST then place the remote target URI into the Route header
    %% field as the last value.
    RouteSet1 = ersip_route_set:remove_first(RouteSet0),
    RURIRoute = ersip_hdr_route:make_route(RemoteTarget),
    RouteSet = ersip_route_set:add_last(RURIRoute, RouteSet1),
    ersip_sipmsg:set(route, RouteSet, Req1).

-spec uas_maybe_update_target(ersip_sipmsg:sipmsg(), request_type(), dialog()) -> uas_process_result().
uas_maybe_update_target(_, regular, #dialog{} = Dialog) ->
    {ok, Dialog};
uas_maybe_update_target(ReqSipMsg, target_refresh, #dialog{} = Dialog) ->
    %% When a UAS receives a target refresh request, it MUST replace the
    %% dialog's remote target URI with the URI from the Contact header field
    %% in that request, if present.
    case ersip_sipmsg:find(contact, ReqSipMsg) of
        not_found ->
            {ok, Dialog};
        {ok, [Contact]} ->
            ContactURI = ersip_hdr_contact:uri(Contact),
            case ersip_uri:scheme(ContactURI) of
                {scheme, S} when S == sip orelse S == sips ->
                    {ok, Dialog#dialog{remote_target = ContactURI}};
                _ ->
                    Reply = ersip_reply:new(400, [{reason, <<"Invalid Contact URI scheme">>}]),
                    {reply, ersip_sipmsg:reply(Reply, ReqSipMsg)}
            end;
        _ ->
            %% star case and many contacs/
            Reply = ersip_reply:new(400, [{reason, <<"Invalid Contact">>}]),
            {reply, ersip_sipmsg:reply(Reply, ReqSipMsg)}
    end.

%% Normative:
%% 12.2.1.1 Generating the Request
%% Requests within a dialog MUST contain strictly monotonically
%% increasing and contiguous CSeq sequence numbers (increasing-by-one)
%% in each direction (excepting ACK and CANCEL of course, whose
%% numbers equal the requests being acknowledged or cancelled).
%% Therefore, if the local sequence number is not empty, the value of
%% the local sequence number MUST be incremented by one, and this
%% value MUST be placed into the CSeq header field.  If the local
%% sequence number is empty, an initial value MUST be chosen using the
%% guidelines of Section 8.1.1.5.  The method field in the CSeq header
%% field value MUST match the method of the request.
%%
%% Implementation: we do not touch ACK and CANCEL sequence number if
%% they are in order.
-spec maybe_update_cseq(ersip_sipmsg:sipmsg(), dialog(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
maybe_update_cseq(InReq, #dialog{local_seq = empty}, Req) ->
    CSeq = get_or_create(cseq, InReq),
    ersip_sipmsg:set(cseq, CSeq, Req);
maybe_update_cseq(InReq, #dialog{local_seq = LocalSeq}, Req) ->
    ACK = ersip_method:ack(),
    CANCEL = ersip_method:cancel(),
    case ersip_sipmsg:method(InReq) of
        Method when Method == ACK orelse Method == CANCEL ->
            case ersip_sipmsg:find(cseq, InReq) of
                {ok, CSeq} ->
                    ersip_sipmsg:set(cseq, CSeq, Req);
                not_found ->
                    set_cseq(InReq, LocalSeq-1, Req)
            end;
        _ ->
            set_cseq(InReq, LocalSeq, Req)
    end.

-spec set_cseq(ersip_sipmsg:sipmsg(), pos_integer(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
set_cseq(InReq, LocalSeq, Req) ->
    CSeq0  = get_or_create(cseq, InReq),
    CSeqNum = ersip_hdr_cseq:number(CSeq0),
    CSeq = case LocalSeq >= CSeqNum of
               true ->
                   ersip_hdr_cseq:set_number(LocalSeq+1, CSeq0);
               _ ->
                   CSeq0
           end,
    ersip_sipmsg:set(cseq, CSeq, Req).
