%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Registrar tests
%%

-module(ersip_request_cancel_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

generate_cancel_test() ->
    InviteReq = invite_req(#{}),
    CancelReq = ersip_request_cancel:generate(InviteReq),
    ?assertEqual(ersip_request:branch(InviteReq),  ersip_request:branch(CancelReq)),
    ?assertEqual(ersip_request:nexthop(InviteReq), ersip_request:nexthop(CancelReq)),
    CancelMsg = ersip_request:sipmsg(CancelReq),
    InviteMsg = ersip_request:sipmsg(InviteReq),

    %% 0. Check that method is set to CANCEL
    ?assertEqual(ersip_method:cancel(), ersip_sipmsg:method(CancelMsg)),
    %% The Request-URI, Call-ID, To, the numeric part of CSeq, and From header
    %% fields in the CANCEL request MUST be identical to those in the
    %% request being cancelled, including tags.
    %%
    %% 1. Request-URI:
    ?assertEqual(ersip_sipmsg:ruri(InviteMsg), ersip_sipmsg:ruri(CancelMsg)),
    %% 2. Call-ID:
    ?assertEqual(ersip_sipmsg:get(callid, InviteMsg), ersip_sipmsg:get(callid, CancelMsg)),
    %% 3. To:
    ?assertEqual(ersip_sipmsg:get(to, InviteMsg), ersip_sipmsg:get(to, CancelMsg)),
    %% 4. The numeric part of CSeq:
    InviteCSEQ = ersip_sipmsg:get(cseq, InviteMsg),
    CancelCSEQ = ersip_sipmsg:get(cseq, CancelMsg),
    ?assertEqual(ersip_hdr_cseq:number(InviteCSEQ), ersip_hdr_cseq:number(CancelCSEQ)),
    %% 5. From
    ?assertEqual(ersip_sipmsg:get(from, InviteMsg), ersip_sipmsg:get(from, CancelMsg)),

    %% If the request being cancelled contains a Route header field, the
    %% CANCEL request MUST include that Route header field's values.
    ?assertEqual(ersip_sipmsg:get(route, InviteMsg), ersip_sipmsg:get(route, CancelMsg)),

    %% The CANCEL request MUST NOT contain any Require or Proxy-Require
    %% header fields.
    ?assertEqual(not_found, ersip_sipmsg:find(require, CancelMsg)),
    ?assertEqual(not_found, ersip_sipmsg:find(proxy_require, CancelMsg)),

    %% Max forwards (not specified by RFC 3261):
    ?assertEqual(ersip_sipmsg:get(maxforwards, InviteMsg), ersip_sipmsg:get(maxforwards, CancelMsg)),

    ok.

%%%===================================================================
%%% Helpers
%%%===================================================================

invite_req(Options) ->
    InviteSipMsg = parse_message(invite_bin(Options)),
    Branch = ersip_branch:make(<<"inviteTrans">>),
    Nexthop = ersip_uri:make(<<"sip:biloxi.com">>),
    ersip_request:new(InviteSipMsg, Branch, Nexthop).

-define(crlf, "\r\n").

invite_bin(_Options) ->
    <<"INVITE sip:bob@biloxi.com SIP/2.0" ?crlf
      "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bKnashds8" ?crlf
      "Max-Forwards: 53" ?crlf
      "To: Bob <sip:bob@biloxi.com>" ?crlf
      "From: Alice <sip:alice@atlanta.com>;tag=1928301774" ?crlf
      "Call-ID: a84b4c76e66710" ?crlf
      "Route: <sip:server10.biloxi.com;lr>," ?crlf %% Need route here to verify ACK generation
      " <sip:bigbox3.site3.atlanta.com;lr>" ?crlf
      "CSeq: 314159 INVITE" ?crlf
      "Contact: <sip:alice@pc33.atlanta.com>" ?crlf
      "Content-Type: application/sdp" ?crlf
      "Content-Length: 0" ?crlf
      ?crlf>>.

parse_message(Bin) when is_binary(Bin) ->
    P  = ersip_parser:new_dgram(Bin),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    {ok, SipMsg} = ersip_sipmsg:parse(PMsg, all),
    SipMsg.
