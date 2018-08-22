%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Identifiers test
%%

-module(ersip_proxy_stateless_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Cases
%%%===================================================================

-define(crlf, "\r\n").

branch_generation_rfc3261_test() ->
    Msg1 = <<"INVITE sip:bob@biloxi.com SIP/2.0"
             ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
             ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
             ?crlf "Max-Forwards: 70"
             ?crlf "To: Bob <sip:bob@biloxi.com>"
             ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
             ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
             ?crlf "CSeq: 314159 INVITE"
             ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
             ?crlf "Content-Type: application/sdp"
             ?crlf "Content-Length: 4"
             ?crlf ?crlf "Test"
           >>,
    Msg2 = <<"ACK sip:bob@biloxi.com SIP/2.0"
             ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
             ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
             ?crlf "Max-Forwards: 70"
             ?crlf "To: Bob <sip:bob@biloxi.com>"
             ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
             ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
             ?crlf "CSeq: 314159 ACK"
             ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
             ?crlf "Content-Type: application/sdp"
             ?crlf "Content-Length: 4"
             ?crlf ?crlf "Test"
           >>,
    Msg3 = <<"ACK sip:bob@biloxi.com SIP/2.0"
             ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK1234"
             ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
             ?crlf "Max-Forwards: 70"
             ?crlf "To: Bob <sip:bob@biloxi.com>"
             ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
             ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
             ?crlf "CSeq: 314159 ACK"
             ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
             ?crlf "Content-Type: application/sdp"
             ?crlf "Content-Length: 4"
             ?crlf ?crlf "Test"
           >>,
    three_message_check(Msg1, Msg2, Msg3),
    ok.

branch_generation_rfc2543_test() ->
    Msg1 = <<"INVITE sip:bob@biloxi.com SIP/2.0"
             ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com"
             ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
             ?crlf "Max-Forwards: 70"
             ?crlf "To: Bob <sip:bob@biloxi.com>"
             ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
             ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
             ?crlf "CSeq: 314159 INVITE"
             ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
             ?crlf "Content-Type: application/sdp"
             ?crlf "Content-Length: 4"
             ?crlf ?crlf "Test"
           >>,
    Msg2 = <<"ACK sip:bob@biloxi.com SIP/2.0"
             ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com"
             ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
             ?crlf "Max-Forwards: 70"
             ?crlf "To: Bob <sip:bob@biloxi.com>"
             ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
             ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
             ?crlf "CSeq: 314159 ACK"
             ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
             ?crlf "Content-Type: application/sdp"
             ?crlf "Content-Length: 4"
             ?crlf ?crlf "Test"
           >>,
    Msg3 = <<"ACK sip:bob@biloxi.com SIP/2.0"
             ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com"
             ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
             ?crlf "Max-Forwards: 70"
             ?crlf "To: Bob <sip:bob@biloxi.com>"
             ?crlf "From: Alice <sip:alice@atlanta.com>;tag=canged_tag_1928301774"
             ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
             ?crlf "CSeq: 314159 ACK"
             ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
             ?crlf "Content-Type: application/sdp"
             ?crlf "Content-Length: 4"
             ?crlf ?crlf "Test"
           >>,
    three_message_check(Msg1, Msg2, Msg3),
    ok.

branch_generation_rfc2543_with_branch_test() ->
    Msg1 = <<"INVITE sip:bob@biloxi.com SIP/2.0"
             ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=rfc2543branch"
             ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
             ?crlf "Max-Forwards: 70"
             ?crlf "To: Bob <sip:bob@biloxi.com>"
             ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
             ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
             ?crlf "CSeq: 314159 INVITE"
             ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
             ?crlf "Content-Type: application/sdp"
             ?crlf "Content-Length: 4"
             ?crlf ?crlf "Test"
           >>,
    Msg2 = <<"ACK sip:bob@biloxi.com SIP/2.0"
             ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=rfc2543branch"
             ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
             ?crlf "Max-Forwards: 70"
             ?crlf "To: Bob <sip:bob@biloxi.com>"
             ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
             ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
             ?crlf "CSeq: 314159 ACK"
             ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
             ?crlf "Content-Type: application/sdp"
             ?crlf "Content-Length: 4"
             ?crlf ?crlf "Test"
           >>,
    Msg3 = <<"ACK sip:bob@biloxi.com SIP/2.0"
             ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=rfc2543branch"
             ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
             ?crlf "Max-Forwards: 70"
             ?crlf "To: Bob <sip:bob@biloxi.com>"
             ?crlf "From: Alice <sip:alice@atlanta.com>;tag=canged_tag_1928301774"
             ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
             ?crlf "CSeq: 314159 ACK"
             ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
             ?crlf "Content-Type: application/sdp"
             ?crlf "Content-Length: 4"
             ?crlf ?crlf "Test"
           >>,
    three_message_check(Msg1, Msg2, Msg3),
    ok.

process_response_test() ->
    %% Connection used for 'sending' message
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),

    %% Request to be sent
    Req = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,

    %% Adding branch to the message
    ReqSIPMsg     = sip_message(Req),
    {new_response, PrevVia, RawRespMsg} = reply_via_conn(ReqSIPMsg, ersip_reply:new(200), Conn),
    ProcessResult = ersip_proxy_stateless:process_response(PrevVia, RawRespMsg),

    ?assertMatch({forward, _}, ProcessResult),
    {forward, SendMessage} = ProcessResult,
    %% Message is not changed:
    ?assertEqual(ersip_msg:serialize_bin(RawRespMsg), ersip_sipmsg:serialize_bin(SendMessage)),
    ok.

process_response_no_forward_via_test() ->
    %% Connection used for 'sending' message
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),

    %% Request to be sent
    Req = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,

    %% Adding branch to the message
    ReqSIPMsg     = sip_message(Req, []),
    {new_response, _, RawRespMsg} = reply_via_conn(ReqSIPMsg, ersip_reply:new(200), Conn),
    PrevVia = ersip_hdr_via:new(ersip_host:make(<<"127.0.0.2">>), 5060, ersip_transport:make(udp)),
    ProcessResult = ersip_proxy_stateless:process_response(PrevVia, RawRespMsg),
    %% Action is drop because unmatched via
    ?assertMatch({drop, via_not_match}, ProcessResult),
    ok.

process_response_via_not_match_test() ->
    %% Connection used for 'sending' message
    LocalIP  = {127, 0, 0, 2},
    RemoteIP = {127, 0, 0, 1},
    UDP  = ersip_transport:make(udp),
    Conn = ersip_conn:new(LocalIP, 5061, RemoteIP, 5060, UDP, #{}),

    %% Request to be sent
    Req = <<"INVITE sip:bob@biloxi.com SIP/2.0"
            ?crlf "Via: SIP/2.0/UDP bigbox3.site3.atlanta.com"
            ?crlf "Max-Forwards: 70"
            ?crlf "To: Bob <sip:bob@biloxi.com>"
            ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
            ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com",
            ?crlf "CSeq: 314159 INVITE"
            ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
            ?crlf "Content-Type: application/sdp"
            ?crlf "Content-Length: 4"
            ?crlf ?crlf "Test"
          >>,

    %% Adding branch to the message
    ReqSIPMsg     = sip_message(Req, []),
    {new_response, PrevVia, RawRespMsg} = reply_via_conn(ReqSIPMsg, ersip_reply:new(200), Conn),
    %% Drop via as if we received message with topmost via only (for
    %% example SIP https://freeswitch.org/jira/browse/FS-11128
    NoViaMsg = del_topmost_via(RawRespMsg),
    ProcessResult = ersip_proxy_stateless:process_response(PrevVia, NoViaMsg),

    %% Action is drop because cannot forward message to without via
    ?assertMatch({drop, no_more_via}, ProcessResult),
    ok.


%%%===================================================================
%%% Helpers
%%%===================================================================

three_message_check(Eq1, Eq2, Neq) ->
    Branch1 = stateless_branch(Eq1),
    Branch2 = stateless_branch(Eq2),
    Branch3 = stateless_branch(Neq),
    [?assert(ersip_branch:is_rfc3261(B)) || B <- [Branch1, Branch2, Branch3]],
    ?assertEqual(Branch1, Branch2),
    ?assertNotEqual(Branch1, Branch3).


stateless_branch(Bin) ->
    ersip_proxy_stateless:branch(sip_message(Bin)).

raw_message(Bin) ->
    P  = ersip_parser:new_dgram(Bin),
    {{ok, PMsg}, _P2} = ersip_parser:parse(P),
    PMsg.

sip_message(Bin) ->
    {ok, SipMsg} = ersip_sipmsg:parse(raw_message(Bin), all),
    SipMsg.

sip_message(Bin, Headers) ->
    {ok, SipMsg} = ersip_sipmsg:parse(raw_message(Bin), Headers),
    SipMsg.

reply_via_conn(SipMsg, Reply, Conn) ->
    ReqSIPRawMsg  = ersip_sipmsg:raw_message(SipMsg),
    Branch        = ersip_proxy_stateless:branch(SipMsg),
    ReqSIPRawMsg1 = ersip_conn:add_via(ReqSIPRawMsg, Branch, Conn),

    %% Generating reply to the message:
    {ok, ReqSIPMsg1} = ersip_sipmsg:parse(ReqSIPRawMsg1, all),
    RespSIPMsg = ersip_sipmsg:reply(Reply, ReqSIPMsg1),

    %% Passing response:
    {_, [{new_response, _, _} = SE]} = ersip_conn:conn_data(ersip_sipmsg:serialize_bin(RespSIPMsg), Conn),
    SE.

del_topmost_via(RawMsg) ->
    ViaH = ersip_msg:get(<<"via">>, RawMsg),
    {ok, _Value, NewViaH} = ersip_hdr:take_topmost(ViaH),
    ersip_msg:set_header(NewViaH, RawMsg).
