%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP transaction id test
%%

-module(ersip_trans_id_test).

-include_lib("eunit/include/eunit.hrl").

-define(crlf, "\r\n").

%%%===================================================================
%%% Cases
%%%===================================================================

uas_transaction_id_test() ->
    InviteMsg1 = <<"INVITE sip:bob@biloxi.com SIP/2.0"
      ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
      ?crlf "Max-Forwards: 70"
      ?crlf "To: Bob <sip:bob@biloxi.com>"
      ?crlf "From: Alice <sip:alice@atlanta.com>;tag=1928301774"
      ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
      ?crlf "CSeq: 314159 INVITE"
      ?crlf "Contact: <sip:alice@pc33.atlanta.com>"
      ?crlf "Content-Type: application/sdp"
      ?crlf "Content-Length: 0"
      ?crlf ?crlf
          >>,
    InviteMsg2 = <<"INVITE sip:bob@biloxi.com SIP/2.0"
      ?crlf "Via: SIP/2.0/UDP pc33.atlanta.com;branch=z9hG4bK776asdhds"
      ?crlf "Max-Forwards: 70"
      ?crlf "To: Not a Bob <sip:bob@biloxi.com>"
      ?crlf "From: Alice Renamed <sip:alice@atlanta.com>;tag=1928301774"
      ?crlf "Call-ID: a84b4c76e66710@pc33.atlanta.com"
      ?crlf "CSeq: 314160 INVITE"
      ?crlf "Contact: Another <sip:alice@pc33.atlanta.com>"
      ?crlf "Content-Type: application/sdp"
      ?crlf "Content-Length: 0"
      ?crlf ?crlf
          >>,
    ?assertEqual(calc_uas_trans_id(InviteMsg1), calc_uas_trans_id(InviteMsg2)).
    


%%%===================================================================
%%% Helpers
%%%===================================================================

make_sipmsg(Binary) ->
    P  = ersip_parser:new_dgram(Binary),
    { {ok, PMsg}, _P2 } = ersip_parser:parse(P),
    { ok, SipMsg } = ersip_sipmsg:parse(PMsg, all),
    SipMsg.

calc_uas_trans_id(Binary) ->
    SipMsg = make_sipmsg(Binary),
    ersip_trans_id:make_uas(SipMsg).
