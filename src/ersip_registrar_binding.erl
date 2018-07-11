%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Registrar Binding
%%
%% Represents one saved binding for AOR
%%

-module(ersip_registrar_binding).

-export([new/4,
         contact/1,
         contact_key/1,
         callid_cseq/1,
         update/4
        ]).
-export_type([binding/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(binding, {contact :: ersip_hdr_contact:contact(),
                  callid  :: ersip_hdr_callid:callid(),
                  cseq    :: ersip_hdr_cseq:cseq_num(),
                  expires :: non_neg_integer()
                 }).
-type binding() :: #binding{}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(ersip_hdr_callid:callid(), ersip_hdr_cseq:cseq_num(), ersip_hdr_contact:contact(), non_neg_integer()) -> binding().
new(CallId, CSeqNum, Contact, Exp) ->
    #binding{contact = Contact,
             callid  = CallId,
             cseq    = CSeqNum,
             expires = Exp}.

-spec contact(binding()) -> ersip_hdr_contact:contact().
contact(#binding{contact = Contact, expires = Exp}) ->
    ersip_hdr_contact:set_expires(Exp, Contact).

-spec contact_key(binding()) -> ersip_uri:uri().
contact_key(#binding{contact = Contact}) ->
    ContactURI = ersip_hdr_contact:uri(Contact),
    ersip_uri:make_key(ContactURI).

-spec callid_cseq(binding()) -> {ersip_hdr_callid:callid(), ersip_hdr_cseq:cseq_num()}.
callid_cseq(#binding{callid = CallId, cseq = CSeq}) ->
    {CallId, CSeq}.

-spec update(NewExpiration :: pos_integer(), NewCallId :: ersip_hdr_callid:callid(), NewCSeq :: pos_integer(), binding()) -> binding().
update(NewExpiration, NewCallId, NewCSeq, #binding{} = Binding) when is_integer(NewExpiration),
                                                                     is_integer(NewCSeq),
                                                                     NewExpiration > 0 ->
    Binding#binding{expires = NewExpiration, cseq = NewCSeq, callid = NewCallId}.

