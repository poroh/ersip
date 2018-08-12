%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% CANCEL request related functions
%%

-module(ersip_request_cancel).

-export([generate/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Constracting CANCEL request in according to RFC3262 9.1 Client Behavior
-spec generate(ersip_request:request()) -> ersip_request:request().
generate(InitialRequest) ->
    ReqSipMsg = ersip_request:sipmsg(InitialRequest),
    %% The following procedures are used to construct a CANCEL request.
    %% The Request-URI, Call-ID, To, the numeric part of CSeq, and From
    %% header fields in the CANCEL request MUST be identical to those
    %% in the request being cancelled, including tags.  A CANCEL
    %% constructed by a client MUST have only a single Via header field
    %% value matching the top Via value in the request being cancelled.
    %% Using the same values for these header fields allows the CANCEL
    %% to be matched with the request it cancels (Section 9.2 indicates
    %% how such matching occurs).  However, the method part of the CSeq
    %% header field MUST have a value of CANCEL.

    RURI = ersip_sipmsg:ruri(ReqSipMsg),
    CANCEL0 = ersip_sipmsg:new_request(ersip_method:cancel(), RURI),
    CANCEL1 = ersip_sipmsg:copy(callid, ReqSipMsg, CANCEL0),
    CANCEL2 = ersip_sipmsg:copy(to,     ReqSipMsg, CANCEL1),

    ReqCSeq = ersip_sipmsg:get(cseq, ReqSipMsg),
    CANCELCSeq = ersip_hdr_cseq:set_method(ersip_method:cancel(), ReqCSeq),
    CANCEL3 = ersip_sipmsg:set(cseq, CANCELCSeq, CANCEL2),
    CANCEL4 = ersip_sipmsg:copy(from, ReqSipMsg, CANCEL3),

    %% If the request being cancelled contains a Route header field, the
    %% CANCEL request MUST include that Route header field's values.
    CANCEL5 = ersip_sipmsg:copy(route, ReqSipMsg, CANCEL4),

    %% RFC 3261 says nothing about max-forwards in CANCEL for this case
    %% but following logic of route copy it should be the same as in
    %% INVITE:
    CANCEL6 = ersip_sipmsg:copy(maxforwards, ReqSipMsg, CANCEL5),

    %% Normative:
    %% A CANCEL constructed by a client MUST have only a single Via
    %% header field value matching the top Via value in the request
    %% being cancelled.
    %%
    %% Implementation: we really do not add Via here because it is
    %% automatically added when message is passed via connection. So
    %% what we really do here - we generate ersip_request with the
    %% same paramters as InitialRequest
    ersip_request:set_sipmsg(CANCEL6, InitialRequest).
