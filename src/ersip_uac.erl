%%%
%%% Copyright (c) 2020 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% UAC
%%%

-module(ersip_uac).

-export([options/3]).

%%===================================================================
%% Types
%%===================================================================

-type req_params() :: [req_param()].
-type req_param() :: {header(), value()}.
-type header() :: ersip_hnames:known_header() | binary().
-type value()  :: any().

%%===================================================================
%% API
%%===================================================================

-spec options(ersip_uri:uri(), ersip_hdr_fromto:fromto(), req_params()) -> ersip_sipmsg:sipmsg().
options(RURI, From, ReqParams) ->
    Req0 = ersip_sipmsg:new_request(ersip_method:options(), RURI),
    From1 =
        case ersip_hdr_fromto:tag(From) of
            undefined ->
                ersip_hdr_fromto:set_random_tag(8, From);
            _ ->
                From
        end,
    Req1 = ersip_sipmsg:set(from, From1, Req0),
    Req2 = apply_req_params(ReqParams, Req1),
    Req3 = add_required(Req2),
    Req3.

%%===================================================================
%% Helpers
%%===================================================================

-spec apply_req_params(req_params(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
apply_req_params(Params, Req) ->
    lists:foldl(fun apply_req_param/2, Req, Params).


-spec apply_req_param(req_param(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
apply_req_param({Name, Value}, Req) when is_atom(Name) ->
    ersip_sipmsg:set(Name, Value, Req);
apply_req_param({Name, Value}, Req) when is_binary(Name), is_binary(Value) ->
    Hdr0 = ersip_sipmsg:raw_header(Name, Req),
    Hdr1 = ersip_hdr:add_value(Value, Hdr0),
    {ok, SipMsg} = ersip_sipmsg:set_raw_header(Hdr1, Req),
    SipMsg.

-spec add_required(ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
add_required(SipMsg) ->
    Required = [to, callid, cseq, maxforwards],
    Missed = lists:filter(fun(HdrAtom) ->
                                  ersip_sipmsg:find(HdrAtom, SipMsg) == not_found
                          end,
                          Required),
    lists:foldl(fun set_default/2, SipMsg, Missed).

-spec set_default(ersip_hnames:known_header(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
set_default(to, SipMsg) ->
    RURI = ersip_sipmsg:ruri(SipMsg),
    To0 = ersip_hdr_fromto:new(),
    To1 = ersip_hdr_fromto:set_uri(RURI, To0),
    ersip_sipmsg:set(to, To1, SipMsg);
set_default(callid, SipMsg) ->
    CallID = ersip_hdr_callid:make_random(16),
    ersip_sipmsg:set(callid, CallID, SipMsg);
set_default(cseq, SipMsg) ->
    CSeq = ersip_hdr_cseq:new(ersip_sipmsg:method(SipMsg)),
    ersip_sipmsg:set(cseq, CSeq, SipMsg);
set_default(maxforwards, SipMsg) ->
    MaxForwards = ersip_hdr_maxforwards:make(70),
    ersip_sipmsg:set(maxforwards, MaxForwards, SipMsg).

