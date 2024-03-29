%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP stateless proxy functions
%%

-module(ersip_proxy_stateless).

-export([branch/1,
         process_response/2
        ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type process_response_result() :: {forward, ersip_sipmsg:sipmsg()}
                                 | {drop, Reason :: term()}.

%%%===================================================================
%%% API
%%%===================================================================

%% 16.11 Stateless Proxy
%% Branch generation.
%%
%% The proxy examines the branch ID in the topmost Via header field of
%% the received request.  If it begins with the magic cookie, the
%% first component of the branch ID of the outgoing request is
%% computed as a hash of the received branch ID.  Otherwise, the first
%% component of the branch ID is computed as a hash of the topmost
%% Via, the tag in the To header field, the tag in the From header
%% field, the Call-ID header field, the CSeq number (but not method),
%% and the Request-URI from the received request.  One of these fields
%% will always vary across two different transactions.
-spec branch(ersip_sipmsg:sipmsg()) -> ersip_branch:branch().
branch(SipMsg) ->
    TopMostVia = ersip_sipmsg:get(topmost_via, SipMsg),
    case ersip_hdr_via:branch(TopMostVia) of
        undefined ->
            rfc2543_based_branch(SipMsg);
        {ok, Branch} ->
            case ersip_branch:is_rfc3261(Branch) of
                true ->
                    rfc3261_based_branch(Branch);
                false ->
                    rfc2543_based_branch(SipMsg)
            end
    end.

%% 16.11 Stateless Proxy
%%
%% Response processing as described in Section 16.7 does not apply to
%% a proxy behaving statelessly.  When a response arrives at a
%% stateless proxy, the proxy MUST inspect the sent-by value in the
%% first (topmost) Via header field value.  If that address matches
%% the proxy, (it equals a value this proxy has inserted into previous
%% requests) the proxy MUST remove that header field value from the
%% response and forward the result to the location indicated in the
%% next Via header field value.  The proxy MUST NOT add to, modify, or
%% remove the message body.  Unless specified otherwise, the proxy
%% MUST NOT remove any other header field values.  If the address does
%% not match the proxy, the message MUST be silently discarded.

-dialyzer({no_match, [check_via_branch/1, process_response/2]}).
%% TODO remove this dialyzer settings when fix check_via_branch func

-spec process_response(PrevVia, RawMsg) -> process_response_result() when
      PrevVia :: ersip_hdr_via:via(),
      RawMsg  :: ersip_msg:message().
process_response(PrevVia, RawMsg) ->
    case ersip_sipmsg:parse(RawMsg, []) of
        {ok, SipMsg} ->
            case ersip_hdr_via:branch(PrevVia) of
                {ok, Branch} ->
                    case check_via_branch(Branch) of
                        match ->
                            %% Check if next via is still exist and forward
                            %% response there.
                            case ersip_sipmsg:find(topmost_via, SipMsg) of
                                not_found ->
                                    {drop, no_more_via};
                                {ok, _} ->
                                    {forward, SipMsg}
                            end;
                        mismatch ->
                            {drop, via_not_match}
                    end;
                undefined ->
                    {drop, via_no_branch}
            end;
        {error, _} = Error ->
            {drop, {parse_error, Error}}
    end.

%%%===================================================================
%%% Helpers
%%%===================================================================

-spec rfc3261_based_branch(ersip_branch:branch()) -> ersip_branch:branch().
rfc3261_based_branch({branch, Bin}) ->
    ersip_branch:make_rfc3261(ersip_id:token(Bin)).


-spec rfc2543_based_branch(ersip_sipmsg:sipmsg()) -> ersip_branch:branch().
rfc2543_based_branch(SipMsg) ->
    %% Otherwise, the first component of the branch ID is computed as
    %% a hash of the topmost Via, the tag in the To header field, the
    %% tag in the From header field, the Call-ID header field, the
    %% CSeq number (but not method), and the Request-URI from the
    %% received request.  One of these fields will always vary across
    %% two different transactions.
    [To, From, TopMostVia, CallId, CSeq]
        = [ersip_sipmsg:get(X, SipMsg) || X <- [to, from, topmost_via, callid, cseq]],
    ToBeHashed =
        [ersip_hdr_via:assemble(TopMostVia), <<"&">>,
         get_tag(To), <<"&">>,
         get_tag(From), <<"&">>,
         ersip_hdr_callid:assemble(CallId), <<"&">>,
         integer_to_binary(ersip_hdr_cseq:number(CSeq))
        ],
    Hash = crypto:hash(md5, ToBeHashed),
    ersip_branch:make_rfc3261(ersip_id:token(Hash)).

-spec check_via_branch(ersip_branch:branch()) -> match | mismatch.
check_via_branch({branch, _} = _Branch) ->
    %% TODO: check against generated by branch/1 function.
    match.

-spec get_tag(ersip_hdr_fromto:fromto()) -> binary().
get_tag(FromTo) ->
    case ersip_hdr_fromto:tag_key(FromTo) of
        undefined ->
            %% use & here to prevent collision with normal tags
            <<"&undef">>;
        {tag_key, TagKey} when is_binary(TagKey) ->
            TagKey
    end.


