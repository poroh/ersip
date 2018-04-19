%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP stateless proxy functions
%%

-module(ersip_proxy_stateless).

-export([branch/1]).

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
-spec branch(ersip_sipmsg:sipmsg()) -> ersip_branch:banch().
branch(SipMsg) ->
    TopMostVia = ersip_sipmsg:get(topmost_via, SipMsg),
    case ersip_hdr_via:branch(TopMostVia) of
        undefined ->
            rfc2543_based_branch(SipMsg);
        {branch,_} = Branch ->
            case ersip_branch:is_rfc3261(Branch) of
                true ->
                    rfc3261_based_branch(Branch);
                false ->
                    rfc2543_based_branch(SipMsg)
            end
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

-spec get_tag(ersip_hdr_fromto:fromto()) -> binary().
get_tag(FromTo) ->
    case ersip_hdr_fromto:tag_key(FromTo) of
        undefined ->
            %% use & here to prevent collision with normal tags
            <<"&undef">>;
        {tag_key, TagKey} when is_binary(TagKey) ->
            TagKey
    end.
