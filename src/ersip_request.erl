%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP output request
%%

-module(ersip_request).

-export([new_stateless_proxy/1,
         new/2,
         branch/1,
         send_via_conn/2
        ]).

-export_type([request/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(request, {sipmsg :: ersip_sipmsg:sipmsg(),
                  branch :: ersip_branch:branch()
                 }).
-type request() :: #request{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc generate stateless proxy output request.
-spec new_stateless_proxy(ersip_sipmsg:sipmsg()) -> request().
new_stateless_proxy(SipMsg) ->
    #request{sipmsg = SipMsg,
             branch = ersip_proxy_stateless:branch(SipMsg)
            }.

-spec new(ersip_sipmsg:sipmsg(), ersip_branch:branch()) -> request().
new(SipMsg, {branch, _} = Branch) ->
    #request{sipmsg = SipMsg,
             branch = Branch
            }.

-spec branch(request()) -> ersip_branch:branch().
branch(#request{branch = Branch}) ->
    Branch.

%% @doc send request via SIP connection.
-spec send_via_conn(request(), ersip_conn:sip_conn()) -> iolist().
send_via_conn(#request{sipmsg = SipMsg, branch = Branch}, SIPConn) ->
    RawMsg  = ersip_sipmsg:raw_message(SipMsg),
    RawMsg1 = ersip_conn:add_via(RawMsg, Branch, SIPConn),
    ersip_msg:serialize(RawMsg1).

