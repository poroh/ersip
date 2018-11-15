%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP output request
%%

-module(ersip_request).

-export([new_stateless_proxy/2,
         new/2,
         new/3,
         branch/1,
         sipmsg/1,
         set_sipmsg/2,
         nexthop/1,
         set_nexthop/2,
         send_via_conn/2
        ]).

-export_type([request/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(request, {sipmsg              :: ersip_sipmsg:sipmsg(),
                  branch              :: ersip_branch:branch(),
                  nexthop = undefined :: undefined | ersip_uri:uri()
                 }).
-type request() :: #request{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc generate stateless proxy output request.
-spec new_stateless_proxy(ersip_sipmsg:sipmsg(), ersip_uri:uri()) -> request().
new_stateless_proxy(SipMsg, Nexthop) ->
    #request{sipmsg = SipMsg,
             branch = ersip_proxy_stateless:branch(SipMsg),
             nexthop = Nexthop
            }.

-spec new(ersip_sipmsg:sipmsg(), ersip_branch:branch()) -> request().
new(SipMsg, {branch, _} = Branch) ->
    #request{sipmsg = SipMsg,
             branch = Branch,
             nexthop = calculate_nexthop(SipMsg)}.

-spec new(ersip_sipmsg:sipmsg(), ersip_branch:branch(), ersip_uri:uri()) -> request().
new(SipMsg, {branch, _} = Branch, Nexthop) ->
    #request{sipmsg = SipMsg,
             branch = Branch,
             nexthop = Nexthop
            }.

-spec branch(request()) -> ersip_branch:branch().
branch(#request{branch = Branch}) ->
    Branch.

-spec sipmsg(request()) -> ersip_sipmsg:sipmsg().
sipmsg(#request{sipmsg = SipMsg}) ->
    SipMsg.

-spec set_sipmsg(ersip_sipmsg:sipmsg(), request()) -> request().
set_sipmsg(SipMsg, #request{} = Base) ->
    Base#request{sipmsg = SipMsg}.

-spec nexthop(request()) -> ersip_uri:uri().
nexthop(#request{nexthop = NexthopURI}) ->
    NexthopURI.

-spec set_nexthop(NexthopURI :: ersip_uri:uri(), request()) -> request().
set_nexthop(NexthopURI, #request{} = Req) ->
    Req#request{nexthop = NexthopURI}.

%% @doc send request via SIP connection.
-spec send_via_conn(request(), ersip_conn:sip_conn()) -> iolist().
send_via_conn(#request{sipmsg = SipMsg, branch = Branch}, SIPConn) ->
    RawMsg  = ersip_sipmsg:raw_message(SipMsg),
    RawMsg1 = ersip_conn:add_via(RawMsg, Branch, SIPConn),
    ersip_msg:serialize(RawMsg1).

%%%===================================================================
%%% Implementation
%%%===================================================================

-spec calculate_nexthop(ersip_sipmsg:sipmsg()) -> ersip_uri:uri().
calculate_nexthop(SipMsg) ->
    case ersip_sipmsg:find(route, SipMsg) of
        not_found ->
            ersip_sipmsg:ruri(SipMsg);
        {ok, RouteSet} ->
            case ersip_route_set:is_empty(RouteSet) of
                true ->
                    ersip_sipmsg:ruri(SipMsg);
                false ->
                    FirstRoute = ersip_route_set:first(RouteSet),
                    ersip_hdr_route:uri(FirstRoute)
            end
    end.



