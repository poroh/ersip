%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP proxy support
%%

-module(ersip_proxy).

-export([new_stateful/2,
         forward_to/2
        ]).

-export_type([params/0,
              options/0,
              internal_trans_id/0
             ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type params()  :: ersip_proxy_common:proxy_params().
-type options() ::
        #{
           %% Prevent options validation (may increase performance
           %% after debug finish.
           no_validate => boolean(),
           %% If 'allow' option is set then proxy is restricted to pass
           %% only methods that are included in this set. If proxy
           %% replies on OPTIONS request it adds Allow header to
           %% expose this restrictions.
           %%
           %% If 'allow' options is not set then proxy is
           %% method-agnostic and passes all messages.
           allow => ersip_hdr_allow:allow(),

           %% 'supported' defines features that are supported by
           %% proxy.  If request hase 'Proxy-Require' header than to
           %% be passed through the proxy it need to be subset of this
           %% 'supported' options.
           %%
           %% Also this set is reported in OPTIONS reply in supported
           %% header field
           supported => ersip_hdr_opttag_list:option_tag_list(),

           %% Optional loop detection is performed by proxy.
           %% loop_detect => boolean()

           %% Todo: realm and proxy-authorization

           %% Record-route functions that checks that this proxy is
           %% generated this route/record-route header. Most obvious
           %% way of checking is domain name/ip address checking.
           check_rroute_fun => check_rroute_fun(),

           %% Record-route header if proxy need to stay on dialog
           %% messages. This URI need to be resolved to proxy's IP
           %% address. Function check_rroute_fun(record_route_uri)
           %% need to return true for next requests.
           %%
           %% Record-route is REQUIRED when proxy is on the border
           %% between secure/not secure transport (see RFC 3261 16.6).
           record_route_uri => ersip_uri:uri(),

           %% Timer C timeout in milliseconds.
           timer_c => non_neg_integer()
         }.


-record(stateful, {phase       :: stateful_phase(),
                   options     :: params(),                           %% Pass options
                   orig_sipmsg :: ersip_sipmsg:sipmsg(),              %% SIP message to be passed
                   fwd_sipmsg  :: ersip_sipmsg:sipmsg() | undefined,  %% SIP message to be forwarded
                   use_trans   :: boolean(),                          %% Transactions is used for request
                   req_map   = #{} :: context_request_map()
                  }).
-type stateful_phase() :: init
                        | select_target
                        | send_requests.
-type stateful() :: #stateful{}.
-type stateful_result() :: {stateful(), ersip_proxy_se:side_effect()}.
-type internal_trans_id() :: any().
-type check_rroute_fun() :: fun((ersip_hdr_route:route()) -> boolean()).

-type context_request_map() :: #{ersip_branch:branch_key() => request_context()}.
-record(request_context,
        {key     :: ersip_branch:branch_key(),
         req     :: ersip_request:request(),
         timer_c :: reference() | undefined,
         resp    :: ersip_sipmsg:sipmsg() | undefined
        }).
-type request_context() :: #request_context{}.


%%%===================================================================
%%% API
%%%===================================================================

%% @doc New stateful proxy request.
-spec new_stateful(ersip_sipmsg:sipmsg(), options()) -> stateful_result().
new_stateful(SipMsg, ProxyOptions) ->
    %% There is no client transaction for ACK.  If the TU
    %% wishes to send an ACK, it passes one directly to the
    %% transport layer for transmission.
    UseTrans = ersip_sipmsg:method(SipMsg) /= ersip_method:ack(),
    Stateful = #stateful{phase       = init,
                         options     = ProxyOptions,
                         orig_sipmsg = SipMsg,
                         use_trans   = UseTrans
                        },
    case UseTrans of
        true ->
            {Stateful, [ersip_proxy_se:create_trans(server, init, SipMsg)]};
        false ->
            trans_result(init, SipMsg, Stateful)
    end.

-spec trans_result(internal_trans_id(), ersip_sipmsg:sipmsg(), stateful()) -> stateful_result().
trans_result(init, SipMsg0, #stateful{phase = init, options = ProxyOptions} = Stateful) ->
    SipMsg1   = ersip_proxy_common:process_route_info(SipMsg0, ProxyOptions),
    Stateful1 = Stateful#stateful{phase = select_target,
                                  fwd_sipmsg = SipMsg1
                                 },
    %% If the Request-URI of the request contains an maddr parameter, the
    %% Request-URI MUST be placed into the target set as the only target
    %% URI, and the proxy MUST proceed to Section 16.6.
    RURI = ersip_sipmsg:ruri(SipMsg1),
    case ersip_uri:params(RURI) of
        #{maddr := Host} ->
            forward_to(Host, Stateful1);
        _ ->
            RURI = ersip_sipmsg:ruri(SipMsg1),
            {Stateful1, [ersip_proxy_se:select_target(RURI)]}
    end;
trans_result(BranchKey, Resp, #stateful{phase = send_requests} = Stateful) ->
    process_response(BranchKey, Resp, Stateful).

%% @doc Forward request to selected target. If list of URIs is
%% specified then all requests are forwarded simultaneously (forked).
%% Caller may specify branch for each target by using tuple {URI, Branch}.
-spec forward_to(Target :: ersip_uri:uri() | [ersip_uri:uri()], stateful()) -> stateful_result().
forward_to(Target, #stateful{} = Stateful) when not is_list(Target) ->
    forward_to([Target], Stateful);
forward_to(TargetList, #stateful{phase = select_target, options = ProxyOptions, fwd_sipmsg = FwdMsg} = Stateful) ->
    Requests =
        lists:map(fun({Target, Branch}) ->
                          {SipMsg, #{nexthop := NexthopURI}} = ersip_proxy_common:forward_request(Target, FwdMsg, ProxyOptions),
                          ersip_request:new(SipMsg, Branch, NexthopURI);
                     (Target) ->
                          {SipMsg, #{nexthop := NexthopURI}} = ersip_proxy_common:forward_request(Target, FwdMsg, ProxyOptions),
                          ersip_request:new(SipMsg, erproxy_branch:generate(), NexthopURI)
                  end,
                  TargetList),
    BranchReqCtxList =
        [begin
             Branch = ersip_request:branch(R),
             BranchKey = ersip_branch:make_key(Branch),
             {BranchKey, create_request_context(BranchKey, R)}
         end
         || R <- Requests],
    SideEffects = create_client_trans(BranchReqCtxList, ProxyOptions, []),
    Stateful1 = Stateful#stateful{phase = send_requests,
                                  req_map = maps:from_list(BranchReqCtxList)
                                 },
    {Stateful1, SideEffects}.

%%%===================================================================
%%% Internal Implementation
%%%===================================================================
-type pr_result() :: stop
                   | continue
                   | {continue, stateful()}
                   | {continue, stateful_result()}.

-spec process_response(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> stateful_result().
process_response(BranchKey, RespSipMsg, #stateful{req_map = ReqCtxMap} = Stateful) ->
    %%    1.  Find the appropriate response context
    %%
    %%    2.  Update timer C for provisional responses
    %%
    %%    3.  Remove the topmost Via
    %%
    %%    4.  Add the response to the response context
    %%
    %%    5.  Check to see if this response should be forwarded immediately
    %%
    %%    6.  When necessary, choose the best final response from the
    %%        response context
    %%
    %% If no final response has been forwarded after every client
    %% transaction associated with the response context has been terminated,
    %% the proxy must choose and forward the "best" response from those it
    %% has seen so far.
    %%
    %% The following processing MUST be performed on each response that is
    %% forwarded.  It is likely that more than one response to each request
    %% will be forwarded: at least each provisional and one final response.
    %%
    %%    7.  Aggregate authorization header field values if necessary
    %%
    %%    8.  Optionally rewrite Record-Route header field values
    %%
    %%    9.  Forward the response
    %%
    %%    10. Generate any necessary CANCEL requests
    do_process_response([fun pr_find_context/3,
                         fun pr_maybe_update_timer_c/3,
                         fun pr_process_via/3,
                         fun pr_add_response/3,
                         fun pr_maybe_forward_response/3
                        ],
                        BranchKey, RespSipMsg,
                        {[], Stateful}).

do_process_response([], _, _, {RevSe, Stateful}) ->
    {lists:reverse(RevSe), Stateful};
do_process_response([F | Rest], BranchKey, RespSipMsg, {RevSE, Stateful} = Acc) ->
    case F(BranchKey, RespSipMsg, Stateful) of
        continue ->
            do_process_response(Rest, BranchKey, RespSipMsg, Acc);
        {continue, #stateful{} = Stateful1} ->
            do_process_response(Rest, BranchKey, RespSipMsg, {RevSE, Stateful1});
        {continue, {#stateful{} = Stateful1, SE1}} ->
            do_process_response(Rest, BranchKey, RespSipMsg, {lists:reverse(SE1) ++ RevSE, Stateful1});
        stop ->
            do_process_response([], BranchKey, RespSipMsg, Acc)
    end.

-spec pr_find_context(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_find_context(BranchKey, _RespSipMsg, #stateful{req_map = ReqCtxMap}) ->
    %% The proxy locates the "response context" it created before
    %% forwarding the original request using the key described in
    %% Section 16.6.  The remaining processing steps take place in
    %% this context.
    case ReqCtxMap of
        #{BranchKey := _} ->
            continue;
        _ ->
            stop
    end.

-spec pr_maybe_update_timer_c(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_maybe_update_timer_c(BranchKey, RespSipMsg, #stateful{req_map = ReqCtxMap} = Stateful) ->
    %% For an INVITE transaction, if the response is a provisional
    %% response with status codes 101 to 199 inclusive (i.e., anything
    %% but 100), the proxy MUST reset timer C for that client
    %% transaction.  The timer MAY be reset to a different value, but
    %% this value MUST be greater than 3 minutes.
    case ersip_sipmsg:status(RespSipMsg) of
        Code when Code < 101 orelse Code > 199  ->
            continue;
        _ ->
            case maps:get(BranchKey, ReqCtxMap) of
                #request_context{timer_c = undefined} ->
                    continue;
                #request_context{} ->
                    {continue, reset_timer_c(BranchKey, Stateful)}
            end
    end.

-spec pr_process_via(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_process_via(BranchKey, RespSipMsg, #stateful{} = Stateful) ->
    %% Normative:
    %% If no Via header field values remain in the response, the
    %% response was meant for this element and MUST NOT be forwarded.
    %% The remainder of the processing described in this section is
    %% not performed on this message, the UAC processing rules
    %% described in Section 8.1.3 are followed instead (transport
    %% layer processing has already occurred).
    %%
    %% Implementation:
    %% We remove topmost via when receive message from connection. So no Via already here
    case ersip_sipmsg:find(topmost_via, RespSipMsg) of
        not_found ->
            pr_process_selfgenerated(BranchKey, RespSipMsg, #stateful{} = Stateful);
        _ ->
            continue
    end.

-spec pr_process_selfgenerated(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_process_selfgenerated(BranchKey, RespSipMsg, #stateful{} = Stateful) ->
    %% TODO: 8.1.3
    stop.

-spec pr_add_response(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_add_response(BranchKey, RespSipMsg, #stateful{req_map = ReqCtxMap} = Stateful) ->
    %% Final responses received are stored in the response context
    %% until a final response is generated on the server transaction
    %% associated with this context.  The response may be a candidate
    %% for the best final response to be returned on that server
    %% transaction.  Information from this response may be needed in
    %% forming the best response, even if this response is not chosen.
    %%
    %% TODO: process 3xx
    case ersip_sipmsg:status(RespSipMsg) of
        Code when Code >= 200 ->
            #{BranchKey := ReqCtx} = ReqCtxMap,
            ReqCtx1 = ReqCtx#request_context{resp = RespSipMsg},
            ReqCtxMap1 = ReqCtxMap#{BranchKey := ReqCtx1},
            {continue, Stateful#stateful{req_map = ReqCtxMap1}};
        _ ->
            continue
    end.

-spec pr_maybe_forward_response(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_maybe_forward_response(BranchKey, RespSipMsg, #stateful{} = Stateful) ->
    %% Until a final response has been sent on the server transaction,
    %% the following responses MUST be forwarded immediately:
    %%
    %% -  Any provisional response other than 100 (Trying)
    %%
    %% -  Any 2xx response
    case ersip_sipmsg:status(RespSipMsg) of
        Code when Code >= 101 andalso Code =< 199  ->
            pr_forward_response(RespSipMsg, Stateful);
        Code when Code >= 200 andalso Code =< 299 ->
            pr_forward_response(RespSipMsg, Stateful);
        Code when Code >= 600 ->
            pr_forward_response(RespSipMsg, Stateful);
        _ ->
            case choose_best_response(Stateful) of
                collect_more ->
                    stop;
                {ok, RespSipMsg} ->
                    pr_forward_response(RespSipMsg, Stateful)
            end
    end.

pr_forward_response(_, _) ->
    stop.

choose_best_response(_) ->
    collect_more.

-spec create_request_context(ersip_branch:branch_key(), ersip_request:request()) -> request_context().
create_request_context(BranchKey, Request) ->
    INVITE = ersip_method:invite(),
    TimerC =
        case ersip_sipmsg:method(ersip_request:sipmsg(Request)) of
            INVITE ->
                make_ref();
            _ ->
                undefined
        end,
    #request_context{key     = BranchKey,
                     req     = Request,
                     timer_c = TimerC,
                     resp    = undefined}.

-spec create_client_trans([{ersip_branch:branch_key(), request_context()}], options(), [ersip_proxy_se:side_effect()]) -> [ersip_proxy_se:side_effect()].
create_client_trans([], _ProxyOptions, Acc) ->
    Acc;
create_client_trans([{BranchKey, #request_context{timer_c = undefined, req = Req}} | Rest], ProxyOptions, Acc) ->
    ClientTrans = ersip_proxy_se:create_trans(client, BranchKey, Req),
    create_client_trans(Rest, ProxyOptions, [ClientTrans | Acc]);
create_client_trans([{BranchKey, #request_context{timer_c = TimerCRef, req = Req}} | Rest], ProxyOptions, Acc) ->
    ClientTrans = ersip_proxy_se:create_trans(client, BranchKey, Req),
    TimerC = ersip_proxy_se:set_timer(timer_c_timeout(ProxyOptions), {timer, BranchKey, TimerCRef}),
    create_client_trans(Rest, ProxyOptions, [ClientTrans, TimerC | Acc]).

-spec timer_c_timeout(stateful() | options()) -> pos_integer().
timer_c_timeout(#stateful{options = ProxyOptions}) ->
    timer_c_timeout(ProxyOptions);
timer_c_timeout(#{timer_c := TimerC}) ->
    TimerC;
timer_c_timeout(_) ->
    180.

-spec reset_timer_c(ersip_branch:branch_key(), stateful()) -> stateful_result().
reset_timer_c(BranchKey, #stateful{req_map = ReqCtxMap} = Stateful) ->
    #{BranchKey := ReqCtx} = ReqCtxMap,
    TimerCRef = make_ref(),
    TimerC = ersip_proxy_se:set_timer(timer_c_timeout(Stateful), {timer, BranchKey, TimerCRef}),
    ReqCtx1 = ReqCtx#request_context{timer_c = TimerCRef},
    ReqCtxMap1 = ReqCtxMap#{BranchKey := ReqCtx1},
    Stateful1 = Stateful#stateful{req_map = ReqCtxMap1},
    {Stateful1, [TimerC]}.

