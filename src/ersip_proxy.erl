%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP proxy support
%%

-module(ersip_proxy).

-export([new_stateful/2,
         trans_result/3,
         forward_to/2,
         timer_fired/2,
         cancel/1
        ]).

-export_type([params/0,
              options/0,
              internal_trans_id/0,
              stateful/0
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
           timer_c => non_neg_integer(),

           %% Pass 503. RFC 3261 states:
           %% If the only response that was received is a 503, the proxy
           %% SHOULD generate a 500 response and forward that upstream.
           %%
           %% If this flag is set to true then proxy will not generate
           %% 500 on 503.
           pass_503 => boolean(),

           %% How long we need wait after 200 OK response is received
           %% on CANCEL until we consider request cancelled.
           cancel_timeout => non_neg_integer()
         }.


-record(stateful, {phase       :: stateful_phase(),
                   options     :: params(),                           %% Pass options
                   orig_sipmsg :: ersip_sipmsg:sipmsg(),              %% SIP message to be passed
                   fwd_sipmsg  :: ersip_sipmsg:sipmsg() | undefined,  %% SIP message to be forwarded
                   req_map   = #{} :: context_request_map(),
                   final_forwarded = false :: boolean()
                  }).
-type stateful_phase() :: init
                        | select_target
                        | collect
                        | cancelled.
-type stateful() :: #stateful{}.
-type stateful_result() :: {stateful(), [ersip_proxy_se:side_effect()]}.
-type internal_trans_id() :: any().
-type check_rroute_fun() :: fun((ersip_hdr_route:route()) -> boolean()).

-type context_request_map() :: #{ersip_branch:branch_key() => request_context()}.
-record(request_context,
        {key     :: ersip_branch:branch_key(),
         req     :: ersip_request:request(),
         timer_c :: reference() | undefined,
         resp    :: ersip_sipmsg:sipmsg() | undefined,
         provisional_received = false :: boolean(),
         timer_c_fired = false :: boolean(),
         cancel_sent   = false :: boolean()
        }).
-type request_context() :: #request_context{}.

-define(SERVER_TRANS_ID, server_trans).
-define(DEFAULT_BRANCH_ENTROPY, 7). %% Entropy (bytes) of the branch parameter

%%%===================================================================
%%% API
%%%===================================================================

%% @doc New stateful proxy request.
-spec new_stateful(ersip_sipmsg:sipmsg(), options()) -> stateful_result().
new_stateful(SipMsg, ProxyOptions) ->
    %% There is no client transaction for ACK.  If the TU
    %% wishes to send an ACK, it passes one directly to the
    %% transport layer for transmission.
    ACK = ersip_method:ack(),
    case ersip_sipmsg:method(SipMsg) of
        ACK ->
            error({api_error, {<<"cannot create stateful proxy request for ACK request">>, SipMsg}});
        _ ->
            ok
    end,
    Stateful = #stateful{phase       = init,
                         options     = ProxyOptions,
                         orig_sipmsg = SipMsg
                        },
    {Stateful, [ersip_proxy_se:create_trans(server, ?SERVER_TRANS_ID, SipMsg)]}.

-spec trans_result(internal_trans_id(), ersip_sipmsg:sipmsg(), stateful()) -> stateful_result().
trans_result(?SERVER_TRANS_ID, SipMsg0, #stateful{phase = init, options = ProxyOptions} = Stateful) ->
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
            URI = ersip_uri:make([{scheme, sip}, {host, Host}]),
            forward_to(URI, Stateful1);
        _ ->
            RURI = ersip_sipmsg:ruri(SipMsg1),
            {Stateful1, [ersip_proxy_se:select_target(RURI)]}
    end;
trans_result(?SERVER_TRANS_ID, _SipMsg, #stateful{phase = cancelled} = Stateful) ->
    %% Request was cancelled while server transaction has been beeing
    %% created
    early_cancel_request(Stateful);
trans_result({cancel, BranchKey}, timeout, #stateful{req_map = ReqCtxMap} = Stateful) ->
    case ReqCtxMap of
        #{BranchKey := #request_context{req = Req}} ->
            %% Behave as if we received 408 on CANCEL request, to do
            %% this we just generate the same request as we has sent
            %% before and then generate 408 reply on it.
            CancelReq = ersip_request_cancel:generate(Req),
            Resp = ersip_sipmsg:reply(408, ersip_request:sipmsg(CancelReq)),
            process_response(BranchKey, Resp, Stateful);
        _ ->
            error({api_error, {<<"Unexpected cancel transaction result">>, BranchKey}})
    end;
trans_result({cancel, BranchKey}, Resp, #stateful{phase = collect} = Stateful) ->
    process_response(BranchKey, Resp, Stateful);
trans_result(BranchKey, timeout, #stateful{phase = collect, fwd_sipmsg = FwdSipMsg} = Stateful) ->
    %% If there are no final responses in the context, the proxy MUST
    %% send a 408 (Request Timeout) response to the server
    %% transaction.
    Resp = ersip_sipmsg:reply(408, FwdSipMsg),
    process_response(BranchKey, Resp, Stateful);
trans_result(BranchKey, Resp, #stateful{phase = collect} = Stateful) ->
    process_response(BranchKey, Resp, Stateful).

%% @doc Forward request to selected target. If list of URIs is
%% specified then all requests are forwarded simultaneously (forked).
%% Caller may specify branch for each target by using tuple {URI, Branch}.
%%
%% If branch is not specified then it is selected randomly generated
%% from ?DEFAULT_BRANCH_ENTROPY random bytes.
-spec forward_to(Target | [Target], stateful()) -> stateful_result() when
      Target :: ersip_uri:uri() | {ersip_uri:uri(), ersip_branch:branch()}.
forward_to(Target, #stateful{} = Stateful) when not is_list(Target) ->
    forward_to([Target], Stateful);
forward_to(_TargetList, #stateful{phase = cancelled} = Stateful) ->
    early_cancel_request(Stateful);
forward_to([], #stateful{phase = select_target, orig_sipmsg = SipMsg} = Stateful) ->
    %% Target not found case:
    RespMsg = ersip_sipmsg:reply(404, SipMsg),
    SE = [ersip_proxy_se:response(?SERVER_TRANS_ID, RespMsg),
          ersip_proxy_se:stop()
         ],
    {Stateful, SE};
forward_to(TargetList, #stateful{phase = select_target, options = ProxyOptions, fwd_sipmsg = FwdMsg} = Stateful) ->
    Requests =
        lists:map(fun({Target, Branch}) ->
                          {SipMsg, #{nexthop := NexthopURI}} = ersip_proxy_common:forward_request(Target, FwdMsg, ProxyOptions),
                          ersip_request:new(SipMsg, Branch, NexthopURI);
                     (Target) ->
                          {SipMsg, #{nexthop := NexthopURI}} = ersip_proxy_common:forward_request(Target, FwdMsg, ProxyOptions),
                          ersip_request:new(SipMsg, ersip_branch:make_random(?DEFAULT_BRANCH_ENTROPY), NexthopURI)
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
    Stateful1 = Stateful#stateful{phase = collect,
                                  req_map = maps:from_list(BranchReqCtxList)
                                 },
    {Stateful1, SideEffects}.

-spec timer_fired(ersip_proxy_se:timer_event(), stateful()) -> stateful_result().
timer_fired({timer, BranchKey, TimerCRef}, #stateful{req_map = ReqCtxMap} = Stateful) ->
    case ReqCtxMap of
        #{BranchKey := #request_context{timer_c = TimerCRef} = ReqCtx} ->
            process_timer_c_fired(BranchKey, ReqCtx, Stateful);
        _ ->
            %% Otherwise ignore event:
            {Stateful, []}
    end;
timer_fired({cancel_timer, BranchKey}, #stateful{req_map = ReqCtxMap} = Stateful) ->
    case ReqCtxMap of
        #{BranchKey := #request_context{resp = undefined, req = Req}} ->
            %% If we still does not have any final response from the
            %% other side - generate 487 and delete transaction
            Resp = ersip_sipmsg:reply(487, ersip_request:sipmsg(Req)),
            {Stateful1, SE} = process_response(BranchKey, Resp, Stateful),
            {Stateful1, [ersip_proxy_se:delete_trans(BranchKey) | SE]};
        _ ->
            %% Otherwise ignore event:
            {Stateful, []}
    end.

-spec cancel(stateful()) -> stateful_result().
cancel(#stateful{phase = init} = Stateful) ->
    %% Just wait when server transaction is created
    Stateful1 = Stateful#stateful{phase = cancelled},
    {Stateful1, []};
cancel(#stateful{phase = select_target} = Stateful) ->
    %% Canceling request before it passed through proxy
    Stateful1 = Stateful#stateful{phase = cancelled},
    {Stateful1, []};
cancel(#stateful{phase = collect} = Stateful) ->
    cancel_all_pending(Stateful);
cancel(#stateful{phase = cancelled} = Stateful) ->
    %% Ignore duplicated cancel
    {Stateful, []}.

%%%===================================================================
%%% Internal Implementation
%%%===================================================================
-type pr_function() :: fun((ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result()).
-type pr_result() :: stop
                     %% Continue processing
                   | continue
                     %% Continue processing with new state
                   | {continue, stateful()}
                     %% Continue processing with new state and produce
                     %% side effect.
                   | {continue, stateful_result()}
                     %% Continue processing with new flow
                   | {continue_with, [pr_function()]}
                     %% Continue processing with new flow and new response
                   | {continue_with, [pr_function()], ersip_branch:branch_key(), ersip_sipmsg:sipmsg()}.

-spec process_response(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> stateful_result().
process_response(BranchKey, RespSipMsg, #stateful{} = Stateful) ->
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
                        {Stateful, []}).

-spec do_process_response([pr_function()], ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful_result()) -> stateful_result().
do_process_response([], _, _, {Stateful, RevSe}) ->
    {Stateful, lists:reverse(RevSe)};
do_process_response([F | Rest], BranchKey, RespSipMsg, {Stateful, RevSE} = Acc) ->
    case F(BranchKey, RespSipMsg, Stateful) of
        continue ->
            do_process_response(Rest, BranchKey, RespSipMsg, Acc);
        {continue, #stateful{} = Stateful1} ->
            do_process_response(Rest, BranchKey, RespSipMsg, {Stateful1, RevSE});
        {continue, {#stateful{} = Stateful1, SE1}} ->
            do_process_response(Rest, BranchKey, RespSipMsg, {Stateful1, lists:reverse(SE1) ++ RevSE});
        {continue_with, FList} ->
            do_process_response(FList, BranchKey, RespSipMsg, Acc);
        {continue_with, FList, NewBranchKey, NewRespSipMsg} ->
            do_process_response(FList, NewBranchKey, NewRespSipMsg, Acc);
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
            error({api_error, {<<"Unexpected transaction result">>, BranchKey}})
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
pr_process_via(_BranchKey, RespSipMsg, #stateful{}) ->
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
            {continue_with, [fun pr_process_selfgenerated/3]};
        _ ->
            continue
    end.

-spec pr_process_selfgenerated(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_process_selfgenerated(BranchKey, RespSipMsg, #stateful{req_map = ReqCtxMap} = Stateful) ->
    case ersip_sipmsg:status(RespSipMsg) of
        Code when Code >= 200 andalso Code =< 299 ->
            %% 200 OK final code on CANCEL - just restart timer_c,
            %% wait for response from server.
            {continue, set_cancel_timer(BranchKey, Stateful)};
        _ ->
            %% Consider request as cancelled:
            #{BranchKey := ReqCtx} = ReqCtxMap,
            case ReqCtx of
                #request_context{timer_c_fired = true, req = Req} ->
                    %% In case of Timer C is fired behave as 408 for
                    %% this branch.
                    Resp = ersip_sipmsg:reply(408, ersip_request:sipmsg(Req)),
                    {Stateful1, SE} = process_response(BranchKey, Resp, Stateful),
                    {continue, {Stateful1, [ersip_proxy_se:delete_trans(BranchKey) | SE]}};
                #request_context{timer_c_fired = false, req = Req} ->
                    %% Consider request as cancelled
                    Resp = ersip_sipmsg:reply(487, ersip_request:sipmsg(Req)),
                    {Stateful1, SE} = process_response(BranchKey, Resp, Stateful),
                    {continue, {Stateful1, [ersip_proxy_se:delete_trans(BranchKey) | SE]}}
            end
    end.

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
    case ersip_status:response_type(ersip_sipmsg:status(RespSipMsg)) of
        final ->
            #{BranchKey := ReqCtx} = ReqCtxMap,
            ReqCtx1 = ReqCtx#request_context{resp = RespSipMsg},
            ReqCtxMap1 = ReqCtxMap#{BranchKey := ReqCtx1},
            {continue, Stateful#stateful{req_map = ReqCtxMap1}};
        provisional ->
            #{BranchKey := ReqCtx} = ReqCtxMap,
            ReqCtx1 = ReqCtx#request_context{provisional_received = true},
            ReqCtxMap1 = ReqCtxMap#{BranchKey := ReqCtx1},
            {continue, Stateful#stateful{req_map = ReqCtxMap1}}
    end.

-spec pr_maybe_forward_response(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_maybe_forward_response(_BranchKey, RespSipMsg, #stateful{}) ->
    %% Until a final response has been sent on the server transaction,
    %% the following responses MUST be forwarded immediately:
    %%
    %% -  Any provisional response other than 100 (Trying)
    %%
    %% -  Any 2xx response
    case ersip_sipmsg:status(RespSipMsg) of
        Code when Code >= 101 andalso Code =< 199  ->
            {continue_with, [fun pr_forward_1xx_response/3]};
        Code when Code >= 200 andalso Code =< 299 ->
            {continue_with, [fun pr_forward_2xx_response/3]};
        Code when Code >= 600 ->
            %% If a 6xx response is received, it is not immediately forwarded,
            %% but the stateful proxy SHOULD cancel all client pending
            %% transactions as described in Section 10, and it MUST NOT create
            %% any new branches in this context.
            %%
            %% This is a change from RFC 2543, which mandated that the proxy
            %% was to forward the 6xx response immediately.  For an INVITE
            %% transaction, this approach had the problem that a 2xx response
            %% could arrive on another branch, in which case the proxy would
            %% have to forward the 2xx.  The result was that the UAC could
            %% receive a 6xx response followed by a 2xx response, which should
            %% never be allowed to happen.  Under the new rules, upon
            %% receiving a 6xx, a proxy will issue a CANCEL request, which
            %% will generally result in 487 responses from all outstanding
            %% client transactions, and then at that point the 6xx is
            %% forwarded upstream.
            {continue_with, [fun pr_process_6xx_response/3]};
        _ ->
            {continue_with, [fun pr_choose_best_response/3]}
    end.

-spec pr_forward_1xx_response(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_forward_1xx_response(_BranchKey, _RespMsg, #stateful{}) ->
    %% Normative:
    %% Any response chosen for immediate forwarding MUST be processed
    %% as described in steps "Aggregate Authorization Header Field
    %% Values" through "Record-Route".
    %%
    %% Implementation: we do not pass provisional response through
    %% "Aggregate Authorization Header Field" step because it
    %% applicable only for 401 and 407 codes.
    {continue_with,
     [fun pr_update_record_route/3,
      fun pr_forward_response/3
     ]}.

-spec pr_forward_2xx_response(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_forward_2xx_response(_BranchKey, _RespMsg, #stateful{}) ->
    %% Normative:
    %% Any response chosen for immediate forwarding MUST be processed
    %% as described in steps "Aggregate Authorization Header Field
    %% Values" through "Record-Route".
    %%
    %% Implementation: we do not pass provisional response through
    %% "Aggregate Authorization Header Field" step because it
    %% applicable only for 401 and 407 codes.
    {continue_with,
     [fun pr_update_record_route/3,
      fun pr_forward_response/3,
      fun pr_cancel_other/3,
      fun pr_maybe_stop_proxy/3
     ]}.


-spec pr_process_6xx_response(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_process_6xx_response(_BranchKey, _RespMsg, #stateful{}) ->
    %% If a 6xx response is received, it is not immediately forwarded,
    %% but the stateful proxy SHOULD cancel all client pending
    %% transactions as described in Section 10, and it MUST NOT create
    %% any new branches in this context.
    %%
    %% This is a change from RFC 2543, which mandated that the proxy
    %% was to forward the 6xx response immediately.  For an INVITE
    %% transaction, this approach had the problem that a 2xx response
    %% could arrive on another branch, in which case the proxy would
    %% have to forward the 2xx.  The result was that the UAC could
    %% receive a 6xx response followed by a 2xx response, which should
    %% never be allowed to happen.  Under the new rules, upon
    %% receiving a 6xx, a proxy will issue a CANCEL request, which
    %% will generally result in 487 responses from all outstanding
    %% client transactions, and then at that point the 6xx is
    %% forwarded upstream.
    {continue_with,
     [fun pr_cancel_other/3,
      fun pr_postprocess_6xx/3]}.

-spec pr_postprocess_6xx(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_postprocess_6xx(_BranchKey, _Resp6xx, #stateful{req_map = ReqCtxMap}) ->
    ReqCtxList = maps:values(ReqCtxMap),
    case all_terminated(ReqCtxList) of
        true ->
            %% If all requests are terminated then forwarward 6xx
            %% response.
            {continue_with,
             [fun pr_update_record_route/3,
              fun pr_forward_response/3,
              fun pr_stop_proxy/3
             ]};
        false ->
            %% If some requests are pending then wait request
            %% cancellation
            continue
    end.

-spec pr_choose_best_response(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_choose_best_response(_BranchKey, _RespMsg, #stateful{final_forwarded = WasFinal, req_map = ReqCtxMap, options = ProxyOptions}) ->
    ReqCtxList = maps:values(ReqCtxMap),
    Action =
        case {all_terminated(ReqCtxList), WasFinal} of
            {false, _} ->
                wait_more;     %% Wait more responses
            {true, true} ->
                stop_processg; %% Stop stateful proxy processing
            {true, false} ->
                select_best    %% Select best response among received responses
        end,
    case Action of
        wait_more ->
            stop;
        stop_processg ->
            {continue_with, [fun pr_stop_proxy/3]};
        select_best ->
            %% A stateful proxy MUST send a final response to a response
            %% context's server transaction if no final responses have been
            %% immediately forwarded by the above rules and all client
            %% transactions in this response context have been terminated.
            {BestRespBranchKey, RespCandidate} = choose_best_response(ReqCtxList),
            BestResp = pr_maybe_patch_503(RespCandidate, ProxyOptions),
            {continue_with,
             [fun pr_aggregate_auth_header/3,
              fun pr_update_record_route/3,
              fun pr_forward_response/3,
              fun pr_stop_proxy/3
              %% We do not need cancel requests here because all
              %% requests has been already terminated.
             ],
             BestRespBranchKey,
             BestResp}
    end.

-spec pr_maybe_patch_503(ersip_sipmsg:sipmsg(), options()) -> ersip_sipmsg:sipmsg().
pr_maybe_patch_503(RespCandidate, #{pass_503 := true} = _ProxyOptions) ->
    RespCandidate;
pr_maybe_patch_503(RespCandidate, #{} = _ProxyOptions) ->
    case ersip_sipmsg:status(RespCandidate) of
        503 ->
            %% If the only response that was received is a 503, the proxy
            %% SHOULD generate a 500 response and forward that upstream.
            ersip_sipmsg:set_status(500, RespCandidate);
        _ ->
            RespCandidate
    end.

-spec pr_update_record_route(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_update_record_route(_BranchKey, _Resp, #stateful{}) ->
    %% TODO: eventually implement this.
    continue.

-spec pr_aggregate_auth_header(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_aggregate_auth_header(_BranchKey, _Resp, #stateful{}) ->
    %% TODO: eventually implement this.
    continue.

-spec pr_forward_response(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_forward_response(_BranchKey, RespMsg, #stateful{} = Stateful) ->
    Stateful1 =
        case ersip_status:response_type(ersip_sipmsg:status(RespMsg)) of
            final ->
                Stateful#stateful{final_forwarded = true};
            provisional ->
                Stateful
        end,
    {continue, {Stateful1, [ersip_proxy_se:response(?SERVER_TRANS_ID, RespMsg)]}}.

-spec pr_cancel_other(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_cancel_other(_BranchKey, _RespMsg, #stateful{} = Stateful) ->
    %% Normative:
    %% If the forwarded response was a final response, the proxy MUST
    %% generate a CANCEL request for all pending client transactions
    %% associated with this response context.
    %%
    %% Implementation:
    %% We do not call pr_cancel_other for non-final response. So we
    %% always cancel all pending requests. Note that by definition of
    %% is_request_pending and because we add all responses to response
    %% context we cannot try to cancel request that produced forwarded
    %% response.
    {continue, cancel_all_pending(Stateful)}.

-spec pr_stop_proxy(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_stop_proxy(_BranchKey, _RespMsg, #stateful{} = Stateful) ->
    {continue, {Stateful, [ersip_proxy_se:stop()]}}.

-spec pr_maybe_stop_proxy(ersip_branch:branch_key(), ersip_sipmsg:sipmsg(), stateful()) -> pr_result().
pr_maybe_stop_proxy(BranchKey, RespMsg, #stateful{req_map = ReqCtxMap} = Stateful) ->
    ReqCtxList = maps:values(ReqCtxMap),
    case all_terminated(ReqCtxList) of
        true ->
            pr_stop_proxy(BranchKey, RespMsg, Stateful);
        false ->
            continue
    end.

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
    lists:reverse(Acc);
create_client_trans([{BranchKey, #request_context{timer_c = undefined, req = Req}} | Rest], ProxyOptions, Acc) ->
    ClientTrans = ersip_proxy_se:create_trans(client, BranchKey, Req),
    create_client_trans(Rest, ProxyOptions, [ClientTrans | Acc]);
create_client_trans([{BranchKey, #request_context{timer_c = TimerCRef, req = Req}} | Rest], ProxyOptions, Acc) ->
    ClientTrans = ersip_proxy_se:create_trans(client, BranchKey, Req),
    TimerC = ersip_proxy_se:set_timer(timer_c_timeout(ProxyOptions), {timer, BranchKey, TimerCRef}),
    create_client_trans(Rest, ProxyOptions, [ClientTrans, TimerC | Acc]).

-spec create_cancel_client_trans([{ersip_branch:branch_key(), request_context()}], options(), stateful_result()) -> stateful_result().
create_cancel_client_trans([], _ProxyOptions, Acc) ->
    Acc;
create_cancel_client_trans([{_, #request_context{} = ReqCtx} | Rest], ProxyOptions, {Stateful, Acc}) ->
    {Stateful1, SE1} = maybe_send_cancel(ReqCtx, Stateful),
    create_cancel_client_trans(Rest, ProxyOptions, {Stateful1, SE1 ++ Acc}).

-spec timer_c_timeout(stateful() | options()) -> pos_integer().
timer_c_timeout(#stateful{options = ProxyOptions}) ->
    timer_c_timeout(ProxyOptions);
timer_c_timeout(#{timer_c := TimerC}) ->
    TimerC;
timer_c_timeout(_) ->
    timer:seconds(180).

-spec reset_timer_c(ersip_branch:branch_key(), stateful()) -> stateful_result().
reset_timer_c(BranchKey, #stateful{req_map = ReqCtxMap} = Stateful) ->
    #{BranchKey := ReqCtx} = ReqCtxMap,
    TimerCRef = make_ref(),
    TimerC = ersip_proxy_se:set_timer(timer_c_timeout(Stateful), {timer, BranchKey, TimerCRef}),
    ReqCtx1 = ReqCtx#request_context{timer_c = TimerCRef},
    ReqCtxMap1 = ReqCtxMap#{BranchKey := ReqCtx1},
    Stateful1 = Stateful#stateful{req_map = ReqCtxMap1},
    {Stateful1, [TimerC]}.

-spec cancel_timeout(stateful() | options()) -> pos_integer().
cancel_timeout(#stateful{options = ProxyOptions}) ->
    cancel_timeout(ProxyOptions);
cancel_timeout(#{cancel_timeout := CancelTimeout}) ->
    CancelTimeout;
cancel_timeout(_) ->
    %% TODO: By RFC3261 it should be 64*T1
    timer:seconds(32).

-spec set_cancel_timer(ersip_branch:branch_key(), stateful()) -> stateful_result().
set_cancel_timer(BranchKey, #stateful{} = Stateful) ->
    CancelTimer = ersip_proxy_se:set_timer(cancel_timeout(Stateful), {cancel_timer, BranchKey}),
    {Stateful, [CancelTimer]}.

%% @private
%% @doc All transactions within request context is terminated.
-spec all_terminated([request_context()]) -> boolean().
all_terminated(ReqCtxList) ->
    lists:all(fun(#request_context{resp = undefined}) ->
                      false;
                 (#request_context{resp = Resp}) ->
                      ersip_sipmsg:status(Resp) >= 200
              end,
              ReqCtxList).

-spec is_request_pending(request_context()) -> boolean().
is_request_pending(#request_context{resp = undefined}) ->
    true;
is_request_pending(#request_context{resp = Resp}) ->
    ersip_sipmsg:status(Resp) < 200.

-spec choose_best_response([request_context()]) -> {ersip_branch:branch_key(), ersip_sipmsg:sipmsg()}.
choose_best_response(ReqCtxList) ->
    %% Sort all responses by comparision class and select minimal
    %% (most preferred) response.
    AllResps = [{code_comparision_class(ersip_sipmsg:status(Resp)), {BranchKey, Resp}}
                || #request_context{key = BranchKey, resp = Resp} <- ReqCtxList],
    [{_ComparisionClass, {_, _} = Result} | _] = lists:sort(AllResps),
    Result.

%% @doc Select class of response that agree with status preference
%% (lower value related to higher preference of the response).
%% For 6xx it is always {1, 1}, For preferred 4xx it is {4, 1..3},
%% for all other codes it is {StatusCode div 100, 5}.
-spec code_comparision_class(ersip_status:code()) -> {pos_integer(), pos_integer()}.
code_comparision_class(Code) when Code >= 600 ->
    %% It MUST choose from the 6xx class responses if any exist in the
    %% context.
    {1, 1};
    %% The proxy MAY select any response within that chosen class.
    %% The proxy SHOULD give preference to responses that provide
    %% information affecting resubmission of this request, such as
    %% 401, 407, 415, 420, and 484 if the 4xx class is chosen.
code_comparision_class(401) -> {4, 1};
code_comparision_class(407) -> {4, 1};
code_comparision_class(484) -> {4, 2};
code_comparision_class(415) -> {4, 3};
code_comparision_class(420) -> {4, 3};
code_comparision_class(503) ->
    %% A proxy which receives a 503 (Service Unavailable) response
    %% SHOULD NOT forward it upstream unless it can determine that any
    %% subsequent requests it might proxy will also generate a 503.
    %% In other words, forwarding a 503 means that the proxy knows it
    %% cannot service any requests, not just the one for the Request-
    %% URI in the request which generated the 503.  If the only
    %% response that was received is a 503, the proxy SHOULD generate
    %% a 500 response and forward that upstream.
    {4, 6};
code_comparision_class(Code) ->
    {Code div 100, 5}.

-spec cancel_all_pending(stateful()) -> stateful_result().
cancel_all_pending(#stateful{orig_sipmsg = SipMsg, req_map = ReqCtxMap, options = Options} = Stateful) ->
    INVITE = ersip_method:invite(),
    case ersip_sipmsg:method(SipMsg) of
        INVITE ->
            ToBeCancelled = [{BranchKey, ReqCtx}
                             || {BranchKey, ReqCtx} <- maps:to_list(ReqCtxMap),
                                is_request_pending(ReqCtx)],
            create_cancel_client_trans(ToBeCancelled, Options, {Stateful, []});
        _ ->
            %% RFC 3261 9.1 Client Behavior:
            %% A CANCEL request SHOULD NOT be sent to cancel a request
            %% other than INVITE.
            {Stateful, []}
    end.

-spec early_cancel_request(stateful()) -> stateful_result().
early_cancel_request(#stateful{orig_sipmsg = ReqSipMsg} = Stateful) ->
    RespMsg = ersip_sipmsg:reply(487, ReqSipMsg),
    Stateful1 = Stateful#stateful{phase = cancelled},
    SE = [ersip_proxy_se:response(?SERVER_TRANS_ID, RespMsg),
          ersip_proxy_se:stop()
         ],
    {Stateful1, SE}.

%% 16.8 Processing Timer C
%% If timer C should fire, the proxy MUST either reset the timer
%% with any value it chooses, or terminate the client transaction.
%% If the client transaction has received a provisional response,
%% the proxy MUST generate a CANCEL request matching that
%% transaction.  If the client transaction has not received a
%% provisional response, the proxy MUST behave as if the
%% transaction received a 408 (Request Timeout) response.
-spec process_timer_c_fired(ersip_branch:branch_key(), request_context(), stateful()) -> stateful_result().
process_timer_c_fired(BranchKey, #request_context{resp = undefined, req = Req, provisional_received = false}, #stateful{} = Stateful) ->
    %% No provisional response:
    Stateful1 = set_timer_c_fired(BranchKey, Stateful),
    Resp = ersip_sipmsg:reply(408, ersip_request:sipmsg(Req)),
    {Stateful2, SE2} = process_response(BranchKey, Resp, Stateful1),
    {Stateful2, [ersip_proxy_se:delete_trans(BranchKey) | SE2]};
process_timer_c_fired(BranchKey, #request_context{resp = undefined, provisional_received = true} = ReqCtx, #stateful{} = Stateful) ->
    %% If the client transaction has received a provisional response,
    %% the proxy MUST generate a CANCEL request matching that
    %% transaction.
    Stateful1 = set_timer_c_fired(BranchKey, Stateful),
    maybe_send_cancel(ReqCtx, Stateful1);
process_timer_c_fired(_BranchKey, #request_context{}, #stateful{} = Stateful) ->
    %% Just ignore this timer C because transaction result has been
    %% already received
    {Stateful, []}.


-spec set_timer_c_fired(ersip_branch:branch_key(), stateful()) -> stateful().
set_timer_c_fired(BranchKey, #stateful{req_map = ReqCtxMap} = Stateful) ->
    ReqCtxMap1 =
        maps:update_with(BranchKey,
                         fun(RCtx) -> RCtx#request_context{timer_c_fired = true} end,
                         ReqCtxMap),
    Stateful#stateful{req_map = ReqCtxMap1}.

-spec maybe_send_cancel(request_context(), stateful()) -> stateful_result().
maybe_send_cancel(#request_context{cancel_sent = true}, #stateful{} = Stateful) ->
    %% Never send second CANCEL request.
    {Stateful, []};
maybe_send_cancel(#request_context{key = BranchKey, req = InitialReq}, Stateful) ->
    CancelReq = ersip_request_cancel:generate(InitialReq),
    CancelClientTrans = ersip_proxy_se:create_trans(client, {cancel, BranchKey}, CancelReq),
    Stateful1 = set_cancel_sent(BranchKey, Stateful),
    {Stateful1, [CancelClientTrans]}.

-spec set_cancel_sent(ersip_branch:branch_key(), stateful()) -> stateful().
set_cancel_sent(BranchKey, #stateful{req_map = ReqCtxMap} = Stateful) ->
    ReqCtxMap1 =
        maps:update_with(BranchKey,
                         fun(RCtx) -> RCtx#request_context{cancel_sent = true} end,
                         ReqCtxMap),
    Stateful#stateful{req_map = ReqCtxMap1}.
