%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAS object
%%

-module(ersip_uas).

-export([new/3,
         request/2,
         reply/2,
         timer/2
        ]).

-export_type([uas/0]).
%%%===================================================================
%%% Types
%%%===================================================================

-record(uas, {allowed_methods   :: ersip_method_set:set(),
              request           :: undefined | ersip_sipmsg:sipmsg(),
              trans             :: stateless | ersip_trans:trans(),
              options           :: options()
             }).
-type uas() :: #uas{}.
-type options() :: #{sip          => ersip:sip_options(),
                     stateless    => boolean(),
                     supported    => ersip_hdr_opttag_list:option_tag_list(),
                     to_tag       => auto | ersip_hdr_fromto:tag(),
                     check_scheme => fun((ersip_uri:scheme()) -> boolean())
                    }.
-type result() :: {uas(), [ersip_ua_se:effect()]}.
-type timer_event() :: {timer, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new(ersip_sipmsg:sipmsg(), ersip_method_set:set(), options()) -> result().
new(SipMsg, AllowedMethods, UASOptions) ->
    new_impl(SipMsg, AllowedMethods, UASOptions).


-spec request(ersip_sipmsg:sipmsg(), uas()) -> result().
request(_RequestSipMsg, #uas{trans = stateless}) ->
    error({api_error, <<"Request cannot match stateless UAS">>});
request(RequestSipMsg, #uas{trans = Trans0} = UAS0) ->
    {Trans1, T1SE} = ersip_trans:event({received, RequestSipMsg}, Trans0),
    UAS1 = UAS0#uas{trans = Trans1},
    process_trans_se(T1SE, {UAS1, []}).

-spec reply(ersip_sipmsg:sipmsg(), uas()) -> result().
reply(RespMsg, #uas{trans = stateless} = UAS) ->
    Code = ersip_sipmsg:status(RespMsg),
    case ersip_status:response_type(Code) of
        provisional ->
            {UAS, [ersip_ua_se:send_response(RespMsg)]};
        final ->
            {UAS, [ersip_ua_se:send_response(RespMsg), ersip_ua_se:completed(normal)]}
    end;
reply(RespSipMsg, #uas{trans = Trans0} = UAS0) ->
    {Trans1, T1SE} = ersip_trans:event({send, RespSipMsg}, Trans0),
    UAS1 = UAS0#uas{trans = Trans1},
    process_trans_se(T1SE, {UAS1, []}).

-spec timer(timer_event(), uas()) -> result().
timer(_TimerEv, #uas{trans = stateless} ) ->
    error({api_error, <<"Unexpected timer event for stateless UAS">>});
timer(TimerEv, #uas{trans = Trans0} = UAS0) ->
    {Trans1, T1SE} = ersip_trans:event(TimerEv, Trans0),
    UAS1 = UAS0#uas{trans = Trans1},
    process_trans_se(T1SE, {UAS1, []}).

%%%===================================================================
%%% Internal implementation
%%%===================================================================
-define(default_options,
        #{sip       => #{}, %% use trasnsactions defaults
          stateless => false,
          supported => ersip_hdr_opttag_list:from_list([]),
          to_tag    => auto,
          check_scheme => fun check_sip_scheme/1
         }).

-spec new_impl(ersip_sipmsg:sipmsg(), ersip_method_set:set(), options()) -> result().
new_impl(SipMsg, AllowedMethods, InOptions) ->
    Options = maps:merge(?default_options, InOptions),
    ACK = ersip_method:ack(),
    case ersip_sipmsg:method(SipMsg) of
        ACK ->
            error({api_error, <<"ACK cannot create UAS">>});
        _ ->
            ok
    end,
    {Trans, SEs} =
        case maps:get(stateless, Options) of
            true ->
                {stateless, [{tu_result, SipMsg}]};
            false ->
                ersip_trans:new_server(SipMsg, maps:get(sip, Options))
        end,
    UAS = #uas{allowed_methods = AllowedMethods,
               request         = undefined,
               trans           = Trans,
               options         = Options
              },
    process_trans_se(SEs, {UAS, []}).


-spec process_trans_se([ersip_trans_se:effect()], result()) -> result().
process_trans_se([], Result) ->
    Result;
process_trans_se([{tu_result, Request}|Rest], {UAS, _} = Result0) ->
    case process_request(Request, UAS) of
        {reply, SipMsg} ->
            case UAS#uas.trans of
                stateless ->
                    %% Respond immediately in case of stateless UAS
                    Result1 = add_se(ersip_ua_se:send_response(SipMsg), Result0),
                    %% Finish UAS immediately
                    add_se(ersip_ua_se:completed(normal), Result1);
                Trans0 ->
                    %% Pass response through transaction for stateful
                    %% UAS
                    {UAS, UASSE} = Result0,
                    {Trans1, T1SE} = ersip_trans:event({send, SipMsg}, Trans0),
                    Result1 = {UAS#uas{trans = Trans1}, UASSE},
                    process_trans_se(Rest ++ T1SE, Result1)
            end;
        {pass, R} ->
            {UAS, UASSE} = Result0,
            Result1 = {UAS#uas{request = R}, UASSE},
            Result2 = add_se(ersip_ua_se:ua_result(R), Result1),
            process_trans_se(Rest, Result2)
    end;
process_trans_se([{send_response, SipMsg}|Rest], Result0) ->
    Result1 = add_se(ersip_ua_se:send_response(SipMsg), Result0),
    process_trans_se(Rest, Result1);
process_trans_se([{send_request, OutReq}|Rest], Result0) ->
    Result1 = add_se(ersip_ua_se:send_request(OutReq), Result0),
    process_trans_se(Rest, Result1);
process_trans_se([{set_timer, {Timeout, TimerEv}}|Rest], Result0) ->
    Result1 = add_se(ersip_ua_se:set_timer(Timeout, TimerEv), Result0),
    process_trans_se(Rest, Result1);
process_trans_se([{clear_trans, Reason}|Rest], Result0) ->
    Result1 = add_se(ersip_ua_se:completed(Reason), Result0),
    process_trans_se(Rest, Result1).

-spec add_se(ersip_ua_se:effect(), result()) -> result().
add_se(Effect, {UAS, UASideEffects}) ->
    {UAS, UASideEffects ++ [Effect]}.

%% 8.2 UAS Behavior
-type process_result() :: {reply, ersip_sipmsg:sipmsg()}
                        | {pass, ersip_sipmsg:sipmsg()}.
-spec process_request(ersip_sipmsg:sipmsg(), uas()) -> process_result().
process_request(SipMsg, UAS) ->
    R = steps([fun method_inspection/2,
               fun header_inspection/2
              ],
               SipMsg,
              UAS),
    case R of
        {continue, SipMsg1} ->
            {pass, SipMsg1};
        {reply, _} = Reply ->
            Reply
    end.

-type step_result() :: steps_result()
                     | continue.
-type steps_result() :: {reply, ersip_sipmsg:sipmsg()}
                      | {continue, ersip_sipmsg:sipmsg()}.

-spec steps([StepFun], ersip_sipmsg:sipmsg(), uas()) -> steps_result() when
      StepFun :: fun((ersip_sipmsg:sipmsg(), uas()) -> step_result()).
steps([], SipMsg, _UAS) ->
    {continue, SipMsg};
steps([StepF|Rest], SipMsg, UAS) ->
    case StepF(SipMsg, UAS) of
        {reply, _} = Reply ->
            Reply;
        continue ->
            steps(Rest, SipMsg, UAS);
        {continue, SipMsg1} ->
            steps(Rest, SipMsg1, UAS)
    end.

%% 8.2.1 Method Inspection
-spec method_inspection(ersip_sipmsg:sipmsg(), uas()) -> step_result().
method_inspection(SipMsg, #uas{allowed_methods = SupportedMethodSet, options = Options}) ->
    case ersip_method_set:has(ersip_sipmsg:method(SipMsg), SupportedMethodSet) of
        false ->
            %% If the UAS recognizes but does not support the method
            %% of a request, it MUST generate a 405 (Method Not
            %% Allowed) response.
            Reply0 = create_reply_params(405, auto, Options),
            ReplySipMsg0 = ersip_sipmsg:reply(Reply0, SipMsg),
            %% The UAS MUST also add an Allow header field to the 405
            %% (Method Not Allowed) response.  The Allow header field
            %% MUST list the set of methods supported by the UAS
            %% generating the message.
            AllowHdr = ersip_hdr_allow:from_method_set(SupportedMethodSet),
            ReplySipMsg1 = ersip_sipmsg:set(allow, AllowHdr, ReplySipMsg0),
            {reply, ReplySipMsg1};
        true ->
            %% If the method is one supported by the server,
            %% processing continues.
            continue
    end.

-spec header_inspection(ersip_sipmsg:sipmsg(), uas()) -> steps_result().
header_inspection(SipMsg, #uas{options = Options} = UAS) ->
    case ersip_sipmsg:parse(SipMsg, [to, callid, cseq, require]) of
        {error, _} = ParseError ->
            {reply, make_bad_request(SipMsg, ParseError, Options)};
        {ok, SipMsg1} ->
            %% We cannot check loops & merged requests here because it
            %% requires global context
            steps([fun check_ruri/2,
                   fun maybe_check_require/2],
                  SipMsg1,
                  UAS)
    end.

-spec check_ruri(ersip_sipmsg:sipmsg(), uas()) -> step_result().
check_ruri(SipMsg, #uas{options = Options}) ->
    SchemeCheckF = maps:get(check_scheme, Options),
    RURI = ersip_sipmsg:ruri(SipMsg),
    Scheme = ersip_uri:scheme(RURI),
    case SchemeCheckF(Scheme) of
        true ->
            continue;
        false ->
            Reply = make_unsupported_scheme(SipMsg, Options, Scheme),
            {reply, Reply}
    end.

-spec maybe_check_require(ersip_sipmsg:sipmsg(), uas()) -> step_result().
maybe_check_require(SipMsg, UAS) ->
    ACK = ersip_method:ack(),
    CANCEL = ersip_method:cancel(),
    case ersip_sipmsg:method(SipMsg) of
        ACK ->
            continue;
        CANCEL ->
            continue;
        _ ->
            check_require(SipMsg, UAS)
    end.

-spec check_require(ersip_sipmsg:sipmsg(), uas()) -> step_result().
check_require(SipMsg, #uas{options = Options}) ->
    case ersip_sipmsg:find(require, SipMsg) of
        {ok, Require} ->
            SupportedExtensions = maps:get(supported, Options),
            case check_supported(Require, SupportedExtensions) of
                all_supported ->
                    continue;
                Unsupported ->
                    {reply, make_bad_extension(SipMsg, Options, Unsupported)}
            end;
        not_found ->
            continue
    end.

-spec create_reply_params(ersip_status:code(), ersip_status:reason() | auto, options()) -> ersip_reply:reply().
create_reply_params(Code, Reason, #{to_tag := ToTag}) ->
    ReplyParams0 =
        case Reason of
            auto ->
                [];
            Reason ->
                [{reason, Reason}]
        end,
    ReplyParams1 = [{to_tag, ToTag}|ReplyParams0],
    ersip_reply:new(Code, ReplyParams1).

-spec make_bad_request(ersip_sipmsg:sipmsg(), {error, term()}, options()) -> ersip_sipmsg:sipmsg().
make_bad_request(SipMsg, ParseError, Options) ->
    Reply = create_reply_params(400, ersip_status:bad_request_reason(ParseError), Options),
    ersip_sipmsg:reply(Reply, SipMsg).


-spec make_bad_extension(ersip_sipmsg:sipmsg(), options(), Unsupported) -> ersip_sipmsg:sipmsg() when
      Unsupported :: ersip_hdr_opttag_list:option_tag_list().
make_bad_extension(SipMsg, Options, Unsupported) ->
    Reply = create_reply_params(420, auto, Options),
    Resp0 = ersip_sipmsg:reply(Reply, SipMsg),
    ersip_sipmsg:set(unsupported, Unsupported, Resp0).

-spec make_unsupported_scheme(ersip_sipmsg:sipmsg(), options(), ersip_uri:scheme()) -> ersip_sipmsg:sipmsg().
make_unsupported_scheme(SipMsg, Options, Scheme) ->
    Reply = create_reply_params(416, ersip_status:unsupported_uri_scheme_reason(Scheme), Options),
    ersip_sipmsg:reply(Reply, SipMsg).

-spec check_supported(Required, Supported) -> all_supported | Unsupported when
      Required    :: ersip_hdr_opttag_list:option_tag_list(),
      Supported   :: ersip_hdr_opttag_list:option_tag_list(),
      Unsupported :: ersip_hdr_opttag_list:option_tag_list().
check_supported(Required, Supported) ->
    Intersect = ersip_hdr_opttag_list:intersect(Required, Supported),
    case Intersect =:= Required of
        true ->
            all_supported;
        false ->
            ersip_hdr_opttag_list:subtract(Required, Supported)
    end.

-spec check_sip_scheme(ersip_uri:scheme()) -> boolean().
check_sip_scheme({scheme, sip}) ->
    true;
check_sip_scheme({scheme, sips}) ->
    true;
check_sip_scheme(_) ->
    false.
