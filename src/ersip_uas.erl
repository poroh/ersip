%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% UAS processing
%%

-module(ersip_uas).

-export([process_request/3]).

-export_type([uas/0]).
%%%===================================================================
%%% Types
%%%===================================================================

-record(uas, {allowed_methods   :: ersip_method_set:set(),
              request           :: undefined | ersip_sipmsg:sipmsg(),
              options           :: options()
             }).
-type uas() :: #uas{}.
-type options() :: #{supported    => ersip_hdr_opttag_list:option_tag_list(),
                     to_tag       => auto | ersip_hdr_fromto:tag(),
                     check_scheme => fun((binary()) -> boolean())
                    }.

-type process_result() :: {reply, ersip_sipmsg:sipmsg()}
                        | {process, ersip_sipmsg:sipmsg()}.

-type result() :: process_result()
                | {error, {parse_error, term()}}.

%%%===================================================================
%%% API
%%%===================================================================

-spec process_request(ersip_msg:message() | ersip_sipmsg:sipmsg(), ersip_method_set:set(), options()) -> result().
process_request(Message, AllowedMethods, Options) ->
    case ersip_sipmsg:parse(Message, []) of
        {ok, SipMsg} ->
            UAS = new(SipMsg, AllowedMethods, Options),
            do_process_request(SipMsg, UAS);
        {error, Reason} ->
            {error, {parse_error, Reason}}
    end.

%%%===================================================================
%%% Internal implementation
%%%===================================================================
-define(default_options,
        #{supported => ersip_hdr_opttag_list:from_list([]),
          to_tag    => auto,
          check_scheme => fun check_sip_scheme/1
         }).

-spec new(ersip_sipmsg:sipmsg(), ersip_method_set:set(), options()) -> uas().
new(SipMsg, AllowedMethods, InOptions) ->
    Options = maps:merge(?default_options, InOptions),
    ACK = ersip_method:ack(),
    case ersip_sipmsg:method(SipMsg) of
        ACK ->
            error({api_error, <<"ACK cannot create UAS">>});
        _ ->
            ok
    end,
    #uas{allowed_methods = AllowedMethods,
         request         = SipMsg,
         options         = Options
        }.

%% 8.2 UAS Behavior
-spec do_process_request(ersip_sipmsg:sipmsg(), uas()) -> process_result().
do_process_request(SipMsg, UAS) ->
    R = steps([fun method_inspection/2,
               fun header_inspection/2
              ],
               SipMsg,
              UAS),
    case R of
        {continue, SipMsg1} ->
            {process, SipMsg1};
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
    SchemeBin = ersip_uri:scheme_bin(RURI),
    case SchemeCheckF(SchemeBin) of
        true ->
            continue;
        false ->
            Reply = make_unsupported_scheme(SipMsg, Options, ersip_uri:scheme(RURI)),
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

-spec create_reply_params(ersip_status:code(), ersip_status:reason() | auto, options()) -> ersip_reply:options().
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
check_sip_scheme(<<"sip">>) ->
    true;
check_sip_scheme(<<"sips">>) ->
    true;
check_sip_scheme(_) ->
    false.
