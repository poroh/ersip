%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP proxy common functions
%%

-module(ersip_proxy_common).

-export([ request_validation/2 ]).

%%%===================================================================
%%% Types
%%%===================================================================

-type validate_options() ::
        #{ %% Mandatory option: To tag used for replies
           to_tag         := ersip_hdr_fromto:tag(),
           %% Validator of the scheme.
           scheme_val_fun => scheme_val_fun(),
           %% Proxy MAY reply on OPTIONS request with Max-Forwards set
           %% to 0. This flag triggers this behavior.
           reply_on_options  => boolean(),
           %% Proxy parameters
           proxy_params      => proxy_params(),
           %% Optional loop detection is performed by proxy.
           loop_detect       => boolean()
         }.
-type validate_result()  :: { ok, ersip_sipmsg:sipmsg() }
                          | { reply, ersip_sipmsg:sipmsg() }
                          | { error, term() }.

-type scheme_val_fun() :: fun((binary() | sip) -> boolean()).
-type reply_or_error() :: { reply, ersip_sipmsg:sipmsg() }
                        | { error, term() }.
-type proxy_params() ::
        #{ %% If 'allow' option is set then proxy is restricted to pass
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
           supported => ersip_hdr_opttag_list:option_tag_list()

           %% Todo: realm and proxy-authorization
         }.

%%%===================================================================
%%% API
%%%===================================================================

%% 16.3 Request Validation
%%
%% Before an element can proxy a request, it MUST verify the message's
%% validity.  A valid message must pass the following checks:
%%
%%    1. Reasonable Syntax
%%    2. URI scheme
%%    3. Max-Forwards
%%    4. (Optional) Loop Detection
%%    5. Proxy-Require
%%    6. Proxy-Authorization
%%
-spec request_validation(ersip_msg:message(), validate_options()) -> validate_result().
request_validation(RawMessage, Options) ->
    lists:foldl(fun(ValFun, { ok, Message }) ->
                        ValFun(Message, Options);
                   (_, { reply, _ReplyMsg } = Reply) ->
                        Reply;
                   (_, { error, _ } = Error) ->
                        Error
                end,
                { ok, RawMessage },
                [ fun val_reasonable_syntax/2,
                  fun val_uri_scheme/2,
                  fun val_max_forwards/2,
                  fun val_loop_detect/2,
                  fun val_proxy_require/2,
                  fun val_proxy_authorization/2
                ]).

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

%% 1. Reasonable syntax check
%% Function also converts raw message to SIP message with "reasonable
%% parsing".
-spec val_reasonable_syntax(ersip_msg:message(), validate_options()) -> validate_result().
val_reasonable_syntax(RawMessage, Options) ->
    case ersip_sipmsg:parse(RawMessage, [ maxforwards, proxy_require ]) of
        { ok, _SipMsg } = R ->
            R;
        { error, _ } = ParseError ->
            case make_bad_request(RawMessage, Options, ParseError) of
                { ok, Reply } ->
                    { reply, Reply };
                { error, _ } = Error ->
                    Error
            end
    end.

%% 2. URI scheme check
-spec val_uri_scheme(ersip_sipmsg:sipmsg(), validate_options()) -> validate_result().
val_uri_scheme(SipMessage, Options) ->
    Val = maps:get(scheme_val_fun, Options, fun(_) -> true end),
    RURI = ersip_sipmsg:ruri(SipMessage),
    case Val(ersip_uri:scheme(RURI)) of
        true ->
            { ok, SipMessage };
        false ->
            %% If the Request-URI has a URI whose scheme is not
            %% understood by the proxy, the proxy SHOULD reject the
            %% request with a 416 (Unsupported URI Scheme) response.
            make_reply(SipMessage, Options, 416)
    end.

%% 3. Max-Forwards check
-spec val_max_forwards(ersip_sipmsg:sipmsg(), validate_options()) -> validate_result().
val_max_forwards(SipMessage, Options) ->
    case ersip_sipmsg:find(maxforwards, SipMessage) of
        not_found ->
            %% If the request does not contain a Max-Forwards header
            %% field, this check is passed.
            { ok, SipMessage };
        { ok, { maxforwards, Value } } when Value > 0  ->
            %% If the request contains a Max-Forwards header field
            %% with a field value greater than zero, the check is
            %% passed.
            { ok, SipMessage };
        { ok, { maxforwards, 0 } } ->
            %% If the request contains a Max-Forwards header field with a field
            %% value of zero (0), the element MUST NOT forward the request.  If
            %% the request was for OPTIONS, the element MAY act as the final
            %% recipient and respond per Section 11.  Otherwise, the element MUST
            %% return a 483 (Too many hops) response.
            case ersip_sipmsg:method(SipMessage) of
                { method, <<"OPTIONS">> } ->
                    maybe_reply_options(SipMessage, Options);
                _ ->
                    make_reply(SipMessage, Options, 483)
            end
    end.

-spec val_loop_detect(ersip_sipmsg:sipmsg(), validate_options()) -> validate_result().
val_loop_detect(SipMessage, #{ loop_detect := true } = Options) ->
    loop_detect(SipMessage, Options);
val_loop_detect(SipMessage, #{}) ->
    { ok, SipMessage }.

%% 5. Proxy-Require check
%%
%% If the request contains a Proxy-Require header field (Section
%% 20.29) with one or more option-tags this element does not
%% understand, the element MUST return a 420 (Bad Extension)
%% response.  The response MUST include an Unsupported (Section
%% 20.40) header field listing those option-tags the element did not
%% understand.
-spec val_proxy_require(ersip_sipmsg:sipmsg(), validate_options()) -> validate_result().
val_proxy_require(SipMessage, Options) ->
    case ersip_sipmsg:find(proxy_require, SipMessage) of
        not_found ->
            { ok, SipMessage };
        { ok, Required } ->
            case check_supported(Required, Options) of
                all_supported ->
                    { ok, SipMessage };
                Unsupported ->
                    make_bad_extension(SipMessage, Options, Unsupported)
            end
    end.

%% 6. Proxy-Authorization check
%%
%% If an element requires credentials before forwarding a request, the
%% request MUST be inspected as described in Section 22.3.  That
%% section also defines what the element must do if the inspection
%% fails.
-spec val_proxy_authorization(ersip_sipmsg:sipmsg(), validate_options()) -> validate_result().
val_proxy_authorization(SipMessage, Options) ->
    %% TODO: implement it eventually
    { ok, SipMessage }.


-spec make_bad_request(ersip_msg:message(), validate_options(), ParseError) -> Result when
      ParseError :: { error, term() },
      Result     :: { ok, ersip_sipmsg:sipmsg() }
                  | { error, term() }.
make_bad_request(RawMessage, Options, ParseError) ->
    case ersip_sipmsg:parse(RawMessage, [ to, from, callid, cseq ]) of
        { ok, SipMsg } ->
            Reply = ersip_reply:new(400,
                                    [ { reason, ersip_status:bad_request_reason(ParseError) },
                                      { to_tag, maps:get(to_tag, Options) }
                                    ]),
            { ok, ersip_sipmsg:reply(Reply, SipMsg) };
        { error, _ } = Error ->
            Error
    end.


-spec make_reply(ersip_sipmsg:sipmsg(), validate_options(), Code) -> Result when
      Code       :: ersip_status:code(),
      Result     :: { reply, ersip_sipmsg:sipmsg() }
                  | { error, term() }.
make_reply(SipMessage, Options, Code) ->
    case ersip_sipmsg:parse(SipMessage, [ to, from, callid, cseq ]) of
        { ok, SipMessage1 } ->
            Reply = ersip_reply:new(Code,
                                    [ { to_tag, maps:get(to_tag, Options) }
                                    ]),
            { reply, ersip_sipmsg:reply(Reply, SipMessage1) };
        { error, _ } = Error ->
            Error
    end.

-spec make_bad_extension(ersip_sipmsg:sipmsg(), validate_options(), Unsupported) -> reply_or_error() when
      Unsupported :: ersip_hdr_opttag_list:option_tag_list().
make_bad_extension(SipMessage, Options, Unsupported) ->
    case ersip_sipmsg:parse(SipMessage, [ to, from, callid, cseq ]) of
        { ok, SipMsg } ->
            Reply = ersip_reply:new(420,
                                    [ { to_tag, maps:get(to_tag, Options) }
                                    ]),
            Resp0 = ersip_sipmsg:reply(Reply, SipMsg),
            Resp1 = ersip_sipmsg:set(unsupported, Unsupported, Resp0),
            { reply,  Resp1 };
        { error, _ } = Error ->
            Error
    end.


-spec maybe_reply_options(ersip_sipmsg:sipmsg(), validate_options()) -> reply_or_error().
maybe_reply_options(SipMessage, #{ reply_on_options := true, proxy_params := _ } = Options) ->
    make_options_reply(SipMessage, Options);
maybe_reply_options(SipMessage, Options) ->
    make_reply(SipMessage, Options, 483).

-spec make_options_reply(ersip_sipmsg:sipmsg(), validate_options()) -> reply_or_error().
make_options_reply(SipMessage, Options) ->
    Reply = ersip_reply:new(200,
                            [ { to_tag, maps:get(to_tag, Options) }
                            ]),
    %% The response to an OPTIONS is constructed using the standard rules
    %% for a SIP response as discussed in Section 8.2.6.
    Resp200 = ersip_sipmsg:reply(Reply, SipMessage),
    %% If the response to an OPTIONS is generated by a proxy server,
    %% the proxy returns a 200 (OK), listing the capabilities of the
    %% server. The response does not contain a message body.
    %%
    %% Allow, Accept, Accept-Encoding, Accept-Language, and Supported
    %% header fields SHOULD be present in a 200 (OK) response to an
    %% OPTIONS request. If the response is generated by a proxy, the
    %% Allow header...
    Enriched =
        lists:foldl(fun(Fun, Resp) ->
                            Fun(Options, Resp)
                    end,
                    Resp200,
                    [ fun maybe_add_allow/2,
                      fun maybe_add_accept/2,
                      fun maybe_add_accept_encoding/2,
                      fun maybe_add_accept_language/2,
                      fun maybe_add_supported/2
                    ]),
    { reply, Enriched }.

-spec maybe_add_allow(validate_options(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
maybe_add_allow(#{ proxy_params := #{ allow := Allow } }, Resp) ->
    ersip_sipmsg:set(allow, Allow, Resp);
maybe_add_allow(_, Resp) ->
    Resp.

-spec maybe_add_accept(validate_options(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
maybe_add_accept(Options, Resp) ->
    Resp.

-spec maybe_add_accept_encoding(validate_options(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
maybe_add_accept_encoding(Options, Resp) ->
    Resp.

-spec maybe_add_accept_language(validate_options(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
maybe_add_accept_language(Options, Resp) ->
    Resp.

-spec maybe_add_supported(validate_options(), ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
maybe_add_supported(#{ proxy_params := #{ supported := Supported } }, Resp) ->
    ersip_sipmsg:set(supported, Supported, Resp);
maybe_add_supported(_, Resp) ->
    Resp.


-spec loop_detect(ersip_sipmsg:sipmsg(), validate_options()) -> validate_result().
loop_detect(SipMessage, Options) ->
    { ok, SipMessage }.

-spec check_supported(Required, validate_options()) -> all_supported | Unsupported when
      Required    :: ersip_hdr_opttag_list:option_tag_list(),
      Unsupported :: ersip_hdr_opttag_list:option_tag_list().
check_supported(Required, #{ proxy_params := #{ supported := Supported } }) ->
    Intersect = ersip_hdr_opttag_list:intersect(Required, Supported),
    case Intersect =:= Required of
        true ->
            all_supported;
        false ->
            ersip_hdr_opttag_list:subtract(Required, Supported)
    end;
check_supported(Required, _) ->
    Required.
