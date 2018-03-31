%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP proxy common functions
%%

-module(ersip_proxy_common).

-export([ request_validation/2,
          process_route_info/2,
          forward_request/3
        ]).

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
           proxy_params      => proxy_params()
         }.
-type validate_result()  :: { ok, ersip_sipmsg:sipmsg() }
                          | { reply, ersip_sipmsg:sipmsg() }
                          | { error, term() }.

-type scheme_val_fun()   :: fun((binary() | sip) -> boolean()).
-type check_rroute_fun() :: fun((ersip_hdr_route:route()) -> boolean()).
-type reply()          :: { reply, ersip_sipmsg:sipmsg() }.
-type reply_or_error() :: reply()
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
           supported => ersip_hdr_opttag_list:option_tag_list(),

           %% Optional loop detection is performed by proxy.
           %% loop_detect => boolean()

           %% Todo: realm and proxy-authorization

           %% Record-route functions that checks that this proxy is
           %% generated this route/record-route header
           check_rroute_fun => check_rroute_fun(),

           %% Record-route header if proxy need to stay on dialog
           %% messages. This URI need to be resolved to proxy's IP
           %% address. Function check_rroute_fun(record_route_uri)
           %% need to return true for next requests.
           record_route_uri => ersip_uri:uri()
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

%% 16.4 Route Information Preprocessing
-spec process_route_info(ersip_sipmsg:sipmsg(), proxy_params()) ->
                                ersip_sipmsg:sipmsg().
process_route_info(SipMsg, ProxyParams) ->
    lists:foldl(fun(RIFun, Message) ->
                        RIFun(Message, ProxyParams)
                end,
                SipMsg,
                [ fun ri_strict_route/2,
                  fun ri_process_maddr/2,
                  fun ri_maybe_remove_route/2
                ]).


%% 16.6 Forward request to one target
-spec forward_request(Target, ersip_sipmsg:sipmsg(), proxy_params()) -> ersip_sipmsg:sipmsg() when
      Target :: ersip_uri:uri().
forward_request(Target, SipMsg, ProxyParams) ->
    lists:foldl(fun(FWDFun, Message) ->
                        FWDFun(Target, Message, ProxyParams)
                end,
                SipMsg,
                [ fun fwd_set_ruri/3,
                  fun fwd_max_forwards/3,
                  fun fwd_record_route/3,
                  fun fwd_check_record_route/3
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
val_loop_detect(SipMessage, Options) ->
    %% TODO: implement it eventually
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


-spec maybe_reply_options(ersip_sipmsg:sipmsg(), validate_options()) -> reply().
maybe_reply_options(SipMessage, #{ reply_on_options := true, proxy_params := _ } = Options) ->
    make_options_reply(SipMessage, Options);
maybe_reply_options(SipMessage, Options) ->
    make_reply(SipMessage, Options, 483).

-spec make_options_reply(ersip_sipmsg:sipmsg(), validate_options()) -> reply().
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

%% Strict routing handling in according to 16.4
%%
%% The proxy MUST inspect the Request-URI of the request.  If the
%% Request-URI of the request contains a value this proxy previously
%% placed into a Record-Route header field (see Section 16.6 item 4),
%% the proxy MUST replace the Request-URI in the request with the last
%% value from the Route header field, and remove that value from the
%% Route header field.  The proxy MUST then proceed as if it received
%% this modified request.
-spec ri_strict_route(ersip_sipmsg:sipmsg(), proxy_params()) -> ersip_sipmsg:sipmsg().
ri_strict_route(SipMsg, #{ check_rroute_fun := CheckRRFun }) when is_function(CheckRRFun) ->
    RURI  = ersip_sipmsg:ruri(SipMsg),
    case CheckRRFun(RURI) of
        true ->
            case ersip_sipmsg:find(route, SipMsg) of
                { ok, RouteSet } ->
                    LastRoute = ersip_route_set:last(RouteSet),
                    SipMsg0 = ersip_sipmsg:set_ruri(ersip_hdr_route:uri(LastRoute), SipMsg),
                    remove_last_route(SipMsg0);
                _ ->
                    SipMsg
            end;
        false ->
            %% This is loose routing
            SipMsg
    end;
ri_strict_route(SipMsg, #{}) ->
    %% We cannot detect if route is from record route
    SipMsg.

%% maddr processing in according to 16.4
%%
%% If the Request-URI contains a maddr parameter, the proxy MUST check
%% to see if its value is in the set of addresses or domains the proxy
%% is configured to be responsible for.  If the Request-URI has a
%% maddr parameter with a value the proxy is responsible for, and the
%% request was received using the port and transport indicated
%% (explicitly or by default) in the Request-URI, the proxy MUST strip
%% the maddr and any non-default port or transport parameter and
%% continue processing as if those values had not been present in the
%% request.
-spec ri_process_maddr(ersip_sipmsg:sipmsg(), proxy_params()) -> ersip_sipmsg:sipmsg().
ri_process_maddr(SipMsg, ProxyParams) ->
    %% TODO: maddr: implement it eventually
    SipMsg.

%% Remove route in according to 16.4
%%
%% If the first value in the Route header field indicates this proxy,
%% the proxy MUST remove that value from the request.
-spec ri_maybe_remove_route(ersip_sipmsg:sipmsg(), proxy_params()) -> ersip_sipmsg:sipmsg().
ri_maybe_remove_route(SipMsg, #{ check_rroute_fun := CheckRRFun }) ->
    case ersip_sipmsg:find(route, SipMsg) of
        { ok, RouteSet } ->
            FirstRoute = ersip_route_set:first(RouteSet),
            URI = ersip_hdr_route:uri(FirstRoute),
            case CheckRRFun(URI) of
                false ->
                    SipMsg;
                true ->
                    RouteSet1 = ersip_route_set:remove_first(RouteSet),
                    ersip_sipmsg:set(route, RouteSet1, SipMsg)
            end;
        _ ->
            SipMsg
    end;
ri_maybe_remove_route(SipMsg, _ProxyParams) ->
    SipMsg.

-spec remove_last_route(ersip_sipmsg:sipmsg()) -> ersip_sipmsg:sipmsg().
remove_last_route(SipMsg) ->
    RouteSet0 = ersip_sipmsg:get(route, SipMsg),
    RouteSet1 = ersip_route_set:remove_last(RouteSet0),
    ersip_sipmsg:set(route, RouteSet1, SipMsg).

%% 16.6 Request Forwarding
%%
%% 2. Update the Request-URI
-spec fwd_set_ruri(ersip_uri:uri(), ersip_sipmsg:sipmsg(), proxy_params()) -> ersip_sipmsg:sipmsg().
fwd_set_ruri(TargetURI, SipMsg, _ProxyParams) ->
    %% The Request-URI in the copy's start line MUST be replaced with
    %% the URI for this target.  If the URI contains any parameters
    %% not allowed in a Request-URI, they MUST be removed.
    CleanURI = ersip_uri:clear_not_allowed_parts(ruri, TargetURI),
    ersip_sipmsg:set_ruri(CleanURI, SipMsg).


%% 16.6 Request Forwarding
%%
%% 3. Max-Forwards
-spec fwd_max_forwards(ersip_uri:uri(), ersip_sipmsg:sipmsg(), proxy_params()) -> ersip_sipmsg:sipmsg().
fwd_max_forwards(_TargetURI, SipMsg, _ProxyParams) ->
    case ersip_sipmsg:find(maxforwards, SipMsg) of
        { ok, MaxForwards } ->
            %% If the copy contains a Max-Forwards header field, the proxy
            %% MUST decrement its value by one (1).
            ersip_sipmsg:set(maxforwards, ersip_hdr_maxforwards:dec(MaxForwards), SipMsg);
        not_found ->
            %% If the copy does not contain a Max-Forwards header field, the
            %% proxy MUST add one with a field value, which SHOULD be 70.
            ersip_sipmsg:set(maxforwards, ersip_hdr_maxforwards:make(70), SipMsg);
        { error, _ } = Error ->
            error(Error)
    end.

%% 16.6 Request Forwarding
%%
%% 4. Record-Route
-spec fwd_record_route(ersip_uri:uri(), ersip_sipmsg:sipmsg(), proxy_params()) -> ersip_sipmsg:sipmsg().
fwd_record_route(_TargetURI, SipMsg, #{ record_route_uri := RR0 }) ->
    RR1 = ersip_uri:clear_not_allowed_parts(record_route, RR0),
    %% The URI placed in the Record-Route header field value MUST be a
    %% SIP or SIPS URI. This URI MUST contain an lr parameter (see
    %% Section 19.1.1).
    RR2 = ersip_uri:set_param(lr, true, RR1),
    RRRoute = ersip_hdr_route:make_route(RR2),
    RRSet0 =
        case ersip_sipmsg:find(record_route, SipMsg) of
            { ok, ExistRRSet } ->
                ExistRRSet;
            not_found ->
                ersip_route_set:new()
        end,
    RRSet1 = ersip_route_set:add_first(RRRoute, RRSet0),
    ersip_sipmsg:set(record_route, RRSet1, SipMsg);
fwd_record_route(_TargetURI, SipMsg, _ProxyParams) ->
    SipMsg.

%% Check record route in accoring to the clause (RFC 3261 16.6 bullet 4):
%%
-spec fwd_check_record_route(ersip_uri:uri(), ersip_sipmsg:sipmsg(), proxy_params()) -> ersip_sipmsg:sipmsg().
fwd_check_record_route(_TargetURI, SipMsg, #{ record_route := RR }) ->
    %% If the Request-URI contains a SIPS URI, or the topmost Route
    %% header field value (after the post processing of bullet 6)
    %% contains a SIPS URI, the URI placed into the Record-Route header
    %% field MUST be a SIPS URI.
    case nexthop_scheme_is({ scheme, sips }, SipMsg) of
        true ->
            case { scheme, sips } == ersip_uri:scheme(RR) of
                true ->
                    ok;
                false ->
                    error({ error, { record_route_must_be_sips } })
            end;
        false ->
            ok
    end,
    SipMsg;
fwd_check_record_route(_TargetURI, SipMsg, _ProxyParams) ->
    %% Note: No record-route is defined by proxy in this clause.
    %%
    %% Furthermore, if the request was not received over TLS, the
    %% proxy MUST insert a Record-Route header field.  In a similar
    %% fashion, a proxy that receives a request over TLS, but
    %% generates a request without a SIPS URI in the Request-URI or
    %% topmost Route header field value (after the post processing of
    %% bullet 6), MUST insert a Record-Route header field that is not
    %% a SIPS URI.
    case ersip_sipmsg:source(SipMsg) of
        undefined ->
            ok;
        Source ->
            IsTLS = ersip_source:is_tls(Source),
            case (not IsTLS) andalso nexthop_scheme_is({ scheme, sips }, SipMsg) of
                true ->
                    error({ error, { record_route_required, sips_transform } });
                false ->
                    ok
            end,
            case IsTLS andalso nexthop_scheme_is({ scheme, sip }, SipMsg) of
                true ->
                    error({ error, { record_route_required, sip_transform } });
                false ->
                    ok
            end
    end,
    SipMsg.

-spec nexthop_scheme_is(ersip_uri:scheme(), ersip_sipmsg:sipmsg()) -> boolean().
nexthop_scheme_is(Scheme, SipMsg) ->
    RURISchemeMatch = Scheme == ersip_uri:scheme(ersip_sipmsg:ruri(SipMsg)),
    TopRouteScemeMatch =
        case ersip_sipmsg:find(route, SipMsg) of
            { ok, RouteSet } ->
                TopRoute = ersip_route_set:first(RouteSet),
                Scheme == ersip_uri:scheme(ersip_hdr_route:uri(TopRoute));
            not_found ->
                false
        end,
    RURISchemeMatch orelse TopRouteScemeMatch.
