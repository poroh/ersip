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

-type validate_options() :: #{ to_tag         := ersip_hdr_fromto:tag(),
                               scheme_val_fun => scheme_val_fun()
                             }.
-type validate_result()  :: { ok, ersip_sipmsg:sipmsg() }
                          | { reply, ersip_sipmsg:sipmsg() }
                          | { error, term() }.

-type scheme_val_fun() :: fun((binary() | sip) -> boolean()).

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
                  fun val_uri_scheme/2
                ]).

%%%===================================================================
%%% Internal Implementation
%%%===================================================================

%% @doc function converts raw message to SIP message with "reasonable
%% parsing".
-spec val_reasonable_syntax(ersip_msg:message(), validate_options()) -> validate_result().
val_reasonable_syntax(RawMessage, Options) ->
    case ersip_sipmsg:parse(RawMessage, [ maxforwards ]) of
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
            { reply, make_reply(SipMessage, Options, 416) }
    end.

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
      Result     :: { ok, ersip_sipmsg:sipmsg() }
                  | { error, term() }.
make_reply(SipMessage, Options, Code) ->
    Reply = ersip_reply:new(Code,
                            [ { to_tag, maps:get(to_tag, Options) }
                            ]),
    ersip_sipmsg:reply(Reply, SipMessage).

