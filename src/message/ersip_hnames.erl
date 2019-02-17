%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Header names
%%

-module(ersip_hnames).

-export([make_key/1,
         print_form/1,
         known_header_form/1,
         all_known_headers/0
        ]).

-export_type([header_key/0,
              known_header/0,
              name_forms/0]).

-include("ersip_headers.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type header_key() :: {hdr_key, atom() | binary()}.
-type known_header() :: from
                      | to
                      | callid
                      | cseq
                      | maxforwards
                      | topmost_via
                      | content_type
                      | allow
                      | route
                      | record_route
                      | supported
                      | unsupported
                      | require
                      | proxy_require
                      | contact
                      | expires
                      | minexpires
                      | date.
-type name_forms() :: header_key()
                    | known_header()
                    | binary().

%%%===================================================================
%%% API
%%%===================================================================

-spec make_key(name_forms()) -> header_key().
make_key({hdr_key, _} = Key) ->
    Key;
make_key(HeaderName) when is_binary(HeaderName) ->
    case key_shortcut(HeaderName) of
        {ok, Key} -> Key;
        not_found ->
            case compact_header_key_map(HeaderName) of
                {ok, Key} -> Key;
                not_found ->
                    header_key_map(ersip_bin:to_lower(HeaderName))
            end
    end;
make_key(KnownHeader) when is_atom(KnownHeader) ->
    known_header_key_map(KnownHeader).


-spec print_form(name_forms()) -> binary().
print_form(HeaderForm) ->
    print_form_map(make_key(HeaderForm)).

-spec known_header_form(binary() | header_key()) -> {ok, known_header()} | not_found.
known_header_form(HeaderName) when is_binary(HeaderName) ->
    known_header_form(make_key(HeaderName));
known_header_form({hdr_key, _} = HdrKey) ->
    known_header_form_map(HdrKey).

all_known_headers() ->
    [from,
     to,
     callid,
     cseq,
     maxforwards,
     topmost_via,
     content_type,
     allow,
     supported,
     unsupported,
     require,
     proxy_require,
     route,
     record_route,
     contact,
     expires,
     minexpires,
     date,
     www_authenticate,
     authorization,
     proxy_authenticate,
     proxy_authorization,
     subscription_state,
     event
    ].

%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% @doc compact header forms
%% (see https://www.iana.org/assignments/sip-parameters/sip-parameters.xhtml#sip-parameters-2).
-spec compact_header_key_map(binary()) -> {ok, known_header()} | not_found.
%% Compact forms:
compact_header_key_map(<<"a">>) -> {ok, ?ERSIPH_ACCEPT_CONTACT};
compact_header_key_map(<<"u">>) -> {ok, ?ERSIPH_ALLOW_EVENTS};
compact_header_key_map(<<"i">>) -> {ok, ?ERSIPH_CALL_ID};
compact_header_key_map(<<"m">>) -> {ok, ?ERSIPH_CONTACT};
compact_header_key_map(<<"e">>) -> {ok, ?ERSIPH_CONTENT_ENCODING};
compact_header_key_map(<<"l">>) -> {ok, ?ERSIPH_CONTENT_LENGTH};
compact_header_key_map(<<"c">>) -> {ok, ?ERSIPH_CONTENT_TYPE};
compact_header_key_map(<<"o">>) -> {ok, ?ERSIPH_EVENT};
compact_header_key_map(<<"f">>) -> {ok, ?ERSIPH_FROM};
compact_header_key_map(<<"y">>) -> {ok, ?ERSIPH_IDENTITY};
compact_header_key_map(<<"r">>) -> {ok, ?ERSIPH_REFER_TO};
compact_header_key_map(<<"b">>) -> {ok, ?ERSIPH_REFERRED_BY};
compact_header_key_map(<<"j">>) -> {ok, ?ERSIPH_REJECT_CONTACT};
compact_header_key_map(<<"d">>) -> {ok, ?ERSIPH_REQUEST_DISPOSITION};
compact_header_key_map(<<"x">>) -> {ok, ?ERSIPH_SESSION_EXPIRES};
compact_header_key_map(<<"s">>) -> {ok, ?ERSIPH_SUBJECT};
compact_header_key_map(<<"k">>) -> {ok, ?ERSIPH_SUPPORTED};
compact_header_key_map(<<"t">>) -> {ok, ?ERSIPH_TO};
compact_header_key_map(<<"v">>) -> {ok, ?ERSIPH_VIA};
compact_header_key_map(X) when is_binary(X) -> not_found.

-spec header_key_map(binary()) -> header_key().
header_key_map(<<"accept">>)              -> ?ERSIPH_ACCEPT;
header_key_map(<<"accept-contact">>)      -> ?ERSIPH_ACCEPT_CONTACT;
header_key_map(<<"allow">>)               -> ?ERSIPH_ALLOW;
header_key_map(<<"allow-events">>)        -> ?ERSIPH_ALLOW_EVENTS;
header_key_map(<<"authorization">>)       -> ?ERSIPH_AUTHORIZATION;
header_key_map(<<"call-id">>)             -> ?ERSIPH_CALL_ID;
header_key_map(<<"contact">>)             -> ?ERSIPH_CONTACT;
header_key_map(<<"content-encoding">>)    -> ?ERSIPH_CONTENT_ENCODING;
header_key_map(<<"content-type">>)        -> ?ERSIPH_CONTENT_TYPE;
header_key_map(<<"content-length">>)      -> ?ERSIPH_CONTENT_LENGTH;
header_key_map(<<"cseq">>)                -> ?ERSIPH_CSEQ;
header_key_map(<<"date">>)                -> ?ERSIPH_DATE;
header_key_map(<<"event">>)               -> ?ERSIPH_EVENT;
header_key_map(<<"expires">>)             -> ?ERSIPH_EXPIRES;
header_key_map(<<"from">>)                -> ?ERSIPH_FROM;
header_key_map(<<"identity">>)            -> ?ERSIPH_IDENTITY;
header_key_map(<<"max-forwards">>)        -> ?ERSIPH_MAX_FORWARDS;
header_key_map(<<"min-expires">>)         -> ?ERSIPH_MIN_EXPIRES;
header_key_map(<<"proxy-authenticate">>)  -> ?ERSIPH_PROXY_AUTHENTICATE;
header_key_map(<<"proxy-authorization">>) -> ?ERSIPH_PROXY_AUTHORIZATION;
header_key_map(<<"proxy-require">>)       -> ?ERSIPH_PROXY_REQUIRE;
header_key_map(<<"record-route">>)        -> ?ERSIPH_RECORD_ROUTE;
header_key_map(<<"refer-to">>)            -> ?ERSIPH_REFER_TO;
header_key_map(<<"referred-by">>)         -> ?ERSIPH_REFERRED_BY;
header_key_map(<<"reject-contact">>)      -> ?ERSIPH_REJECT_CONTACT;
header_key_map(<<"require">>)             -> ?ERSIPH_REQUIRE;
header_key_map(<<"request-disposition">>) -> ?ERSIPH_REQUEST_DISPOSITION;
header_key_map(<<"route">>)               -> ?ERSIPH_ROUTE;
header_key_map(<<"session-expires">>)     -> ?ERSIPH_SESSION_EXPIRES;
header_key_map(<<"subject">>)             -> ?ERSIPH_SUBJECT;
header_key_map(<<"subscription-state">>)  -> ?ERSIPH_SUBSCRIPTION_STATE;
header_key_map(<<"supported">>)           -> ?ERSIPH_SUPPORTED;
header_key_map(<<"to">>)                  -> ?ERSIPH_TO;
header_key_map(<<"unsupported">>)         -> ?ERSIPH_UNSUPPORTED;
header_key_map(<<"via">>)                 -> ?ERSIPH_VIA;
header_key_map(<<"www-authenticate">>)    -> ?ERSIPH_WWW_AUTHENTICATE;
header_key_map(Name) when is_binary(Name) ->
    case compact_header_key_map(Name) of
        {ok, HdrKey} -> HdrKey;
        not_found -> {hdr_key, Name}
    end.


-spec print_form_map(header_key()) -> binary().
print_form_map(?ERSIPH_ACCEPT)               -> <<"Accept">>;
print_form_map(?ERSIPH_ACCEPT_CONTACT)       -> <<"Accept-Contact">>;
print_form_map(?ERSIPH_ALLOW)                -> <<"Allow">>;
print_form_map(?ERSIPH_ALLOW_EVENTS)         -> <<"Allow-Events">>;
print_form_map(?ERSIPH_AUTHORIZATION)        -> <<"Authorization">>;
print_form_map(?ERSIPH_CALL_ID)              -> <<"Call-Id">>;
print_form_map(?ERSIPH_CONTACT)              -> <<"Contact">>;
print_form_map(?ERSIPH_CONTENT_ENCODING)     -> <<"Content-Encoding">>;
print_form_map(?ERSIPH_CONTENT_TYPE)         -> <<"Content-Type">>;
print_form_map(?ERSIPH_CONTENT_LENGTH)       -> <<"Content-Length">>;
print_form_map(?ERSIPH_CSEQ)                 -> <<"CSeq">>;
print_form_map(?ERSIPH_DATE)                 -> <<"Date">>;
print_form_map(?ERSIPH_EVENT)                -> <<"Event">>;
print_form_map(?ERSIPH_EXPIRES)              -> <<"Expires">>;
print_form_map(?ERSIPH_FROM)                 -> <<"From">>;
print_form_map(?ERSIPH_IDENTITY)             -> <<"Identity">>;
print_form_map(?ERSIPH_MAX_FORWARDS)         -> <<"Max-Forwards">>;
print_form_map(?ERSIPH_MIN_EXPIRES)          -> <<"Min-Expires">>;
print_form_map(?ERSIPH_PROXY_AUTHENTICATE)   -> <<"Proxy-Authenticate">>;
print_form_map(?ERSIPH_PROXY_AUTHORIZATION)  -> <<"Proxy-Authorization">>;
print_form_map(?ERSIPH_PROXY_REQUIRE)        -> <<"Proxy-Require">>;
print_form_map(?ERSIPH_RECORD_ROUTE)         -> <<"Record-Route">>;
print_form_map(?ERSIPH_REFER_TO)             -> <<"Refer-To">>;
print_form_map(?ERSIPH_REFERRED_BY)          -> <<"Referred-By">>;
print_form_map(?ERSIPH_REJECT_CONTACT)       -> <<"Reject-Contact">>;
print_form_map(?ERSIPH_REQUIRE)              -> <<"Require">>;
print_form_map(?ERSIPH_REQUEST_DISPOSITION)  -> <<"Request-Disposition">>;
print_form_map(?ERSIPH_ROUTE)                -> <<"Route">>;
print_form_map(?ERSIPH_SESSION_EXPIRES)      -> <<"Session-Expires">>;
print_form_map(?ERSIPH_SUBJECT)              -> <<"Subject">>;
print_form_map(?ERSIPH_SUBSCRIPTION_STATE)   -> <<"Subscription-State">>;
print_form_map(?ERSIPH_SUPPORTED)            -> <<"Supported">>;
print_form_map(?ERSIPH_TO)                   -> <<"To">>;
print_form_map(?ERSIPH_UNSUPPORTED)          -> <<"Unsupported">>;
print_form_map(?ERSIPH_VIA)                  -> <<"Via">>;
print_form_map(?ERSIPH_WWW_AUTHENTICATE)     -> <<"WWW-Authenticate">>;
print_form_map({hdr_key, Name}) when is_binary(Name) ->
    Name.

-spec known_header_key_map(known_header()) -> header_key().
known_header_key_map(from               ) -> ?ERSIPH_FROM;
known_header_key_map(to                 ) -> ?ERSIPH_TO;
known_header_key_map(cseq               ) -> ?ERSIPH_CSEQ;
known_header_key_map(callid             ) -> ?ERSIPH_CALL_ID;
known_header_key_map(maxforwards        ) -> ?ERSIPH_MAX_FORWARDS;
known_header_key_map(content_type       ) -> ?ERSIPH_CONTENT_TYPE;
known_header_key_map(route              ) -> ?ERSIPH_ROUTE;
known_header_key_map(record_route       ) -> ?ERSIPH_RECORD_ROUTE;
known_header_key_map(allow              ) -> ?ERSIPH_ALLOW;
known_header_key_map(supported          ) -> ?ERSIPH_SUPPORTED;
known_header_key_map(unsupported        ) -> ?ERSIPH_UNSUPPORTED;
known_header_key_map(require            ) -> ?ERSIPH_REQUIRE;
known_header_key_map(proxy_require      ) -> ?ERSIPH_PROXY_REQUIRE;
known_header_key_map(contact            ) -> ?ERSIPH_CONTACT;
known_header_key_map(expires            ) -> ?ERSIPH_EXPIRES;
known_header_key_map(minexpires         ) -> ?ERSIPH_MIN_EXPIRES;
known_header_key_map(date               ) -> ?ERSIPH_DATE;
known_header_key_map(topmost_via        ) -> ?ERSIPH_VIA;
known_header_key_map(www_authenticate   ) -> ?ERSIPH_WWW_AUTHENTICATE;
known_header_key_map(authorization      ) -> ?ERSIPH_AUTHORIZATION;
known_header_key_map(proxy_authenticate ) -> ?ERSIPH_PROXY_AUTHENTICATE;
known_header_key_map(proxy_authorization) -> ?ERSIPH_PROXY_AUTHORIZATION;
known_header_key_map(subscription_state ) -> ?ERSIPH_SUBSCRIPTION_STATE;
known_header_key_map(event              ) -> ?ERSIPH_EVENT.

-spec known_header_form_map(header_key()) -> {ok, known_header()} | not_found.
known_header_form_map(?ERSIPH_FROM)                -> {ok, from          };
known_header_form_map(?ERSIPH_TO)                  -> {ok, to            };
known_header_form_map(?ERSIPH_CSEQ)                -> {ok, cseq          };
known_header_form_map(?ERSIPH_CALL_ID)             -> {ok, callid        };
known_header_form_map(?ERSIPH_MAX_FORWARDS)        -> {ok, maxforwards   };
known_header_form_map(?ERSIPH_CONTENT_TYPE)        -> {ok, content_type  };
known_header_form_map(?ERSIPH_ROUTE)               -> {ok, route         };
known_header_form_map(?ERSIPH_RECORD_ROUTE)        -> {ok, record_route  };
known_header_form_map(?ERSIPH_ALLOW)               -> {ok, allow         };
known_header_form_map(?ERSIPH_SUPPORTED)           -> {ok, supported     };
known_header_form_map(?ERSIPH_UNSUPPORTED)         -> {ok, unsupported   };
known_header_form_map(?ERSIPH_REQUIRE)             -> {ok, require       };
known_header_form_map(?ERSIPH_PROXY_REQUIRE)       -> {ok, proxy_require };
known_header_form_map(?ERSIPH_CONTACT)             -> {ok, contact       };
known_header_form_map(?ERSIPH_EXPIRES)             -> {ok, expires       };
known_header_form_map(?ERSIPH_MIN_EXPIRES)         -> {ok, minexpires    };
known_header_form_map(?ERSIPH_DATE)                -> {ok, date          };
known_header_form_map(?ERSIPH_WWW_AUTHENTICATE)    -> {ok, www_authenticate};
known_header_form_map(?ERSIPH_AUTHORIZATION)       -> {ok, authorization};
known_header_form_map(?ERSIPH_PROXY_AUTHENTICATE)  -> {ok, proxy_authenticate};
known_header_form_map(?ERSIPH_PROXY_AUTHORIZATION) -> {ok, proxy_authorization};
known_header_form_map(?ERSIPH_SUBSCRIPTION_STATE)  -> {ok, subscription_state};
known_header_form_map(?ERSIPH_EVENT)               -> {ok, event};
known_header_form_map({hdr_key, _}) -> not_found.

%% Most common names mapping to keys. This improves performance in
%% most common cases.
-spec key_shortcut(binary()) -> {ok, binary()} | not_found.
key_shortcut(<<"Via">>)                 -> {ok, ?ERSIPH_VIA};
key_shortcut(<<"From">>)                -> {ok, ?ERSIPH_FROM};
key_shortcut(<<"To">>)                  -> {ok, ?ERSIPH_TO};
key_shortcut(<<"CSeq">>)                -> {ok, ?ERSIPH_CSEQ};
key_shortcut(<<"Cseq">>)                -> {ok, ?ERSIPH_CSEQ};
key_shortcut(<<"Call-ID">>)             -> {ok, ?ERSIPH_CALL_ID};
key_shortcut(<<"Call-Id">>)             -> {ok, ?ERSIPH_CALL_ID};
key_shortcut(<<"Max-Forwards">>)        -> {ok, ?ERSIPH_MAX_FORWARDS};
key_shortcut(<<"Contact">>)             -> {ok, ?ERSIPH_CONTACT};
key_shortcut(<<"Expires">>)             -> {ok, ?ERSIPH_EXPIRES};
key_shortcut(<<"Allow">>)               -> {ok, ?ERSIPH_ALLOW};
key_shortcut(<<"Route">>)               -> {ok, ?ERSIPH_ROUTE};
key_shortcut(<<"Record-Route">>)        -> {ok, ?ERSIPH_RECORD_ROUTE};
key_shortcut(<<"Event">>       )        -> {ok, ?ERSIPH_EVENT};
key_shortcut(<<"Content-Type">>)        -> {ok, ?ERSIPH_CONTENT_TYPE};
key_shortcut(<<"Content-Length">>)      -> {ok, ?ERSIPH_CONTENT_LENGTH};
key_shortcut(<<"Supported">>)           -> {ok, ?ERSIPH_SUPPORTED};
key_shortcut(<<"Require">>)             -> {ok, ?ERSIPH_REQUIRE};
key_shortcut(<<"Proxy-Require">>)       -> {ok, ?ERSIPH_PROXY_REQUIRE};
key_shortcut(<<"WWW-Authenticate">>)    -> {ok, ?ERSIPH_WWW_AUTHENTICATE};
key_shortcut(<<"Authorization">>)       -> {ok, ?ERSIPH_AUTHORIZATION};
key_shortcut(<<"Proxy-Authenticate">>)  -> {ok, ?ERSIPH_PROXY_AUTHENTICATE};
key_shortcut(<<"Proxy-Authorization">>) -> {ok, ?ERSIPH_PROXY_AUTHORIZATION};
key_shortcut(<<"Subscription-State">>)  -> {ok, ?ERSIPH_SUBSCRIPTION_STATE};
key_shortcut(<<"P-Asserted-Identity">>) -> {ok, {hdr_key, <<"p-asserted-identity">>}};
key_shortcut(_) -> not_found.

