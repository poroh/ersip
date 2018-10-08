%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Header names
%%

-module(ersip_hdr_names).

-export([make_key/1,
         compact_form/1,
         print_form/1,
         known_header_form/1,
         all_known_headers/0
        ]).

-export_type([header_key/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type header_key() :: {hdr_key, binary()}.
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
    {hdr_key, ersip_hdr_names:compact_form(HeaderName)};
make_key(KnownHeader) when is_atom(KnownHeader) ->
    known_header_key_map(KnownHeader).

%% @doc transform header to its compact form
-spec compact_form(HeaderName) -> binary() when
      HeaderName :: {lower, binary()}
                  | binary().
compact_form({lower, HeaderName}) when is_binary(HeaderName) ->
    compact_form_map(HeaderName);
compact_form(HeaderName) when is_binary(HeaderName) ->
    compact_form({lower, ersip_bin:to_lower(HeaderName)}).

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
     date
    ].

%%%===================================================================
%%% Internal implementation
%%%===================================================================

%% @doc compact header forms
%% (see https://www.iana.org/assignments/sip-parameters/sip-parameters.xhtml#sip-parameters-2).
-spec compact_form_map(binary()) -> binary().
compact_form_map(<<"accept-contact">>     ) -> <<"a">>;
compact_form_map(<<"allow-events">>       ) -> <<"u">>;
compact_form_map(<<"call-id">>            ) -> <<"i">>;
compact_form_map(<<"contact">>            ) -> <<"m">>;
compact_form_map(<<"content-encoding">>   ) -> <<"e">>;
compact_form_map(<<"content-length">>     ) -> <<"l">>;
compact_form_map(<<"content-type">>       ) -> <<"c">>;
compact_form_map(<<"event">>              ) -> <<"o">>;
compact_form_map(<<"from">>               ) -> <<"f">>;
compact_form_map(<<"identity">>           ) -> <<"y">>;
compact_form_map(<<"refer-to">>           ) -> <<"r">>;
compact_form_map(<<"referred-by">>        ) -> <<"b">>;
compact_form_map(<<"reject-contact">>     ) -> <<"j">>;
compact_form_map(<<"request-disposition">>) -> <<"d">>;
compact_form_map(<<"session-expires">>    ) -> <<"x">>;
compact_form_map(<<"subject">>            ) -> <<"s">>;
compact_form_map(<<"supported">>          ) -> <<"k">>;
compact_form_map(<<"to">>                 ) -> <<"t">>;
compact_form_map(<<"via">>                ) -> <<"v">>;
compact_form_map(Other) -> Other.

-spec print_form_map(header_key()) -> binary().
print_form_map({hdr_key, <<"f">>})             -> <<"From">>;
print_form_map({hdr_key, <<"t">>})             -> <<"To">>;
print_form_map({hdr_key, <<"cseq">>})          -> <<"CSeq">>;
print_form_map({hdr_key, <<"i">>})             -> <<"Call-Id">>;
print_form_map({hdr_key, <<"v">>})             -> <<"Via">>;
print_form_map({hdr_key, <<"max-forwards">>})  -> <<"Max-Forwards">>;
print_form_map({hdr_key, <<"c">>})             -> <<"Content-Type">>;
print_form_map({hdr_key, <<"route">>})         -> <<"Route">>;
print_form_map({hdr_key, <<"record-route">>})  -> <<"Record-Route">>;
print_form_map({hdr_key, <<"allow">>})         -> <<"Allow">>;
print_form_map({hdr_key, <<"k">>})             -> <<"Supported">>;
print_form_map({hdr_key, <<"unsupported">>})   -> <<"Unsupported">>;
print_form_map({hdr_key, <<"require">>})       -> <<"Require">>;
print_form_map({hdr_key, <<"proxy-require">>}) -> <<"Proxy-Require">>;
print_form_map({hdr_key, <<"m">>})             -> <<"Contact">>;
print_form_map({hdr_key, <<"expires">>})       -> <<"Expires">>;
print_form_map({hdr_key, <<"min-expires">>})   -> <<"Min-Expires">>;
print_form_map({hdr_key, Name}) ->
    Name.

-spec known_header_key_map(header_key()) -> {ok, known_header()} | not_found.
known_header_key_map(from          ) -> {hdr_key, <<"f">>}             ;
known_header_key_map(to            ) -> {hdr_key, <<"t">>}             ;
known_header_key_map(cseq          ) -> {hdr_key, <<"cseq">>}          ;
known_header_key_map(callid        ) -> {hdr_key, <<"i">>}             ;
known_header_key_map(maxforwards   ) -> {hdr_key, <<"max-forwards">>}  ;
known_header_key_map(content_type  ) -> {hdr_key, <<"c">>}             ;
known_header_key_map(route         ) -> {hdr_key, <<"route">>}         ;
known_header_key_map(record_route  ) -> {hdr_key, <<"record-route">>}  ;
known_header_key_map(allow         ) -> {hdr_key, <<"allow">>}         ;
known_header_key_map(supported     ) -> {hdr_key, <<"k">>}             ;
known_header_key_map(unsupported   ) -> {hdr_key, <<"unsupported">>}   ;
known_header_key_map(require       ) -> {hdr_key, <<"require">>}       ;
known_header_key_map(proxy_require ) -> {hdr_key, <<"proxy-require">>} ;
known_header_key_map(contact       ) -> {hdr_key, <<"m">>}             ;
known_header_key_map(expires       ) -> {hdr_key, <<"expires">>}       ;
known_header_key_map(minexpires    ) -> {hdr_key, <<"min-expires">>}   ;
known_header_key_map(date          ) -> {hdr_key, <<"date">>}          ;
known_header_key_map(topmost_via   ) -> {hdr_key, <<"v">>}.

-spec known_header_form_map(header_key()) -> {ok, known_header()} | not_found.
known_header_form_map({hdr_key, <<"f">>})             -> {ok, from          };
known_header_form_map({hdr_key, <<"t">>})             -> {ok, to            };
known_header_form_map({hdr_key, <<"cseq">>})          -> {ok, cseq          };
known_header_form_map({hdr_key, <<"i">>})             -> {ok, callid        };
known_header_form_map({hdr_key, <<"max-forwards">>})  -> {ok, maxforwards   };
known_header_form_map({hdr_key, <<"c">>})             -> {ok, content_type  };
known_header_form_map({hdr_key, <<"route">>})         -> {ok, route         };
known_header_form_map({hdr_key, <<"record-route">>})  -> {ok, record_route  };
known_header_form_map({hdr_key, <<"allow">>})         -> {ok, allow         };
known_header_form_map({hdr_key, <<"k">>})             -> {ok, supported     };
known_header_form_map({hdr_key, <<"unsupported">>})   -> {ok, unsupported   };
known_header_form_map({hdr_key, <<"require">>})       -> {ok, require       };
known_header_form_map({hdr_key, <<"proxy-require">>}) -> {ok, proxy_require };
known_header_form_map({hdr_key, <<"m">>})             -> {ok, contact       };
known_header_form_map({hdr_key, <<"expires">>})       -> {ok, expires       };
known_header_form_map({hdr_key, <<"min-expires">>})   -> {ok, minexpires    };
known_header_form_map({hdr_key, <<"date">>})          -> {ok, date          };
known_header_form_map({hdr_key, _Name}) when is_binary(_Name) -> not_found.
