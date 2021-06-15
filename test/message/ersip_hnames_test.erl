%%%
%%% Copyright (c) 2018, 2021 Dmitry Poroh
%%% All rights reserved.
%%% Distributed under the terms of the MIT License. See the LICENSE file.
%%%
%%% Header names test
%%%


-module(ersip_hnames_test).

-include_lib("eunit/include/eunit.hrl").
-include("ersip_headers.hrl").

%%===================================================================
%% Cases
%%===================================================================

compact_form_test() ->
    IANAPageCopy =
        <<"Accept-Contact	a	[RFC3841]"
          "|Referred-By	b	[RFC3892]"
          "|Content-Type	c	[RFC3261]"
          "|Request-Disposition	d	[RFC3841]"
          "|Content-Encoding	e	[RFC3261]"
          "|From	f	[RFC3261]"
          "|Call-ID	i	[RFC3261]"
          "|Reject-Contact	j	[RFC3841]"
          "|Supported	k	[RFC3261]"
          "|Content-Length	l	[RFC3261]"
          "|Contact	m	[RFC3261]"
          "|Event	o	[RFC6665][RFC6446]"
          "|Refer-To	r	[RFC3515]"
          "|Subject	s	[RFC3261]"
          "|To	t	[RFC3261]"
          "|Allow-Events	u	[RFC6665]"
          "|Via	v	[RFC3261][RFC7118]"
          "|Session-Expires	x	[RFC4028]"
          "|Identity	y	[RFC8224]">>,
    HeadersL = binary:split(IANAPageCopy, <<"|">>, [global]),
    HeadersF =
        lists:map(
          fun(Binary) ->
                  [FieldName, CompactForm, _] =
                      binary:split(Binary, <<"\t">>, [global]),
                  {FieldName, CompactForm}
          end,
          HeadersL),
    lists:foreach(
      fun({FieldName, CompactForm}) ->
              ?assertEqual(ersip_hnames:make_key(CompactForm), ersip_hnames:make_key(FieldName))
      end,
      HeadersF).


print_form_test() ->
    PrintForms =
        [<<"From">>,
         <<"To">>,
         <<"CSeq">>,
         <<"Call-Id">>,
         <<"Max-Forwards">>,
         <<"Content-Type">>,
         <<"Route">>,
         <<"Record-Route">>,
         <<"Allow">>,
         <<"Supported">>,
         <<"Unsupported">>,
         <<"Require">>,
         <<"Proxy-Require">>,
         <<"p-custom-header">>,
         <<"Accept">>,
         <<"Accept-Contact">>,
         <<"Allow-Events">>,
         <<"Content-Encoding">>,
         <<"Identity">>,
         <<"Referred-By">>,
         <<"Reject-Contact">>,
         <<"Request-Disposition">>,
         <<"Session-Expires">>,
         <<"Subject">>
        ],
    [test_print_form(Name) || Name <- PrintForms].


known_headers_from_binary_test() ->
    KnownHeaders = [{from,                <<"From">>},
                    {cseq,                <<"CSeq">>},
                    {callid,              <<"Call-ID">>},
                    {maxforwards,         <<"Max-Forwards">>},
                    {content_type,        <<"Content-Type">>},
                    {route,               <<"Route">>},
                    {record_route,        <<"Record-route">>},
                    {allow,               <<"Allow">>},
                    {supported,           <<"Supported">>},
                    {unsupported,         <<"Unsupported">>},
                    {require,             <<"Require">>},
                    {proxy_require,       <<"Proxy-Require">>},
                    {contact,             <<"Contact">>},
                    {expires,             <<"Expires">>},
                    {minexpires,          <<"Min-Expires">>},
                    {date,                <<"Date">>},
                    {www_authenticate,    <<"WWW-Authenticate">>},
                    {authorization,       <<"Authorization">>},
                    {proxy_authenticate,  <<"Proxy-Authenticate">>},
                    {proxy_authorization, <<"Proxy-Authorization">>},
                    {subscription_state,  <<"Subscription-State">>},
                    {event,               <<"Event">>},
                    {refer_to,            <<"Refer-To">>},
                    {replaces,            <<"Replaces">>},
                    {rseq,                <<"RSeq">>},
                    {rack,                <<"RAck">>}],
    [?assertMatch({ok, KnownHeader}, ersip_hnames:known_header_form(Name))
     || {KnownHeader, Name} <- KnownHeaders],
    ok.

make_key_test() ->
    KeysMap = [{ersip_hnames:make_key(<<"p-asserted-identity">>), <<"P-Asserted-Identity">>},
               {?ERSIPH_SUBSCRIPTION_STATE,  <<"subscription-state">>},
               {?ERSIPH_AUTHORIZATION,       <<"aUthorization">>},
               {?ERSIPH_CONTENT_LENGTH,      <<"cOntent-Length">>},
               {?ERSIPH_EVENT,               <<"event">>},
               {?ERSIPH_EXPIRES,             <<"eXpireS">>},
               {?ERSIPH_PROXY_AUTHENTICATE,  <<"proxy-Authenticate">>},
               {?ERSIPH_PROXY_AUTHORIZATION, <<"proxy-Authorization">>},
               {?ERSIPH_REPLACES,            <<"replaceS">>},
               {?ERSIPH_RSEQ,                <<"rSeq">>},
               {?ERSIPH_RACK,                <<"raCk">>}],
    [?assertEqual(Key, ersip_hnames:make_key(Name)) || {Key, Name} <- KeysMap],
    ok.


%%===================================================================
%% Helpers
%%===================================================================

test_print_form(Bin) ->
    ?assertEqual(Bin, ersip_hnames:print_form(ersip_bin:to_lower(Bin))).
