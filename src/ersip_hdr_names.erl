%%
%% Copyright (c) 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% SIP Header names
%%

-module(ersip_hdr_names).

-export([compact_form/1,
         print_form/1
        ]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc transform header to its compact form
-spec compact_form(HeaderName) -> binary() when
      HeaderName :: {lower, binary()}
                  | binary().
compact_form({lower, HeaderName}) when is_binary(HeaderName) ->
    compact_form_map(HeaderName);
compact_form(HeaderName) when is_binary(HeaderName) ->
    compact_form({lower, ersip_bin:to_lower(HeaderName)}).

-spec print_form(LowerHeaderName :: binary()) -> binary().
print_form(LowerHeaderName) ->
    print_form_map(LowerHeaderName).

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

-spec print_form_map(binary()) -> binary().
print_form_map(<<"from">>)          -> <<"From">>;
print_form_map(<<"to">>  )          -> <<"To">>;
print_form_map(<<"cseq">>)          -> <<"CSeq">>;
print_form_map(<<"call-id">>)       -> <<"Call-Id">>;
print_form_map(<<"max-forwards">>)  -> <<"Max-Forwards">>;
print_form_map(<<"content-type">>)  -> <<"Content-Type">>;
print_form_map(<<"route">>)         -> <<"Route">>;
print_form_map(<<"record-route">>)  -> <<"Record-Route">>;
print_form_map(<<"allow">>)         -> <<"Allow">>;
print_form_map(<<"supported">>)     -> <<"Supported">>;
print_form_map(<<"unsupported">>)   -> <<"Unsupported">>;
print_form_map(<<"require">>)       -> <<"Require">>;
print_form_map(<<"proxy-require">>) -> <<"Proxy-Require">>;
print_form_map(<<"contact">>)       -> <<"Contact">>;
print_form_map(<<"expires">>)       -> <<"Expires">>;
print_form_map(<<"min-expires">>)   -> <<"Min-Expires">>;
print_form_map(Binary) ->
    Binary.
