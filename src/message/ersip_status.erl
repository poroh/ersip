%%
%% Copyright (c) 2017, 2018 Dmitry Poroh
%% All rights reserved.
%% Distributed under the terms of the MIT License. See the LICENSE file.
%%
%% Response status codes
%%

-module(ersip_status).

-export([response_type/1,
         reason_phrase/1,
         bad_request_reason/1,
         unsupported_uri_scheme_reason/1
        ]).
-export_type([response_type/0,
              code/0,
              reason/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-type response_type() :: final | provisional.
-type code() :: 100..699.
-type reason() :: binary().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Convert status code to response type (final or provisional).
-spec response_type(code()) -> response_type().
response_type(StatusCode) when StatusCode >= 200 andalso StatusCode =< 699 ->
    final;
response_type(StatusCode) when StatusCode >= 100 andalso StatusCode =< 199 ->
    provisional.

-spec reason_phrase(code()) -> reason().
reason_phrase(Code) ->
    reason_impl(Code).

%%-spec bad_request_reason({error, any()}) -> reason().
bad_request_reason({error, {header_error, {maxforwards, _}}}) ->
    <<"Invalid max-forwards value">>;
bad_request_reason({error, {header_error, {HeaderName, _}}}) when is_atom(HeaderName) ->
    HeaderNameBin = atom_to_binary(HeaderName, utf8),
    <<"Invalid ", HeaderNameBin/binary," value">>;
bad_request_reason({error, _}) ->
    <<"Bad Request">>.

-spec unsupported_uri_scheme_reason(ersip_uri:scheme()) -> reason().
unsupported_uri_scheme_reason(URIScheme) ->
    SchemeBin = ersip_uri:assemble_scheme(URIScheme),
    <<"URI Scheme ", SchemeBin/binary, " not supported">>.

%%%===================================================================
%%% Internal Implementation
%%%===================================================================
reason_impl(100) -> <<"Trying">>;
reason_impl(200) -> <<"OK">>;
reason_impl(404) -> <<"Not Found">>;
reason_impl(405) -> <<"Method Not Allowed">>;
reason_impl(408) -> <<"Request Timeout">>;
reason_impl(416) -> <<"Unsupported URI Scheme">>;
reason_impl(420) -> <<"Bad Extension">>;
reason_impl(423) -> <<"Interval Too Brief">>;
reason_impl(483) -> <<"Too many hops">>;
reason_impl(_)   -> <<"Unknown Status">>.
