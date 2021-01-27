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
bad_request_reason({error, {header_error, {HeaderName, _}}}) when is_atom(HeaderName) ->
    HeaderNameBin = ersip_hnames:print_form(HeaderName),
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
reason_impl(180) -> <<"Ringing">>;
reason_impl(182) -> <<"Queued">>;
reason_impl(183) -> <<"Session Progress">>;
reason_impl(200) -> <<"OK">>;
reason_impl(202) -> <<"Accepted">>;
reason_impl(300) -> <<"Multiple Choices">>;
reason_impl(301) -> <<"Moved Permanently">>;
reason_impl(302) -> <<"Moved Temporarily">>;
reason_impl(305) -> <<"Use Proxy">>;
reason_impl(380) -> <<"Alternative Service">>;
reason_impl(400) -> <<"Bad Request">>;
reason_impl(401) -> <<"Unauthorized">>;
reason_impl(402) -> <<"Payment Required">>;
reason_impl(403) -> <<"Forbidden">>;
reason_impl(404) -> <<"Not Found">>;
reason_impl(405) -> <<"Method Not Allowed">>;
reason_impl(406) -> <<"Not Acceptable">>;
reason_impl(407) -> <<"Proxy Authentication Required">>;
reason_impl(408) -> <<"Request Timeout">>;
reason_impl(410) -> <<"Gone">>;
reason_impl(413) -> <<"Request Entity Too Large">>;
reason_impl(414) -> <<"Request-URI Too Long">>;
reason_impl(415) -> <<"Unsupported Media Type">>;
reason_impl(416) -> <<"Unsupported URI Scheme">>;
reason_impl(420) -> <<"Bad Extension">>;
reason_impl(421) -> <<"Extension Required">>;
reason_impl(423) -> <<"Interval Too Brief">>;
reason_impl(480) -> <<"Temporarily Unavailable">>;
reason_impl(481) -> <<"Call/Transaction Does Not Exist">>;
reason_impl(482) -> <<"Loop detected">>;
reason_impl(483) -> <<"Too many hops">>;
reason_impl(484) -> <<"Address Incomplete">>;
reason_impl(485) -> <<"Ambiguous">>;
reason_impl(486) -> <<"Busy Here">>;
reason_impl(487) -> <<"Request Terminated">>;
reason_impl(488) -> <<"Not Acceptable Here">>;
reason_impl(491) -> <<"Request Pending">>;
reason_impl(493) -> <<"Undecipherable">>;
reason_impl(500) -> <<"Internal Server Error">>;
reason_impl(501) -> <<"Not Implemented">>;
reason_impl(502) -> <<"Bad Gateway">>;
reason_impl(503) -> <<"Service Unavailable">>;
reason_impl(504) -> <<"Server Time-out">>;
reason_impl(505) -> <<"Version Not Supported">>;
reason_impl(513) -> <<"Message Too Large">>;
reason_impl(600) -> <<"Busy Everywhere">>;
reason_impl(603) -> <<"Decline">>;
reason_impl(604) -> <<"Does Not Exist Anywhere">>;
reason_impl(606) -> <<"Not Acceptable">>;
reason_impl(_)   -> <<"Unknown Status">>.
